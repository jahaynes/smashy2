module Data.Smashy.Buckets where

import Data.Smashy.Encoding
import Data.Smashy.Hashes
import Data.Smashy.FreeStore
import Data.Smashy.Number
import Data.Smashy.Types

import Control.Monad                                    (when)
import Control.Monad.STM                                (atomically)
import Data.ByteString (ByteString)
import qualified Data.Vector.Storable            as VS
import qualified Data.Vector.Storable.Mutable    as VM
import qualified Data.Vector.Storable.ByteString as VB
import Data.Word                                        (Word8)

data Found = ValueHasSizeAt Int
           | NotFound Int deriving Show

get :: State -> ByteString -> IO (Maybe ByteString)
get state bs =
    withHash state bs $ \hash -> do
        bId <- readHash state hash
        when (bId == 0) (error "Not found")
        let bucket = sliceBucket state bId
        r <- searchIn hash bucket (escapeBs bs)
        case r of
            NotFound _ -> return Nothing
            ValueHasSizeAt szIndex -> Just <$> freezeBucketRegion bucket szIndex
    where
    --freezeBucketRegion :: Bucket -> Position -> IO ByteString
    freezeBucketRegion buck@(Bucket b) p = do
        (sz, p') <- intFrom buck p
        VB.vectorToByteString <$> VS.freeze (VM.unsafeSlice p' sz b)

searchIn :: Hash -> Bucket -> Escaped -> IO Found
searchIn _ b@(Bucket bucket) (Escaped esc) = go (VS.length esc)
    where
    go key_sz = do
        (sz, p) <- intFrom b 0

        checkSize sz p
        where
        {- We check the key's size here -}
        checkSize  0 p = return $ NotFound (p-1) --This -1 is a bit of a hacky rewind
        checkSize sz p
            | sz == key_sz = checkKey True 0 p
            | otherwise = checkKey False sz (p + sz)

            where
            {- We check the key's string here -}
            checkKey matched i j
                | i == sz =
                    if matched
                        then return $ ValueHasSizeAt j
                        else do
                            (sz',p') <- intFrom b j
                            checkSize sz' p'
                | otherwise = do
                    v <- VM.unsafeRead bucket j
                    if v == VS.unsafeIndex esc i
                        then checkKey True (i+1) (j+1)
                        else do
                            let skippable = sz - i
                            checkKey False (i+skippable) (j+skippable)

insert :: State -> ByteString -> Val -> IO ()
insert state bs val =

    withHash state bs $ \hash -> do

        putStrLn $ "Got Hash " ++ show hash

        hv <- readHash state hash

        putStrLn $ "hv was " ++ show hv

        bId <-
            if hv == 0
                then do
                    newBucketId <- atomically $ takeNextFree state
                    setHash state hash newBucketId
                    return newBucketId
                else
                    return hv

        putStrLn $ "bId was " ++ show bId

        let bucket = sliceBucket state bId

        r <- searchIn hash bucket (escapeBs bs)
        case r of
            ValueHasSizeAt _ -> error "key was already present!"
            NotFound p -> put (escapeBs bs) bucket p

    where
    put :: Key -> Bucket -> Position -> IO ()
    put key (Bucket bucket) = writeBucketAt (toBucketDatum key val)

        where
        writeBucketAt :: BucketData -> Int -> IO ()
        writeBucketAt (BucketData bucketData) offset =
            -- (+1 for 0, +4 in case we want to store 32 bits of 'next bucket id'
            if offset + VS.length bucketData + 5 > bucketSize
                then error "Too big!"
                else copy bucket bucketData 0 (VS.length bucketData)
            where
            {- Can we use an existing function for this -}
            copy :: VM.IOVector Word8 -> VS.Vector Word8 -> Int -> Int -> IO ()
            copy target src i j
                | i == j = VM.unsafeWrite target (offset+i) 0
                | otherwise = do
                    VM.unsafeWrite target (offset+i) (VS.unsafeIndex src i)
                    copy target src (i+1) j

sliceBucket :: State -> BucketId -> Bucket
sliceBucket state bId = Bucket $ VM.slice bucketStart bucketSize (bucketList state)
    where bucketStart = bId * bucketSize

toBucketDatum :: Key -> Val -> BucketData
toBucketDatum (Escaped k) (Escaped v) =
    let Number sz_k = fromInt (VS.length k)
        Number sz_v = fromInt (VS.length v)
    in BucketData $ VS.concat [sz_k, k, sz_v, v]

{- Reads a Number at a particular (Bucket,Position)
   also returning the offset immediately after the
   Number -}
{- Is this a candidate for unboxed magic hash? -}   
intFrom :: Bucket -> Position -> IO (Int, Position)
intFrom (Bucket bucket) p = do
    i <- VM.unsafeRead bucket p
    if i < 128
        then return (toInt . Number . VS.singleton $ i, p+1)
        else do
            slice <- VS.freeze $ VM.unsafeSlice (p+1) 8 bucket --This slice might cross buckets :(
            return (toInt . Number $ slice, p+8)
