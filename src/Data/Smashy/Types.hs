module Data.Smashy.Types where

import Control.Concurrent.STM                       (TVar, newTVarIO)
import Control.Concurrent.STM.TQueue                (TQueue, newTQueueIO)
import Data.Hashable                                (Hashable, hashWithSalt)
import Data.Vector.Storable                         (Vector)
--import Data.Vector.Storable.MMap
import Data.Vector.Storable.Mutable     as VM       (IOVector, new)
import qualified STMContainers.Set      as S        (Set, newIO)
import Data.Word (Word8, Word32)

{-Is this numRootBuckets? and should it be guaranteed to be at
least as big as hashtable size?  
every hashentry needs to point to a bucket
-}

numBuckets :: Int
numBuckets = 4

bucketSize :: Int
bucketSize = 32

hashTableStartSize :: Int
hashTableStartSize = 4

newtype Bucket = Bucket (VM.IOVector Word8)

newtype BucketData = BucketData (Vector Word8) deriving Show

data State = State {
        resizing :: TVar Bool,
        hashTableSize :: TVar Int,
        hashTable :: IOVector Word32,
        bucketList :: IOVector Word8,      
        takenHashes :: S.Set Hash,
        nextFreeBucket :: TVar BucketId,
        freeStore :: TQueue BucketId
    }

newState :: IO State
newState = do
    let buckListSize = numBuckets * bucketSize
    resz <- newTVarIO False
    hts <- newTVarIO hashTableStartSize
    ht <- VM.new hashTableStartSize
    --ht <- unsafeMMapMVector "./hashtable" ReadWriteEx (Just (0, hashTableStartSize)) 
    bl <- VM.new buckListSize
    --bl <- unsafeMMapMVector "./bucketList" ReadWriteEx (Just (0, buckListSize)) 
    h <- S.newIO
    nfb <- newTVarIO 1
    fs <- newTQueueIO
    return $ State resz hts ht bl h nfb fs

newtype Escaped = Escaped (Vector Word8) deriving Show

newtype Hash = Hash Int deriving (Eq, Show)

--This could be interesting.  It's its own hash!
instance Hashable Hash where
    hashWithSalt _ (Hash h) = h

type Key = Escaped

type Val = Escaped

type Position = Int

type BucketId = Int
