module Data.Smashy.Types where

import Control.Concurrent.STM                       (TVar, newTVarIO)
import Control.Concurrent.STM.TQueue                (TQueue, newTQueueIO)
import Data.Hashable                                (Hashable, hashWithSalt)
import Data.Vector.Storable                         (Vector)
import Data.Vector.Storable.MMap
import Data.Vector.Storable.Mutable     as VM       (IOVector, new)
import qualified STMContainers.Set      as S        (Set, newIO)
import Data.Word (Word8, Word32)

numBuckets :: Int
numBuckets = 200000

bucketSize :: Int
bucketSize = 1024

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
    let htSize = 200000
        buckListSize = numBuckets * bucketSize
    resz <- newTVarIO False
    hts <- newTVarIO htSize
    ht <- VM.new htSize
    --ht <- unsafeMMapMVector "./hashtable" ReadWriteEx (Just (0, htSize)) 
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
