module Data.Smashy.Hashes where

import Data.Smashy.Types

import Control.Applicative                              ((<$>))
import Control.Concurrent.STM                           (readTVar)
import Control.Monad                                    (when)
import Control.Monad.STM                                (STM, atomically, retry)
import Control.Exception                                (bracket)
import Data.ByteString                                  (ByteString)
import Data.Digest.XXHash                               (xxHash')

import qualified Data.Vector.Storable.Mutable       as VM
import qualified STMContainers.Set                  as S

withHash :: State -> ByteString -> (Hash -> IO a) -> IO a
withHash state bs =

    bracket
        (atomically takeAndBoundHash)
        (atomically . releaseHash)

    where

    releaseHash :: Hash -> STM ()
    releaseHash hash = S.delete hash (takenHashes state)

    takeAndBoundHash :: STM Hash
    takeAndBoundHash = do
        waitWhileResizing
        size <- readTVar . hashTableSize $ state
        let hash = Hash $ 1 + (fromIntegral (xxHash' bs) `mod` (size-1))
        taken <- S.lookup hash (takenHashes state)
        if taken
            then retry
            else do
                S.insert hash (takenHashes state)
                return hash

        where
        waitWhileResizing :: STM ()
        waitWhileResizing = readTVar (resizing state) >>= \rsz -> when rsz retry

readHash :: State -> Hash -> IO BucketId
readHash state (Hash h) = fromIntegral <$> VM.unsafeRead (hashTable state) h

setHash :: State -> Hash -> BucketId -> IO ()
setHash state (Hash h) hv = VM.unsafeWrite (hashTable state) h (fromIntegral hv)
