module Data.Smashy.FreeStore where

import Data.Smashy.Types

import Control.Concurrent.STM                           (readTVar, modifyTVar')
import Control.Concurrent.STM.TQueue                    (tryReadTQueue, writeTQueue)
import Control.Monad.STM                                (STM)

takeNextFree :: State -> STM BucketId
takeNextFree state = do    
    mv <- tryReadTQueue (freeStore state)
    case mv of
        Just v -> return v
        Nothing -> do
            nf <- readTVar (nextFreeBucket state)
            modifyTVar' (nextFreeBucket state) (+1)
            return nf

{- Here we might optionally want to try to decrement
the nextFreeBucket, but for now just add to free queue 

perhaps a heap might do the trick

-}
releaseBucketId :: State -> BucketId -> STM ()
releaseBucketId state = writeTQueue (freeStore state)