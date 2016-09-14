{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Smashy.Buckets
import Data.Smashy.Encoding
import Data.Smashy.Types
import qualified Data.Vector.Storable                         as V
import Control.Monad (forM_)
import qualified Data.ByteString.Char8           as C8

main :: IO ()
main = do

    state <- newState

    --keys <- take 2 . C8.words <$> C8.readFile "/usr/share/dict/words"

    --let vals = map (escapeBs . C8.pack . show) ([0..] :: [Int])
    --   keysAndVals = zip keys vals

    --mapM_ (\(k,v) -> insert state k v) keysAndVals

    
    insert state "hello" (escapeBs "A")    
    insert state "goodbye" (escapeBs "B")
    insert state "three" (escapeBs "C")
    insert state "four" (escapeBs "D")
    insert state "hello1" (escapeBs "A")    
    insert state "goodbye1" (escapeBs "B")
    insert state "three1" (escapeBs "C")
    insert state "four1" (escapeBs "D")
    
    
    get state "goodbye1" >>= print
    
    --dump state
    
    
    
dump state = do
    putStrLn "----------------------------------------"
    forM_ [1..numBuckets-1] $ \i -> do

        let (Bucket bm) = sliceBucket state i
    
        b <- V.freeze bm
    
        putStrLn $ "Bucket " ++ show i
        print b
        putStrLn ""
    
    --mapM_ (\k -> do
    --        Just v <- get state k
    --        print v) keys
