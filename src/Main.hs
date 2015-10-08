{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Smashy.Buckets
import Data.Smashy.Encoding
import Data.Smashy.Types
import Control.Applicative                              ((<$>))

import qualified Data.ByteString.Char8           as C8

main :: IO ()
main = do

    state <- newState

    keys <- C8.words <$> C8.readFile "/usr/share/dict/words"

    let vals = map (escapeBs . C8.pack . show) ([0..] :: [Int])
        keysAndVals = zip keys vals

    mapM_ (\(k,v) -> insert state k v) keysAndVals

    mapM_ (\k -> do
            Just v <- get state k
            return ()) keys
