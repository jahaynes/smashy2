{-# LANGUAGE BangPatterns #-}

module Data.Smashy.Encoding where

import Data.Smashy.Types

import Data.ByteString (ByteString)
import Control.Monad.ST.Strict                          (runST, ST)
import qualified Data.Vector.Storable            as VS
import qualified Data.Vector.Storable.Mutable    as VM
import qualified Data.Vector.Storable.ByteString as VB
import Data.Word (Word8)

escapeBs :: ByteString -> Escaped
escapeBs = escape . VB.byteStringToVector

unescapeBs :: Escaped -> ByteString
unescapeBs = VB.vectorToByteString . unescape

escape :: VS.Vector Word8 -> Escaped
escape vec =
    let size = VS.length vec
        newSize = size + countEscapable size
    in
    if size == newSize
        then Escaped vec
        else Escaped (runST $ VM.unsafeNew newSize >>= \mvec -> esc mvec 0 0 size)
    where
    esc :: VM.MVector s Word8 -> Int -> Int -> Int -> ST s (VS.Vector Word8)
    esc mvec i j size
        | i == size = VS.unsafeFreeze mvec
        | otherwise =
            let x = VS.unsafeIndex vec i
            in
            if x == 10 || x == 92
                then do
                    VM.unsafeWrite mvec j 92
                    VM.unsafeWrite mvec (j + 1) x
                    esc mvec (i + 1) (j + 2) size
                else do
                    VM.unsafeWrite mvec j x
                    esc mvec (i + 1) (j + 1) size
    countEscapable :: Int -> Int
    countEscapable size = go 0 0
        where
        go !acc i
            | i == size = acc
            | otherwise =
                let x = VS.unsafeIndex vec i
                in
                if x == 10 || x == 92
                    then go (acc+1) (i+1)
                    else go acc (i+1)

unescape :: Escaped -> VS.Vector Word8
unescape (Escaped vec) =
    let size = VS.length vec
        origSize = size - countEscaped size
    in
    if size == origSize
        then vec
        else runST $ VM.unsafeNew origSize >>= \mvec -> unesc mvec 0 0 size       
    where
    unesc :: VM.MVector s Word8 -> Int -> Int -> Int -> ST s (VS.Vector Word8)
    unesc mvec i j size
        | i == size = VS.unsafeFreeze mvec
        | otherwise =
            let x = VS.unsafeIndex vec i
            in
            if x == 92
                then do
                    VM.unsafeWrite mvec j (VS.unsafeIndex vec (i+1))
                    unesc mvec (i+2) (j+1) size
                else do
                    VM.unsafeWrite mvec j x
                    unesc mvec (i+1) (j+1) size
    countEscaped :: Int -> Int
    countEscaped size = go 0 0
        where
        go !acc i
            | i == size = acc
            | VS.unsafeIndex vec i == 92 = go (acc+1) (i+2)
            | otherwise = go acc (i+1)