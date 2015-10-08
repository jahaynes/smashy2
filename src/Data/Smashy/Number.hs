{-# LANGUAGE BangPatterns #-}

module Data.Smashy.Number where

import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import qualified Data.Vector.Storable           as VS
import qualified Data.Vector.Storable.Mutable   as VM
import Data.Word (Word8)
import Control.Monad.ST.Strict                          (runST)

newtype Number = Number (VS.Vector Word8) deriving Show

fromInt :: Int -> Number
fromInt n
    | n < 0 = error "Passed negative"
    | n < 128 = Number . VS.singleton . fromIntegral $ n
    | otherwise = Number $ runST $ do
        v <- VM.unsafeNew 9
        VM.unsafeWrite v 0 255
        go v n 1
    where
    go v _ 9 = VS.unsafeFreeze v
    go v x i = do
        VM.unsafeWrite v i (fromIntegral $ 255 .&. x)
        go v (unsafeShiftR x 8) (i+1)

toInt :: Number -> Int
toInt (Number v)
    | VS.length v == 1 = fromIntegral . VS.head $ v
    | otherwise = go 0 0 1
    where
    go !x _ 9 = x
    go  x o i = go (x .|. (fromIntegral ( VS.unsafeIndex v i) `unsafeShiftL` o)) (o+8) (i+1)
