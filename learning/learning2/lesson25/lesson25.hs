{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

sampleString :: String
sampleString = BC.unpack sampleBytes

toInt :: BC.ByteString -> Int
toInt str = read (BC.unpack str) :: Int
-- read . BC.unpack でもOK

bcInt :: BC.ByteString
bcInt = "6"
