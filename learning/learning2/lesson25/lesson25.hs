{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

sampleString :: String
sampleString = BC.unpack sampleBytes

toInt :: BC.ByteString -> Int
toInt str = read (BC.unpack str) :: Int
-- read . BC.unpack でもOK

bcInt :: BC.ByteString
bcInt = "6"

bcHoge :: BC.ByteString
bcHoge = "ほげほげ"

hogeText :: T.Text
hogeText = "ほげほげ"

hogeSafe :: B.ByteString
hogeSafe = E.encodeUtf8 hogeText