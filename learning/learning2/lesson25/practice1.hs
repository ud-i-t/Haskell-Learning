{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

charCount :: B.ByteString -> Int
charCount str = T.length (E.decodeUtf8 str)

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    file <- B.readFile fileName
    let b = B.length file
    let l = charCount file
    print b 
    print l
    print "all done"