{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

charCount :: T.Text -> Int
charCount str = B.length (E.encodeUtf8 str)

bcInt :: T.Text
bcInt = "あいうえお"

-- これだとUnicodeを読み込めない
main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    file <- TIO.readFile fileName
    let l = T.length file
    -- let c = charCount file
    print l 
    -- print c
    print "all done"