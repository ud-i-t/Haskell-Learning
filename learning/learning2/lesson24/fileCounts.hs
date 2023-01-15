-- lesson24
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TI

-- 24.4
-- Text型は正格なデータ型なので遅延評価に起因する問題を回避できる
getCounts :: T.Text -> (Int,Int,Int)
getCounts input = (charCount,wordCount,lineCount)
    where charCount = T.length input
          wordCount = (length . T.words) input
          lineCount = (length . T.lines) input

countsText :: (Int,Int,Int) -> T.Text
countsText (cc,wc,lc) = T.pack(unwords ["chars: ", show cc
                                          , " words: "
                                          , show wc
                                          , " lines: "
                                          , show lc])

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    input <- TI.readFile fileName
    let summary = (countsText . getCounts) input
    TI.appendFile "stats.dat" (mconcat [(T.pack fileName), " ", summary, "\n"])
    TI.putStrLn summary