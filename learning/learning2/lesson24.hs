-- lesson24
{-# LANGUAGE OverloadedStrings #-} -- 言語拡張: Text型にリテラル値を使用
import System.IO

-- 24.1
main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  secondLine <- hGetLine helloFile
  putStrLn secondLine
  hClose helloFile

  goodbyFile <- openFile "goodby.txt" WriteMode
  hPutStrLn goodbyFile secondLine
  hClose goodbyFile

  putStrLn "done!"
