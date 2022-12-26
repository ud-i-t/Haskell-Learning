-- lesson23
{-# LANGUAGE OverloadedStrings #-} -- 言語拡張: Text型にリテラル値を使用
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- 23-3
helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement