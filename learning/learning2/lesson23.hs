-- lesson23
{-# LANGUAGE OverloadedStrings #-} -- 言語拡張: Text型にリテラル値を使用
import qualified Data.Text as T
import Data.Semigroup

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

myWord :: T.Text
myWord = "dog"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"
-- T.lines sampleInput でリストに分割
-- T.unlines で戻す

someText :: T.Text
someText = "Some\ntext for\tyou"
-- T.words someText でリストに分割
-- T.unwords で戻す

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"
-- T.splitOn breakText exampleText で任意の文字列で分割(この場合"simple")
-- T.intercalate breakText (T.splitOn breakText exampleText) で戻す

-- 文字列の連結は++を使えない。mconcatか<>を使用する
combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

-- quick check 23-3
myLines :: T.Text -> [T.Text]
myLines str = T.splitOn "\n" str

myUnlines :: [T.Text] -> T.Text
myUnlines list = T.intercalate "\n" list