-- lesson22
import System.Environment
import Control.Monad
import Data.List.Split

sampleData = ['6', '2', '\n', '2', '1', '\n'] 

myLines lines = splitOn "\n" lines

toInts :: String -> [Int]
toInts lines = map read (myLines lines)

main :: IO ()
main = do
    userInput <- getContents
    putStrLn userInput
    --let numbers = toInts userInput
    --print (sum numbers)