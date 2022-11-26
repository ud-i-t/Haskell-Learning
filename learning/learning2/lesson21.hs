-- lesson21
-- IO

import System.Random

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    -- getLineはI/Oアクション 戻り値の型はIO String
    name <- getLine
    -- helloPersonは関数String -> String だが, do表記のおかげでnameでも通る
    let statement = helloPerson name
    -- putStrLnはI/Oアクション (IO Stringではなく)Stringをとる
    putStrLn statement


minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

main2 :: IO ()
main2 = do
    dieRoll <- randomRIO (minDie,maxDie)
    putStrLn (show dieRoll)
