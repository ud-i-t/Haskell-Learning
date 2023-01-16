{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
    args <- getArgs
    let src = head args
    input <- TI.readFile src
    TI.writeFile src (T.toUpper input)