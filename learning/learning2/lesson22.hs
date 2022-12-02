-- lesson22
-- practice

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args -- mapM_は結果を捨てる