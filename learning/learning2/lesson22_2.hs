-- lesson22
-- Quick check 22-1
import System.Environment

main :: IO ()
main = do
    a <- mapM (\_ -> getLine) [0..2]
    mapM_ putStrLn a

