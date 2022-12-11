-- lesson22
import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)
    
-- Quick check 22-2
myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n func = mapM (\_ -> func) [1 .. n]
