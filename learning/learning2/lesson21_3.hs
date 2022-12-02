-- lesson21
-- practice

-- 
main :: IO ()
main = do
    putStrLn "put a number"
    number <- getLine
    let fibNum = fib (read number)
    putStrLn (show fibNum)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1)) + (fib (n-2)) 