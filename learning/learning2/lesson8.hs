-- lesson8: 再帰
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : rest
    where rest = myTake (n - 1) xs

finiteCycle (first:rest) = first:rest ++ [first]

myCycle (first:rest) = first:myCycle (rest++[first])

myReverse [] = []
myReverse [x]= [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)