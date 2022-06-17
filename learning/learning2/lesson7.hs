-- lesson7: 再帰
sayAmount n = case n of
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

sayAmountV2 1 = "one"
sayAmountV2 2 = "two"
sayAmountV2 n = "a bunch"

isEmpty [] = True
isEmpty _ = False

myHead (x:xs) = x
myHead [] = error "No head for empty list"

myTail (_:xs) = xs
myTail [] = []

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)