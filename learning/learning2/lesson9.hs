-- lesson9: 高階関数
import Data.Char

addAnA [] = []
addAnA (x:xs) = ("a " ++ x):addAnA xs

squareAll [] = []
squareAll (x:xs) = (x^2):squareAll xs 

myFilter test [] = []
myFilter test (x:xs) = if test x
                       then x:myFilter test xs
                       else myFilter test xs

remove test [] = []
remove test (x:xs) = if test x
                     then remove test xs
                     else x:remove test xs

myProduct acc [] = acc
myProduct acc (x:xs) = myProduct (acc*x) xs

concatAll xs = foldl (++) "" xs
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x
myReverse xs = foldl rcons [] xs

myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f newAcc xs
    where newAcc = f acc x

myElem list target = length filterd > 0
    where filterd = filter (\y -> y == target) list

isPalindrome word = processed == reverse processed
    where filterd = filter (\x -> x /= ' ') word
          processed = map toLower filterd

addHarmonic a b = a + 1/b
myHarmonic n = foldl addHarmonic 0 [1..n]

harmonic n = sum (take n seriesValues)
    where seriesPairs = zip (cycle [1.0]) [1.0,2.0 .. ]
          seriesValues = map
                         (\pair -> (fst pair)/(snd pair))
                         seriesPairs 

main = do
    print $ map ("a " ++) ["train", "plane", "boat"]
    print $ map (^2) [1, 2, 3]
    print $ filter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"]
    print $ myFilter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"]
    print $ remove (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"]
    print $ foldl (+) 0 [1..4]