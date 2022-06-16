-- lesson6: list
paExample1 = (!!) "dog"
paExample2 = ("dog" !!)
paExample3 = (!! 2)

isPalindrome word = word == reverse word
takeLast n aList = reverse (take n (reverse aList))
ones n = take n (cycle [1])
assignToGroups n aList = zip groups aList
    where groups = cycle [1 .. n]

myRepeat x = cycle [x]
subseq begin end list = take difference (drop begin list) 
    where difference = end - begin
inFirstHalf list x = elem x firstHalf
    where midPoint = (length list) `div` 2
          firstHalf = take midPoint list
main = do
    print $ [1,2,3] !! 0 
    print $ "puppies" !! 4
    print $ paExample1 2
    print $ paExample2 2
    print $ paExample3 "dog"
    
    print $ length [1..20]
    print $ length [(10,20),(1,2),(15,16)]
    print $ length "quicksand"

    print $ reverse [1,2,3]
    print $ reverse "hoge"

    print "isPalindrome:"
    print $ isPalindrome "cheese"
    print $ isPalindrome "racecar"
    print $ isPalindrome [1,2,3]
    print $ isPalindrome [1,2,1]

    print "elem:"
    print $ elem 13 [0,13 .. 100]
    print $ elem 'p' "cheese"

    print "take:"
    print $ takeLast 10 [1 .. 100]
    print $ takeLast 5 "silversecond"

    print "zip:"
    print $ zip "dog" "rabbit"
    print $ zip ['a' .. 'f'] [1 ..]

    print "cycle:"
    print $ ones 2
    print $ ones 4
    print $ assignToGroups 3 ["file1.txt", "file2.txt", "file3.txt", "file4.txt", "file5.txt", "file6.txt", "file7.txt", "file8.txt"]
    print $ assignToGroups 2 ["Bob", "Kathy", "Sue", "Joan", "Jim", "Mike"]
