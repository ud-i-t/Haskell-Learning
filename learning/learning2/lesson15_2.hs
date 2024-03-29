-- lesson15.2
xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
    where remainder = n `mod` 2
          nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' n)
          missingBits = maxBits - (length reversedBits)
          leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
    where size = length bits
          indices = [size-1 ,size-2 .. 0]
          trueLocations = filter (\x -> fst x == True)
                          (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

-- 15.4 ワンタイムパッド
myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\pair -> (fst pair) `xor` (snd pair))
                              (zip padBits plainTextBits)
    where padBits = map charToBits pad
          plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
    where bitList = applyOTP' pad plainText

class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotE