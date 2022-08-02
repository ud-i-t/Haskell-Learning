-- lesson15
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar charToEncrypt = rotN sizeOfAlphabet charToEncrypt
    where sizeOfAlphabet = 1 + fromEnum (maxBound :: Char)

fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1,L3,L4,L1,L1,L2]

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
    where alphabetSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot4l = rotN alphabetSize

data ThreeLetterAlphabet = Alpha 
                         | Beta
                         | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncorder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncorder vals = map rot3l vals
 where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot3l = rotN alphaSize

rotNdecorder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecorder n c = toEnum rotation
    where halfN = n `div` 2
          offset = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
          rotation = offset `mod` n

threeLetterDecorder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecorder vals = map rot3ldecorder vals
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3ldecorder = rotNdecorder alphaSize

rotEncoder :: String ->String
rotEncoder text = map rotChar text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNdecorder alphaSize

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

-- 暗号型の定義
class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

-- RotNを使った暗号化・復号化
data Rot = Rot

instance Cipher Rot where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

-- ワンタイムパッドを使った暗号化・復号化
data OneTimePad = OTP String

instance Cipher OneTimePad where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])