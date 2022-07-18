-- lesson14
import Data.List

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord)

instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"

-- instance Eq SixSidedDie where
--     (==) S6 S6 = True
--     (==) S5 S5 = True
--     (==) S4 S4 = True
--     (==) S3 S3 = True
--     (==) S2 S2 = True
--     (==) S1 S1 = True
--     (==) _ _ = False

-- instance Ord SixSidedDie where
--     compare S6 S6 = EQ
--     compare S6 _ = GT
--     compare _ S6 = LT
--     compare S5 S5 = EQ
--     compare S5 _ = GT
--     compare _ S5 = LT
--     compare S4 S4 = EQ
--     compare S4 _ = GT
--     compare _ S4 = LT

data TwoSidedDie = One | Two
instance Show TwoSidedDie where
    show One = "one"
    show Two = "two"

data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

names :: [Name]
names = [Name ("Emil", "Cioran"),
         Name ("Eugene", "Thacker"),
         Name ("Friedrich", "Nietzche")]

