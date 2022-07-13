-- lesson14
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"

instance Eq SixSidedDie where
    (==) S6 S6 = True
    (==) S5 S5 = True
    (==) S4 S4 = True
    (==) S3 S3 = True
    (==) S2 S2 = True
    (==) S1 S1 = True
    (==) _ _ = False

data TwoSidedDie = One | Two
instance Show TwoSidedDie where
    show One = "one"
    show Two = "two"