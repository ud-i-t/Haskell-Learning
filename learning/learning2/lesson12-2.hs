-- lesson12
-- 型シノニム
type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddleName FirstName MiddleName LastName

showName :: Name -> String
showName (Name firstName lastName) = firstName ++ " " ++ lastName
showName (NameWithMiddleName firstName middleName lastName) 
    = firstName ++ " " ++ middleName ++ " " ++ lastName

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddleName "Jerome" "David" "Salinger"

data Sex = Male | Female

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType -- データコンストラクタ

-- data Patient = Patient Name Sex Int Int Int BloodType
johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeSmith :: Patient
janeSmith = Patient (NameWithMiddleName "Jane" "Elizabeth" "Smith") Female 28 59 178 (BloodType B Neg)

--レコード構文
data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

jackieSmith :: Patient
jackieSmith = Patient {name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }
jackieSmithUpdated = jackieSmith { age = 44 }

-- 練習問題
canDonateToP :: Patient -> Patient -> Bool
canDonateToP a b = canDonateTo (bloodType a) (bloodType b)

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True    -- どの血液型にも輸血できる
canDonateTo _ (BloodType AB _) = True   -- どの血液型からでも輸血できる
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

patientSummary :: Patient -> String
patientSummary p = "*************\n" ++
                   "Patient Name: " ++ showName (name p) ++ "\n" ++
                   "Sex: " ++ showSex (sex p) ++ "\n" ++
                   "Age: " ++ show (age p) ++ "\n" ++
                   "Height: " ++ show (height p) ++ "in.\n" ++
                   "Weight: " ++ show (weight p) ++ "lbs.\n" ++
                   "Blood Type: " ++ showBloodType (bloodType p)
