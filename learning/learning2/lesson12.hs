-- lesson12
-- 型シノニム
type PatientName = (String,String)
type Age = Int
type Height = Int

firstName :: PatientName -> String
firstName name = fst name

lastName :: PatientName -> String
lastName name = snd name

patientInfo :: PatientName -> Age -> Height -> String
patientInfo name age height = fullname ++ " " ++ ageHeight
    where fullname = (lastName name) ++ ", " ++ (firstName name)
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- 型の作成
data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'


data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType -- データコンストラクタ

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

patient1BT :: BloodType
patient1BT = BloodType A Pos
patient2BT :: BloodType
patient2BT = BloodType O Neg
patient3BT :: BloodType
patient3BT = BloodType AB Pos

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True    -- どの血液型にも輸血できる
canDonateTo _ (BloodType AB _) = True   -- どの血液型からでも輸血できる
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False