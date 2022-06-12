import Data.List

ifEven myFunction x = if even x
                      then myFunction x
                      else x

inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n

names = [("Ian", "Curtis"),
         ("Bernard", "Sumner"),
         ("Peter", "Hook"),
         ("Peter", "Hooa"),
         ("Stephan", "Morris")]

compareLastNames name1 name2 = if result == EQ
                               then compare (fst name1) (fst name2)
                               else result
    where result = compare (snd name1) (snd name2)

sfOffice name = if lastName < "L"
                then nameText
                     ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ ": PO Box 456 - Reno, NV, 89523"
    where nameText = snd name

dcOffice name = nameText ++ "- PO Box 5166 - Washington DC, dc, 44941"
    where nameText = (fst name) ++ " " ++ (snd name) ++ "Esq"

getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location