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

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT 
                               else if lastName1 < lastName2
                                   then LT
                                   else compareFirstNames name1 name2
    where lastName1 = snd name1
          lastName2 = snd name2

compareFirstNames name1 name2 = if firstName1 > firstName2
                                then GT
                                else if firstName1 < firstName2
                                    then LT
                                    else EQ
    where firstName1 = fst name1
          firstName2 = fst name2