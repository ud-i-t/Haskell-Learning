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

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddleName "Jerome" "David" "Salinger"