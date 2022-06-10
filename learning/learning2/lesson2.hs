calcChange owed given = if change > 0
                        then change
                        else 0
    where
        change = given - owed

inc x = x + 1
double x = x * 2
square x = x * x 
hoge x = if even x
         then x - 2
         else x * 3 + 1