sumSquareOrSquareSum x y = (\sumSquare squareSum ->
                           if sumSquare > squareSum
                           then sumSquare
                           else squareSum) (x^2 + y^2) ((x+y)^2)

doubleDouble x = (\d -> d * 2) (x * 2)