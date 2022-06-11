sumSquareOrSquareSum x y = (\sumSquare squareSum ->
                           if sumSquare > squareSum
                           then sumSquare
                           else squareSum) (x^2 + y^2) ((x+y)^2)

doubleDouble x = (\d -> d * 2) (x * 2)

overwrite x = (\x -> 
    (\x -> 
        (\x -> x) 4 
        ) 3
    ) 2

counter x = (\x -> x + 1)
            ((\x -> x + 1)   -- ここから下を↑のラムダの引数として渡す     
             ((\x -> x) x))     