cup flOz = \message -> message flOz
coffeeCup  = cup 12

getOz aCup = aCup (\flOz -> flOz)
isEmpty aCup = getOz aCup == 0
afterManyShip = foldl drink coffeeCup [1,1,1,1,1]

drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
    where flOz = getOz aCup
          ozDiff = flOz - ozDrank


robot (name,attack,hp) = \message -> message (name,attack,hp)
name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,hp) = hp
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAtttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

printRobot aRobot = aRobot (\(n,a,h) -> n ++
                                        " attack:" ++ (show a) ++
                                        " hp:" ++ (show h))

killerRobot = robot ("Kill3r", 25, 200)
nicerRobot = setName killerRobot "kitty"
gentlerRobot = setAtttack killerRobot 5
softerRobot = setHP killerRobot 50