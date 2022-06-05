module Action where
import Lib
import System.Random

randNum = randomRIO (0, 100) :: IO Int