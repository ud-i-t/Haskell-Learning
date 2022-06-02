module Color where
import Lib

data Color = Red | Green | Blue | Magenta | Cyan | Yellow | White deriving (Show, Eq)

mix Red Green = Yellow
mix Red Blue = Magenta
mix Red Yellow= Yellow
mix Red Magenta = Magenta
mix Red Cyan = White
mix Green Blue = Cyan
mix Green Yellow = Yellow
mix Green Magenta = White
mix Green Cyan = Cyan
mix Blue Yellow = White
mix Blue Magenta = Magenta
mix Blue Cyan = Cyan
mix Yellow Magenta = White
mix Yellow Cyan = White
mix Magenta Cyan = White
mix White _ = White
mix c1 c2 | c1 == c2 = c1
          | otherwise = mix c2 c1