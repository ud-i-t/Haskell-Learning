module Fact where
import Lib

fact 0 = 1
fact n | n > 0 = n * fact (n - 1)