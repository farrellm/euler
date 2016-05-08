import Primes
import Misc
import Data.List
import qualified Data.Set as Set

isPandigit n = let ds = digits n
                   len  = fromIntegral $ length ds
               in Set.fromList [1 .. len] == Set.fromList ds

filter isPandigit (takeWhile ((< 8) . length . digits) primes)
