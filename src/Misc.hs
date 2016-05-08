module Misc where

digits :: Integer -> [Integer]
digits 0 = []
digits n = (n `mod` 10) : digits (n `quot` 10)
