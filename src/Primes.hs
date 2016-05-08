module Primes (primes) where

import Data.PSQueue

newtype Prime = Prime Integer
  deriving (Show, Eq, Ord)

type PrimeQ = PSQ Prime Integer

restPrimes :: PrimeQ -> [Integer] -> [Integer]
restPrimes q (v : vs) =
  let (Just (_ :-> nextComp)) = findMin q
  in if nextComp == v
     then restPrimes (cyclePrimes q v) vs
     else v : restPrimes (insert (Prime v) (v*v) q) vs
  where cyclePrimes :: PrimeQ -> Integer -> PrimeQ
        cyclePrimes q' v' =
          let (Just ((k@ (Prime p)) :-> c)) = findMin q'
          in if v' == c
             then cyclePrimes (insert k (c + 2*p) (deleteMin q')) v'
             else q'

primes :: [Integer]
primes = [2, 3] ++ restPrimes (insert (Prime 3) 9 empty) [5, 7 ..]
