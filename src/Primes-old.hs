module Primes (primes) where

import qualified Data.PQueue.Prio.Min as PQ
import Control.Monad.State.Lazy

type PrimesState = ([Integer], PQ.MinPQueue Integer Integer)

initPrimes :: PrimesState
initPrimes = ([5, 7 ..], PQ.fromList [(9, 3)])

nextPrime :: () -> State PrimesState Integer
nextPrime () = do
  (nns@(n : ns), pq) <- get
  let (q, p) = PQ.findMin pq
  if q > n
    then do put (ns, PQ.insert (3*n) n pq)
            return n
    else do put (if q==n then ns else nns,
                 PQ.insert (q + 2*p) p $ PQ.deleteMin pq)
            nextPrime ()

listPrimes :: () -> State PrimesState [Integer]
listPrimes () = do
  p <- nextPrime ()
  ps <- listPrimes ()
  return (p : ps)

primes :: [Integer]
primes = 2 : 3 : evalState (listPrimes ()) initPrimes
