import Primes

-- step = nextPrime . snd

main :: IO ()
main = putStrLn . show $ take 18 primes
