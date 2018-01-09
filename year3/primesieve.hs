module PrimeSieve where

-- Note: It's better for primes/primes' to not take an argument. The compiler
-- is much more easily able to optimise the value to the single infinite list
-- that it is.

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\a -> a `mod` x /= 0) xs)

primes :: Integer -> [Integer]
primes n
  | n < 0     = []
  | otherwise = take (fromIntegral n) (sieve [2 .. ])

isPrime :: Integer -> Bool
isPrime n
  | n == 2    = True
  | otherwise = all (\a -> n `mod` a /= 0) [2 .. n `div` 2]

primes' :: Integer -> [Integer]
primes' n
  | n < 0     = []
  | otherwise = take (fromIntegral n) (filter isPrime [2 .. ])
