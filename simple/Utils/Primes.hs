module Utils.Primes (
    primesUntil,
    primeFactors
) where

primesUntil :: Int -> [Int]

dividesAny divisors target = any (\x -> target `mod` x == 0) divisors

-- TODO: Use a proper primes sieve instead of trial division (somehow)
primesUntil x = let
    rootPrimes = if endTest <= 16 then [2,3,5,7,11,13] else primesUntil endTest
    in rootPrimes ++ (filter (\x -> not $ dividesAny rootPrimes x) [2..x])
    where endTest = ceiling $ sqrt $ fromIntegral x

primeFactors :: Int -> [Int]
primeFactors x = let
    possibleFactors = primesUntil x
    in filter (\fac -> x `mod` fac == 0) possibleFactors