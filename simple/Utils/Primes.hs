module Utils.Primes (
    primesUntil,
    primeFactors
) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

dividesAny divisors target = any (\x -> target `mod` x == 0) divisors

-- NOTE: Internal method for prime sieve
eliminatePrimes :: IntSet -> Int -> Int -> IntSet
eliminatePrimes primeSet start stop
    | start >= stop = primeSet -- Finished
    -- Just skip to next number if we know `start` is not prime
    | start `IntSet.notMember` primeSet =  eliminatePrimes primeSet (start + 1) stop
    | otherwise = eliminatePrimes filteredPrimeSet (start + 1) stop -- Just continue eliminating
        where primeSquared = ceiling $ sqrt $ fromIntegral start
              filteredPrimeSet = primeSet `IntSet.difference` (IntSet.fromDistinctAscList $
                takeWhile (<=stop) (map (\i -> start ^ 2 + start * i) [0..]))


primesUntil :: Int -> [Int]

primesUntil x = filter (\n -> IntSet.member n primeSet) [2..x]
    where primeSet = eliminatePrimes (IntSet.fromDistinctAscList [2..x]) 2 x

primeFactors :: Int -> [Int]
primeFactors x = let
    possibleFactors = primesUntil x
    in filter (\fac -> x `mod` fac == 0) possibleFactors