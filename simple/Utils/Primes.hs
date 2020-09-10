module Utils.Primes (
    primes,
    primeFactors
) where

import Debug.Trace

-- Had trouble figuring out how to avoid creating tons of lists
--
-- Based on this wiki page:
-- http://wiki.haskell.org/Prime_numbers#Optimal_trial_division
primes = 2 : oddprimes
    where
        oddprimes =  sieve [3,5..] 9 oddprimes
    -- Pattern match `~` ensures that `future_primes` won't be eagerly evaluated
    -- See page: https://wiki.haskell.org/Lazy_pattern_match
        sieve (first_prime:remaining_primes) limit futures@ ~(future_test:remaining_futures)
          -- If the start of the list is less than the current limit of known primes,
          -- we've already tested and verified it's primes. It can simply be prepended to the
          -- result list
          | first_prime < limit = first_prime : sieve remaining_primes limit futures
          -- Otherwise filter out the list of primes based on if it's not divisible by `future_test`
          | otherwise =  sieve [x | x <- remaining_primes, x `rem` future_test /= 0]
                                (head remaining_futures^2) remaining_futures

splitFactors :: Int -> [Int] -> [Int]
splitFactors remaining primes @ (first_prime:remaining_primes)
    | remaining < first_prime = [] -- No more remaining primes to split
    | remaining == first_prime = [remaining]  -- One last prime
    -- Divisible by the target, append it to the list of primes
    | (remaining `rem` first_prime == 0) = first_prime : (splitFactors
        (remaining `div` (last $ takeWhile (\multiple -> remaining `rem` multiple == 0) (map (first_prime^) [1..])))
        remaining_primes)
    -- Skip this prime, it's not a factor
    | otherwise = (splitFactors remaining remaining_primes)


primeFactors target = splitFactors target (takeWhile (<=target) primes) 
    where
