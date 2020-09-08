module Utils.Primes (
    primes,
    primeFactors
) where

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

primeFactors target = [p | p <- (takeWhile (<=target) primes), target `rem` p == 0]
