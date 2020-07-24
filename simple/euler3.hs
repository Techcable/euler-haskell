import qualified Utils.Primes as Primes

targetNumber = 600851475143

main = do
    putStr "Prime factors of 13195: "
    putStrLn $ show $ Primes.primeFactors 13195
    putStr "Prime factors of "
    putStr $ show targetNumber 
    putStr ": "
    putStrLn $ show $ solutionFactors
    putStr "Solution: "
    putStrLn $ show $ maximum solutionFactors
    where solutionFactors = (Primes.primeFactors targetNumber)