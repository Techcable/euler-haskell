fibonnaci :: (Integral a) => a -> a 
fibonnaci 0 = 0
fibonnaci 1 = 1
fibonnaci x = fibonnaci(x - 1) + fibonnaci(x - 2)

fibonnaci_seq = map fibonnaci [1..]

solve = sum (filter (\x -> x `mod` 2 == 0)
        (takeWhile (<=4000000) fibonnaci_seq))

main = do
    putStr "First 10 fibonnaci numbers: "
    putStrLn $ show $ take 10 fibonnaci_seq
    putStr "Solution: "
    putStrLn $ show solve
