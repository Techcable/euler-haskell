isMultipleOf3Or5 x = x `mod` 5 == 0 || x `mod` 3 == 0

multiplesOf3Or5 :: [Integer];
multiplesOf3Or5 = filter isMultipleOf3Or5 [1..]

main = putStr "Multiples of 3 & 5 < 10: " >>
       putStrLn (show (takeWhile (<10) multiplesOf3Or5)) >>
       putStr "Sum of multiples of 3 & 5 < 1000: " >>
       putStrLn (show $ sum (takeWhile (<1000) multiplesOf3Or5))
