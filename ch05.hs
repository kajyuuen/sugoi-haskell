multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

-- let multTwoWithNine = multThree 9
-- multTwoWithNine 2 3

compareWithHundred :: Int -> Ordering
-- compareWithHundred x = compare 100 x
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])