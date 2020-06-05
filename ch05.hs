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

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- g(f(a, b): return c, [a], [b]): return [c]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--     where g x y = f y x
flip' f y x = f x y

-- map' :: (a -> b) -> [a] -> [b]
-- map' _ [] = []
-- map' f (x:xs) = f x : map f xs

-- filter' :: (a -> Bool) -> [a] -> [a]
-- filter' _ [] = []
-- filter' f (x:xs)
--     | f x = x : filter' f xs
--     | otherwise = filter' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = filter (<=x) xs
        larger = filter (>x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
    where p x = (x `mod` 3829 == 0)

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd  n = n : chain (n * 3 + 1)

numLongChains :: Int
-- numLongChains = length (filter isLong (map chain [1..100]))
--     where isLong xs = length xs > 15
numLongChains = length (filter (\xs -> length xs > 15)
                                (map chain [1..100]))

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
-- 上でされているデフォルトのカリー化を可視化すると…
addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

-- flip' :: (a -> b -> c) -> b -> a -> c
-- flip' f = \x y -> f y x
-- flip'関数fは無名関数が引数x yを受け取り,f y xとして返す

-- sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
-- sum' = foldl (+) 0

map' :: (a -> b) -> [a] -> [b]
-- map' f xs = foldr (\x acc -> f x : acc) [] xs
map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
-- reverse' = foldl (\acc x -> x : acc) []
reverse' = foldl (flip(:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []
-- pは述語

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (+) 0 xs
sum' = foldl (+) 0