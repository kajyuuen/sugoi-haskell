-- # パターン

-- 7が関数の引数のときは"Luccy number seven!"
-- それ以外が引数だったときは"Sorry..."
lucky :: Int -> String
lucky 7 = "Luccy number seven!"
lucky x = "Sorry..."

sayMe :: Int -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
-- addVectors a b = (fst a + fst b, snd a + snd b) 
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Error!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "empty"
tell (x:[]) = "one: " ++ show x
tell (x:y:[]) = "two: " ++ show x ++ " and " ++ show y
tell (_) = "This is long"

-- asパターン
firstLetter :: String -> String
firstLetter "" = "Empty"
firstLetter all@(x:xs) = "The first letter of" ++ all ++ " is " ++ [x]

-- # ガード

-- bmiTell :: Double -> String
-- bmiTell bmi
--     | bmi <= 18.5 = "Underweight"
--     | bmi <= 25.0 = "Normal"
--     | bmi <= 30.0 = "Fat"
--     | otherwise = "Very Fat"

-- bmiTell :: Double -> Double -> String
-- bmiTell weight height
--     | weight / height ^ 2 <= 18.5 = "Underweight"
--     | weight / height ^ 2 <= 25.0 = "Normal"
--     | weight / height ^ 2 <= 30.0 = "Fat"
--     | otherwise = "Very Fat"

-- bmiTell :: Double -> Double -> String
-- bmiTell weight height
--     | bmi <= 18.5 = "Underweight"
--     | bmi <= 25.0 = "Normal"
--     | bmi <= 30.0 = "Fat"
--     | otherwise = "Very Fat"
--     where bmi = weight / height ^ 2

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Fat"
    | otherwise = "Very Fat"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Fat"
    | otherwise = "Very Fat"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT

badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello!"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- calcBmis :: [(Double, Double)] -> [Double]
-- calcBmis xs = [bmi w h | (w, h) <- xs]
--     where bmi weight height = weight / height ^ 2 -- bmiは関数

-- p46から
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- calcBmis :: [(Double, Double)] -> [Double]
-- calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

-- discribeList :: [a] -> String
-- discribeList ls = "The list is "
--                   ++ case ls of [] -> "empty."
--                                 [x] -> "a singleton list."
--                                 xs -> "a longer list."

discribeList :: [a] -> String
discribeList ls = "The list is " ++ what ls
                    where what [] = "empty."
                          what [x] = "a singleton list."
                          what xs = "a longer list."
