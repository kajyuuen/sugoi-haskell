import Data.List
import Data.Char
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort .words
-- 関数合成なしver
-- wordNums xs = map (\ws -> (head ws, length ws)) (group (sort (words xs)))

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40)[1..]

-- findKey :: (Eq k) => k -> [(k, v)] -> v
-- findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

-- 改良版
-- findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k, v):xs)
--     | key == k = Just v
--     | otherwise = findKey key xs

-- 改良版ver2
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key xs = foldr
                    (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

-- Map
-- phoneBook :: Map.Map String String
-- phoneBook = Map.fromList $ 
--     [
--         ("a", "1")
--        ,("b", "2")
--        ,("c", "3")
--        ,("d", "4")
--        ,("e", "5")
--     ]

-- newBook = Map.insert "f" "6" phoneBook

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

-- intBook = Map.map string2digits phoneBook

-- phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
-- phoneBookToMap xs = Map.fromListWith add xs
--             where add number1 number2 = number1 ++ ", " ++ number2
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
phoneBook :: [(String, String)]
phoneBook = [
        ("a", "1")
       ,("a", "2")
       ,("a", "3")
       ,("d", "4")
       ,("e", "5")
    ]
phoneBookToMap phoneBook