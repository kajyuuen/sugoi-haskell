import qualified Data.Map as Map
-- phoneBook :: [(String, String)]
-- phoneBook = [
--         ("a", "1")
--        ,("a", "2")
--        ,("a", "3")
--        ,("d", "4")
--        ,("e", "5")
--     ]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
phoneBook = [
        ("a", "1")
       ,("a", "2")
       ,("a", "3")
       ,("d", "4")
       ,("e", "5")
    ]

--

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
                Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
                Just (state, code) -> if state /= Taken
                                        then Right code
                                        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
        [
                (100, (Taken, "passward1")),
                (101, (Free, "passward2")),
                (103, (Free, "passward3")),
                (105, (Free, "passward4")),
                (109, (Taken, "passward5")),
                (110, (Taken, "passward6"))
        ]

