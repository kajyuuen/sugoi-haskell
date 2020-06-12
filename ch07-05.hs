data Person = Person { firstName :: String
                       , lastName :: String
                       , age :: Int              
                     } deriving (Eq, Show, Read)

mikeD = Person { firstName = "M", lastName = "D", age = 43}
adRock = Person { firstName = "A", lastName = "H", age = 41}
mca = Person { firstName = "A", lastName = "Y", age = 41}