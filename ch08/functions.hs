import Control.Monad

-- main = do
--     putStr "Hey, "
--     putStr "I'm "
--     putStrLn "Andy!"

-- main = do
--     putChar 'a'
--     putChar 'b'

-- putStr :: String -> IO ()
-- putStr [] = return ()
-- putStr (x:xs) = do
--     putChar x
--     putStr xs

-- main = do
--     print True
--     print 3.0

-- main = do
--     input <- getLine
--     when (input == "SWORDFISH") $ do
--         putStrLn input

-- main = do
--     rs <- sequence [getLine, getLine, getLine]
--     print rs

main = do
    colors <- forM [1, 2, 3, 4] $ \a -> do
        putStrLn $ "Which color do you associate with the number "
                    ++ show a ++ "?"
        color <- getLine
        return color
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors;