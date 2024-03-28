isLetter :: Char -> Bool
isLetter l = l `elem` ['a'..'z'] || l `elem` ['A'..'Z']

main :: IO ()
main = do
    putStrLn "Ingrese un carácter:"
    input <- getLine
    let char = head input
    putStrLn $ "Usando esLetra: " ++ show (isLetter char)
    