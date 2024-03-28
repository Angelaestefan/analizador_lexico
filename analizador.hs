-------------------------- ANALIZADOR LÉXICO -----------------------------------

-- Angela Estefanía Aguilar Medina

-- Declaración de funciones para tipos de Tokens

-- Función que identifica si el caracter ingresado es una lettra
isLetter :: Char -> Bool
isLetter l = l `elem` ['a'..'z'] || l `elem` ['A'..'Z']

-- Función que identifica si el caracter ingresado es un entero
isInteger :: Char -> Bool
isInteger n = n `elem` ['0'..'9']

-- Función que identifica si el caracter ingresado es una operador
isOperator :: Char -> Bool
isOperator i
    | i == '=' = True
    | i == '+' = True
    | i == '-' = True
    | i == '*' = True
    | i == '/' = True
    | i == '^' = True
    | otherwise = False

-- Función que identifica si el caracter ingresado es una simbolo especial
isEspecialSimbol :: Char -> Bool
isEspecialSimbol i
    | i == '(' = True
    | i == ')' = True
    | otherwise = False

main :: IO ()
main = do
    putStrLn "Ingrese un carácter:"
    input <- getLine
    let char = head input
    putStrLn $ "¿Es un número? " ++ show (isEspecialSimbol char)
