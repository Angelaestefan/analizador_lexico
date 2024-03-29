-------------------------- ANALIZADOR LÉXICO -----------------------------------

-- Angela Estefanía Aguilar Medina

--------------------------------------------------------------------------------
-- Declaración de funciones para tipos de Tokens

-- Función que identifica si el caracter ingresado es una lettra
isLetter :: Char -> Bool
isLetter l = l `elem` ['a'..'z'] || l `elem` ['A'..'Z']

isEuler :: Char -> Bool
isEuler i = i == 'e' || i == 'E'

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

-----------------------------------------------------------------------------------
-- Declare functions of states for lexic analyzer
deathState :: String -> IO ()
deathState s = do
    putStrLn "Inside death state"

q10 :: String -> IO ()
q10 s = do
    putStrLn "Inside q10"

q9 :: String -> IO ()
q9 "" = putStrLn "Inside q9"
q9 (x:xs)
    | isInteger x = q1 xs

q8 :: String -> IO ()
q8 s = do
    putStrLn "Inside q8"

q7 :: String -> IO ()
q7 "" = putStrLn "Inside q7"
q7 (x:xs)
    | isInteger x = q1 xs

q6 :: String -> IO ()
q6 "" = putStrLn "Inside q6"
q6 (x:xs)
    | isInteger x = q1 xs

q5 :: String -> IO ()
q5 "" = putStrLn "Inside q5"
q5 (x:xs)
    | isInteger x = q6 xs
    | otherwise = deathState xs

q4 :: String -> IO ()
q4 "" = putStrLn "Inside q4"
q4 (x:xs)
    | isInteger x = q6 xs
    | x == '-' = q5 xs
    | otherwise = deathState xs

q3 :: String -> IO ()
q3 "" = putStrLn "Inside q3"
q3 (x:xs)
    | isInteger x = q1 xs
    | otherwise = deathState xs

q2 :: String -> IO ()
q2 "" = putStrLn "Inside q2"
q2 (x:xs)
    | isInteger x = q2 xs
    | isEuler x = q4 xs
    | x == '/' = q8 xs
    | x == '.' = deathState xs
    | x == '-' = deathState xs
    | isOperator x = deathState xs
    | otherwise = deathState xs

q1 :: String -> IO ()
q1 "" = putStrLn "Inside q1"
q1 (x:xs)
    | isInteger x = q1 xs
    | x == '.' = q2 xs
    | isEuler x = q4 xs
    | x == '-' = deathState xs
    | x == '/' = deathState xs
    | isOperator x = deathState xs
    | otherwise = deathState xs


q0 :: String -> IO ()
q0 "" = deathState ""  -- Error: Empty string
q0 (x:xs)
    | isInteger x = q1 xs
    | x == '-' = q3 xs
    | x == '/' = q8 xs
    | isLetter x = q10 xs
    | x == '.' = deathState xs
    | isEuler x = deathState xs
    | otherwise = deathState xs  -- Error: Unknown token


main :: IO ()
main = do
    putStrLn "Ingrese un string:"
    input <- getLine
    putStrLn "¿Es un número?"
    q0 input

    {- How to read a txt file an output its message
    contents <- readFile "example.txt"
    putStrLn contents -}
   
