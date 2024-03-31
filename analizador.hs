-------------------------- ANALIZADOR LÉXICO -----------------------------------

-- Angela Estefanía Aguilar Medina

--------------------------------------------------------------------------------
-- Declaración de funciones para tipos de Tokens

-- Define a data type for token types
data TokenType = Variable | Entero | Real | Multiplicacion | Asignacion | Underscore | Resta | Potencia | Division | ParentesisAbre | ParentesisCierra | Comentario deriving (Show)

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
    | i == '+' = True 
    | i == '-' = True
    | i == '*' = True
    | i == '^' = True
    | otherwise = False

-- Function to print a token along with its type
printTokenWithType :: String -> TokenType -> IO ()
printTokenWithType token tokenType = putStrLn $ token ++ "\t\t" ++ show tokenType

-----------------------------------------------------------------------------------
-- Declare functions of states for lexic analyzer
deathState :: String -> IO () -- Death State
deathState s = do
    putStrLn "Inside death state"

q15 :: String -> IO () -- Accepting State variable close parenthesis
q15 "" = putStrLn "Inside q15 Accepting state "
q15 (x:xs)
    | x == '-' = q6 xs
    | isInteger x = q7 xs
    | isLetter x = q3 xs
    | isOperator x = q14 xs
    | x == '/' = q13 xs
    | otherwise = deathState xs

q14 :: String -> IO () 
q14 "" = putStrLn "Inside q14 "
q14 (x:xs)
    | x == '-' = q6 xs
    | isInteger x = q7 xs
    | otherwise = deathState xs

q13 :: String -> IO () 
q13 "" = putStrLn "Inside q13 "
q13 (x:xs)
    | x == '-' = q6 xs
    | isInteger x = q7 xs
    | x == '/' = q5 xs
    | otherwise = deathState xs

q12 :: String -> IO () 
q12 "" = putStrLn "Inside q12 "
q12 (x:xs)
    | isLetter x = q3 xs
    | x == '(' = printTokenWithType [x] ParentesisAbre >> q14 xs
    
    | otherwise = deathState xs

q11 :: String -> IO () -- Accepting State is a Real
q11 "" = putStrLn "Inside q11 Accepting State is a Real"
q11 (x:xs)
    | x == '/' = q13 xs
    | x == ')' = q15 xs -- problem with / simbol and comments
    | isOperator x = q12 xs
    | otherwise = deathState xs


-- check q10 and the variable with operators
q10 :: String -> IO () -- Accepting State variable
q10 "" = putStrLn "Inside q10"
q10 (x:xs)
    | isInteger x = q11 xs
    | otherwise = deathState xs


q9 :: String -> IO () 
q9 "" = putStrLn "Inside q9 "
q9 (x:xs)
    | isInteger x = q11 xs
    | x == '-' = q10 xs
    | otherwise = deathState xs

q8 :: String -> IO () --Accepting State is a Integer
q8 "" = putStrLn "Inside q8 Accepting State is a integer"
q8 (x:xs)
    | isInteger x = q8 xs
    | isOperator x = q12 xs
    | x == ')' = printTokenWithType [x] ParentesisCierra >> q15 xs
    | x == '/' = q13 xs
    | isEuler x = q9 xs
    | otherwise = deathState xs

q7 :: String -> IO () --Accepting State is an Integer
q7 "" = putStrLn "Inside q7 Accepting State is a Integer"
q7 (x:xs)
    | isInteger x = q7 xs  
    | x == '.' = q8 xs
    | isEuler x = q9 xs
    | otherwise = deathState xs 

q6 :: String -> IO () 
q6 "" = putStrLn "Inside q6 "
q6 (x:xs)
    | isInteger x = q7 xs
    | otherwise = deathState xs


q5 :: String -> IO () --Accepting State is a number -- is a comment
q5 "" = putStrLn "Inside q5 Accepting state"
q5 (_:xs) = putStrLn "Inside q5 (Accepting)"
    

q4 :: String -> IO ()
q4 "" = putStrLn "Inside q4"
q4 (x:xs)
    | x == '/' = q5 xs
    | otherwise = deathState xs

q3 :: String -> IO () --Accepting State is a number - is variable
q3 "" = putStrLn "Inside q3 --Accepting State is a number - is variable"
q3 (x:xs)
    | isInteger x = q3 xs
    | isLetter x = q3 xs
    | x == '_' = q3 xs
    | isOperator x = q2 xs
    | x == '/' = q4 xs
    | x == ')' = printTokenWithType [x] ParentesisCierra >> q15 xs
    | otherwise = deathState xs

q2 :: String -> IO () 
q2 "" = putStrLn "Inside q2 "
q2 (x:xs)
    | isLetter x = q3 xs
    | x == '-' = q6 xs
    | isInteger x = q7 xs
    | x == '(' = printTokenWithType [x] ParentesisAbre >> q14 xs
    | otherwise = deathState xs

q1 :: String -> IO () --Accepting State is a number - is variable
q1 "" = putStrLn "Inside q1 (Accepting State)"
q1 (x:xs)
    | isLetter x = q1 xs
    | isInteger x = q1 xs
    | x == '_' = q1 xs
    | x == '=' = printTokenWithType [x] Asignacion >> q2 xs
    | otherwise = deathState xs


q0 :: String -> IO ()
q0 "" = deathState ""  -- Error: Empty string
q0 (x:xs)
    | isLetter x = q1 xs
    | isInteger x = q7 xs
    | x == '-' = printTokenWithType [x] Resta >> q6 xs
    | x == '/' = q4 xs
    | otherwise = deathState xs  -- Error: Unknown token


main :: IO ()
main = do
    putStrLn "Ingrese un string:"
    input <- getLine
    putStrLn "¿Es un número?"
    putStrLn "Token\t\tType"
    q0 input
    

    {- How to read a txt file an output its message
    contents <- readFile "example.txt"
    putStrLn contents -}
   
