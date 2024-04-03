import System.IO
-------------------------- ANALIZADOR LÉXICO -----------------------------------

-- Angela Estefanía Aguilar Medina

--------------------------------------------------------------------------------
-- Declaración de funciones para tipos de Tokens

-- Define a data type for token types
data TokenType = Variable | Entero | Real | Multiplicacion | Asignacion | Underscore | Suma |Resta | Potencia | Division | ParentesisAbre | ParentesisCierra | Comentario deriving (Show)

-- Función que identifica si el caracter ingresado es una lettra
isLetter :: Char -> Bool
isLetter l = l `elem` ['a'..'z'] || l `elem` ['A'..'Z']

isEuler :: Char -> Bool
isEuler i = i == 'e' || i == 'E'

-- Función que identifica si el caracter ingresado es un entero
isInteger :: Char -> Bool
isInteger n = n `elem` ['0'..'9']



-- Función que identifica si el caracter ingresado es una operador
isOperator :: Char  ->Bool
isOperator i
    | i == '+' = True 
    | i == '-' = True
    | i == '*' = True
    | i == '^' = True
    | otherwise = False

operatorToken :: Char ->TokenType
operatorToken i
    | i == '+' = Suma 
    | i == '-' = Resta
    | i == '*' = Multiplicacion
    | i == '^' = Potencia
   

-- Function to print a token along with its type
printTokenWithType :: String -> TokenType -> IO ()
printTokenWithType token tokenType = putStrLn $ token ++ "\t\t" ++ show tokenType

-----------------------------------------------------------------------------------
-- Declare functions of states for lexic analyzer
deathState :: String -> IO () -- Death State
deathState s = do
    putStrLn "Inside death state"

q15 :: String -> String ->IO () -- Accepting State variable close parenthesis
q15 _"" = putStrLn "Inside q15 Accepting state "
q15 buff (x:xs)
    | x == '\n' = q0 [] xs
    | x == '-' = q6 (buff++[x]) xs
    | isInteger x = q7 (buff++[x]) xs
    | isLetter x = q3 (buff++[x]) xs
    | isOperator x =  printTokenWithType [x] (operatorToken x) >> q14 [] xs
    | x == '/' = q13 (buff++[x])xs
    | otherwise = deathState xs

q14 :: String ->String -> IO () 
q14 _"" = putStrLn "Inside q14 "
q14 buff (x:xs)
    | x == '\n' = q0 [] xs
    | x == '-' = q6 (buff++[x]) xs
    | isInteger x = q7 (buff++[x]) xs
    | otherwise = deathState xs

q13 :: String ->String -> IO () 
q13 _"" = putStrLn "Inside q13 "
q13 buff (x:xs)
    | x == '\n' = q0 [] xs
    | x == '-' = q6 (buff++[x]) xs
    | isInteger x = do
        printTokenWithType buff Variable 
        q7 (buff++[x]) xs
    | x == '/' = q5 (buff++[x]) xs
    | otherwise = deathState xs

q12 :: String -> String ->IO () 
q12 _"" = putStrLn "Inside q12 "
q12 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isLetter x = q3 (buff++[x]) xs
    | isInteger x = q7 (buff++[x]) xs
    | x == '(' = printTokenWithType [x] ParentesisAbre >> q14 [] xs
    | otherwise = deathState xs

q11 :: String -> String ->IO () -- Accepting State is a Real
q11 _"" = putStrLn "Inside q11 Accepting State is a Real"
q11 _ ('\n':xs) = q0 [] xs
q11 buff (x:xs)   
    | x == '\n' = q0 [] xs
    | x == '/' = q13 (buff++[x]) xs
    | x == ')' =  printTokenWithType [x] ParentesisCierra >>q15 [] xs 
    | isOperator x =  printTokenWithType [x] (operatorToken x) >> q12 [] xs
    | otherwise = deathState xs


-- check q10 and the variable with operators
q10 :: String -> String ->IO () -- Accepting State variable
q10 _"" = putStrLn "Inside q10"
q10 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isInteger x = q11 (buff++[x]) xs
    | otherwise = deathState xs


q9 :: String ->String -> IO () 
q9 _"" = putStrLn "Inside q9 "
q9 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isInteger x = q11 (buff++[x]) xs
    | x == '-' = q10 (buff++[x]) xs
    | otherwise = deathState xs

q8 :: String -> String ->IO () --Accepting State is a Integer
q8 _"" = putStrLn "Inside q8 Accepting State is a integer"
q8 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isInteger x = q8 (buff++[x]) xs
    | isOperator x = do
        printTokenWithType buff Real 
        printTokenWithType [x] (operatorToken x)     
        q12 [] xs
    | x == ')' = printTokenWithType [x] ParentesisCierra >> q15 [] xs
    | x == '/' = q13 (buff++[x]) xs
    | isEuler x = q9 (buff++[x]) xs
    | otherwise = deathState xs

q7 :: String -> String ->IO () --Accepting State is an Integer
q7 buff "" = printTokenWithType buff Entero
q7 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isInteger x = q7 (buff++[x]) xs  
    | x == '/' = do
        printTokenWithType buff Entero>>q4 [x] xs
    | x == '.' = q8 (buff++[x]) xs
    | isEuler x = q9 (buff++[x]) xs
    | isOperator x = do
        printTokenWithType buff Real
        printTokenWithType [x] (operatorToken x) >> q7 [] xs
    | otherwise = deathState xs 

q6 :: String ->String -> IO () 
q6 _"" = putStrLn "Inside q6 "
q6 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isInteger x = q7 (buff++[x]) xs
    | isLetter x = q3 (buff++[x]) xs
    | otherwise = deathState xs


q5 :: String -> String -> IO () -- Accepting State is a number -- is a comment
q5 buff "" = printTokenWithType buff Comentario  -- Imprimir el token completo como comentario
q5 buff (x:xs)
    | x == '\n' = q0 [] xs  -- Regresar al estado q0 cuando se encuentra un salto de línea
    | otherwise = q5 (buff ++ [x]) xs  -- Continuar acumulando el token

q4 :: String -> String ->IO ()
q4 _"" = putStrLn "Inside q4"
q4 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isLetter x = do
        printTokenWithType buff Division
        q3 ([x]) xs
    | x == ' ' = q0 [] xs
    | x == '/' = q5 (buff++[x]) xs
    | isInteger x = do
        printTokenWithType buff Division
        q7 ([x]) xs
    | otherwise = deathState xs

q3 :: String -> String -> IO () -- Accepting State is a number - is variable
q3 buff "" = printTokenWithType buff Variable
q3 _ ('\n':xs) = q0 [] xs
q3 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isInteger x = q3 (buff ++ [x]) xs
    | isLetter x = q3 (buff ++ [x]) xs
    | x == '_' = q3 (buff ++ [x]) xs
    | isOperator x = do
        printTokenWithType buff Variable 
        printTokenWithType [x] (operatorToken x) 
        q2 "" xs
    | x == '/' = do
        printTokenWithType buff Variable >> q4 [x] xs
    | x == ')' = do
        printTokenWithType buff Variable
        printTokenWithType [x] ParentesisCierra
        q15 "" xs
    | otherwise = deathState xs


q2 :: String ->String -> IO () 
q2 _"" = putStrLn "Inside q2 "
q2 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isLetter x = q3 (buff++[x]) xs
    | x == '-' = q6 (buff++[x]) xs
    | isInteger x = q7 (buff++[x]) xs
    | x == '(' = printTokenWithType [x] ParentesisAbre >> q14 [] xs
    | otherwise = deathState xs

q1 :: String ->String -> IO () --Accepting State is a number - is variable
q1 buff "" = printTokenWithType buff Variable
q1 _ ('\n':xs) = q0 [] xs
q1 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isLetter x = q1 (buff++[x]) xs
    | isInteger x = q1 (buff++[x]) xs
    | x == '_' = q1 (buff++[x]) xs
    | x == '=' = do
        printTokenWithType buff Variable 
        printTokenWithType [x] Asignacion >> q2 [] xs
    | isOperator x = printTokenWithType [x] (operatorToken x) >> q7 [] xs
    | x == '/' = do
        printTokenWithType buff Variable 
        printTokenWithType [x] Division >> q7 [] xs
    | otherwise = deathState xs


q0 :: String -> String -> IO ()
q0 _"" = deathState ""  -- Error: Empty string
q0 _ ('\n':xs) = q0 [] xs 
q0 buff (x:xs)
    | x == '\n' = q0 [] xs
    | isLetter x = q1 (buff++[x]) xs
    | isInteger x = q7 (buff++[x]) xs
    | x == '-' =  q6 (buff++[x]) xs
    | x == '/' = q4 (buff++[x]) xs
    | otherwise = deathState xs  -- Error: Unknown token


main :: IO ()
main = do
    putStrLn "Token\t\tType"
    withFile "example.txt" ReadMode $ \fileHandle -> do
        contents <- hGetContents fileHandle
        let linesWithoutSpaces = filter (not . null) (lines contents)
        mapM_ (\line -> q0 [] line) linesWithoutSpaces
    --q0 [] input 
    

    {- How to read a txt file an output its message
    contents <- readFile "example.txt"
    putStrLn contents -}
   
