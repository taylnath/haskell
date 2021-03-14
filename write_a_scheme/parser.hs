import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseChar :: Parser Char
parseChar = do
    char <- (noneOf "\"")
    case char of
        '\\' -> do
            char <- anyChar
            case char of 
                '"' -> return char
                'n' -> return char
                'r' -> return char
                't' -> return char
                '\\' -> return char
                _ -> unexpected $ "\\" ++ [char]
        _   -> return char

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many parseChar
    -- x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    s <- many1 digit
    return $ (Number . read) s
-- other ways to do parseNumber:
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = many1 digit >>= \s -> return $ (Number. read) s

parseExpr :: Parser LispVal
parseExpr = parseAtom 
    <|> parseString 
    <|> parseNumber
    <|> parseQuoted
    <|> do 
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x
-- parseExpr = parseAtom <|> parseNumber

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found "  ++ show val

spaces :: Parser ()
spaces = skipMany1 space

main = do
    -- s <- getLine
    -- putStrLn . readExpr
    -- putStrLn . parseString
    getLine >>= putStrLn . readExpr
    main


