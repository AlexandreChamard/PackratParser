module MonadicParser where

import Data.Char
import Data.Either
import Control.Applicative

data Parser a = Parser {
    parse :: String -> ((Either String a), String)
}

instance Functor Parser where
    fmap = fmapParser

instance Applicative Parser where
    pure = pureParser
    (<*>) = applicateParser

instance Alternative Parser where
    empty = emptyParser
    (<|>) = opOrParser

instance Monad Parser where
    (>>=) = bindParser
    return = returnParser


fmapParser :: (a -> b) -> Parser a -> Parser b
fmapParser fab elem = do
    res <- elem
    return (fab res)

pureParser :: a -> Parser a
pureParser a = return a

applicateParser :: Parser (a -> b) -> Parser a -> Parser b
applicateParser mfab ma = do
    fab <- mfab
    a <- ma
    return (fab a)

emptyParser :: Parser a
emptyParser = Parser $ \str -> (Left "empty Parser", str)

opOrParser :: Parser a -> Parser a -> Parser a
opOrParser l r = Parser $ \str -> case parse l str of
    (Right a, b) -> (Right a, b)
    _ -> parse r str

returnParser :: a -> Parser a
returnParser a = Parser $ \str -> (Right a, str)

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser elem fapb = Parser $ \str -> case parse elem str of
    (Right a, str') -> parse (fapb a) str'
    (Left err, str') -> (Left err, str)

-- verifie si le char parsé est valide ou non
pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy f = Parser func
    where
        func "" = (Left "End Of string", "")
        func (c:cs) = if f c then (Right c, cs) else (Left "Bad parser Char", c:cs)

-- renvoie le premier Parser valide
pChoice :: [Parser a] -> Parser a
pChoice ps = foldr (<|>) emptyParser ps

-- renvoie un tableau de n elements parsés
pCount :: Int -> Parser a -> Parser [a]
pCount n p
    | n <= 0    = return []
    | otherwise = sequence (replicate n p)

pBetween :: Parser open -> Parser close -> Parser a -> Parser a
pBetween open close p = do
    _ <- open
    p' <- p
    _ <- close
    return p'

pSepBy :: Parser a -> Parser sep -> Parser [a]
pSepBy p sep = pOr (pSepBy1 p sep) (return [])

pSepBy1 :: Parser a -> Parser sep -> Parser [a]
pSepBy1 p sep = do
    x <- p
    xs <- many (sep >> p)
    return (x:xs)

-- parse un char donné
pChar :: Char -> Parser Char
pChar c = pSatisfy (== c)

-- parse une string donnée
pString :: String -> Parser String
pString str = sequence [pChar x | x <- str]

-- parse un char qui appartient à la chaine donnée
pOneOf :: String -> Parser Char
pOneOf elems = pSatisfy (flip elem elems)

pNoneOf :: String -> Parser Char
pNoneOf elems = pSatisfy (not . flip elem elems)

pOr :: Parser a -> Parser a -> Parser a
pOr a b = a <|> b

-- renvoie un tableau de n elements parsés (ne peut fail)
pMany :: Parser a -> Parser [a]
pMany p = many p

pMany1 :: Parser a -> Parser [a]
pMany1 p = do
    x <- p
    xs <- many p
    return $ x:xs

--

pSpaces :: Parser [Char]
pSpaces = pMany (pSatisfy isSpace)

pDigit :: Parser Char
pDigit = pOneOf "0123456789"

data Bla = Bla Char Char deriving Show

parseBla :: Parser Bla
parseBla = do
    a <- pChar 'a'
    _ <- pCount 3 (pChar ' ')
    b <- pOneOf "abcdef"
    return $ Bla a b

-- parseElem :: String -> (Either String a, String)
parseElem str = parse (pCount 4 (do {_ <- pMany (pChar ' '); a <- pMany (pNoneOf " ") ; return a})) str
-- parseElem str = parse parseBla str