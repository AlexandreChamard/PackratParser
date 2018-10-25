module MonadicParser where

import Data.Char
import Data.Either
import Data.Maybe
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
emptyParser = Parser $ \str -> (Left "empty", str)

opOrParser :: Parser a -> Parser a -> Parser a
opOrParser l r = Parser $ \str -> case parse l str of
    (Right a, b) -> (Right a, b)
    _ -> parse r str

opOnErrParser :: Parser a -> String -> Parser a
opOnErrParser p err = Parser $ \str -> case parse p str of
        (Left e, _) -> (Left err, str)
        parsed -> parsed

(<?>) :: Parser a -> String -> Parser a
(<?>) = opOnErrParser

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

-- parse un element qui se trouve entre deux autres elements
pBetween :: Parser open -> Parser close -> Parser a -> Parser a
pBetween open close p = do
    _ <- open
    p' <- p
    _ <- close
    return p'

-- renvoie l'emement parser si réussi sinon renvoie l'element par default
pOption :: a -> Parser a -> Parser a
pOption x p = p <|> (return x)

-- renvoie Just + l'emement parser si réussi sinon renvoie Nothing
pOptionMaybe :: Parser a -> Parser (Maybe a)
pOptionMaybe p = pOption Nothing (liftA Just p)


pOptional :: Parser a -> Parser ()
pOptional p = (do { _ <- p ; return () }) <|> (return ())

-- renvoie un tableau de n elements parsés (ne peut fail)
pMany :: Parser a -> Parser [a]
pMany p = many p

-- renvoie un tableau de d'au moins 1 element parsé
pMany1 :: Parser a -> Parser [a]
pMany1 p = do
    x <- p
    xs <- many p
    return $ x:xs


pSkipMany :: Parser a -> Parser ()
pSkipMany p = (do { _ <- pMany p ; return () }) <|> (return ())


pSkipMany1 :: Parser a -> Parser ()
pSkipMany1 p = do
    _ <- p
    _ <- pSkipMany p
    return ()


pSepBy :: Parser a -> Parser sep -> Parser [a]
pSepBy p sep = (pSepBy1 p sep) <|> (return [])


pSepBy1 :: Parser a -> Parser sep -> Parser [a]
pSepBy1 p sep = do
    x <- p
    xs <- pMany (sep >> p)
    return (x:xs)


pEndBy :: Parser a -> Parser end -> Parser [a]
pEndBy p pend = pMany (do
    x <- p
    _ <- pend
    return x)


pEndBy1 :: Parser a -> Parser end -> Parser [a]
pEndBy1 p pend = pMany1 (do
    x <- p
    _ <- pend
    return x)


pSepEndBy :: Parser a -> Parser end -> Parser [a]
pSepEndBy p pend = (pSepEndBy1 p pend) <|> (return [])


pSepEndBy1 :: Parser a -> Parser end -> Parser [a]
pSepEndBy1 p pend = do
    x <- p
    (do ; _ <- pend ; xs <- pSepEndBy p pend ; return (x:xs)) <|> (return [x])


-- parse un char donné
pChar :: Char -> Parser Char
pChar c = pSatisfy (== c)

-- parse une string donnée
pString :: String -> Parser String
pString str = sequence [pChar x | x <- str]

-- parse un char qui appartient à la String donnée
pOneOf :: String -> Parser Char
pOneOf elems = pSatisfy (flip elem elems)

-- parse un char qui n'appartient pas à la String donnée
pNoneOf :: String -> Parser Char
pNoneOf elems = pSatisfy (not . flip elem elems)

--

pSpaces :: Parser ()
pSpaces = pSkipMany (pSatisfy isSpace)

pDigit :: Parser Char
pDigit = pOneOf "0123456789"

pNumber :: Parser Int
pNumber = do
    neg <- pOptionMaybe (pChar '-')
    n <- pUNumber
    case neg of
        Just _ -> return (-n)
        Nothing -> return n

pUNumber :: Parser Int
pUNumber = do
    digits <- pMany1 pDigit
    return (read digits :: Int)

pNumber' :: Parser Int
pNumber' = do
    neg <- pMany (pOneOf "+-")
    n <- pUNumber
    if odd $ foldl (\sum e -> sum + (if e == '-' then 1 else 0)) 0 neg
    then
        return (-n)
    else
        return n

pFloat :: Parser Float
pFloat = do
    n <- pNumber
    f <- pOption 0 (do
        _ <- pChar '.'
        f' <- pUNumber
        return f')
    return (read (show n ++ "." ++ show f) :: Float)

pFloat' :: Parser Float
pFloat' = do
    n <- pNumber'
    f <- pOption 0 (do
        _ <- pChar '.'
        f' <- pUNumber
        return f')
    return (read (show n ++ "." ++ show f) :: Float)


data Bla = Bla Char Char deriving Show

parseBla :: Parser Bla
parseBla = do
    a <- pChar 'a'
    _ <- pCount 3 (pChar ' ')
    b <- pOneOf "abcdef"
    return $ Bla a b