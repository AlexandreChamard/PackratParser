module MonadicParser where

import Data.Either

data Parser a = Parser {
    parse :: String -> ((Either String a), String)
}

instance Functor Parser where
    fmap = fmapParser

instance Applicative Parser where
    pure = pureParser
    (<*>) = undefined

instance Monad Parser where
    (>>=) = bindParser
    return = returnParser


fmapParser :: (a -> b) -> Parser a -> Parser b
fmapParser fab elem = do
    res <- elem
    return (fab res)

pureParser :: a -> Parser a
pureParser a = return a

returnParser :: a -> Parser a
returnParser a = Parser $ \str -> (Right a, str)

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser elem fapb = Parser $ \str -> case parse elem str of
    (Right a, str') -> parse (fapb a) str'
    (Left err, str') -> (Left err, str)

pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy f = Parser func
    where
        func "" = (Left "End Of string", "")
        func (c:cs) = if f c then (Right c, cs) else (Left "Bad parser Char", c:cs)

pChar :: Char -> Parser Char
pChar c = pSatisfy (== c)

pOneOf :: String -> Parser Char
pOneOf elems = pSatisfy (flip elem elems)

pNoneOf :: String -> Parser Char
pNoneOf elems = pSatisfy (not . flip elem elems)

pOr :: Parser a -> Parser a -> Parser a
pOr = undefined

-- pMany :: (Char -> Bool) -> Parser [a]
pMany :: Parser a -> Parser [a]
pMany p = do
    a <- p
    b <- pMany p
    return $ a:b

data Bla = Bla Char Char deriving Show

parseBla :: Parser Bla
parseBla = do
    a <- pChar 'a'
    c <- pMany (pChar ' ')
    b <- pOneOf "abcdef"
    return $ Bla a b

-- parseElem :: String -> (Either String a, String)
parseElem str = parse parseBla str