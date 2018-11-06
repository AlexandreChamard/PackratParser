module XmlParser where

import System.IO
import System.Environment
import System.Exit

import MonadicParser

data Balise = Balise {
    b_name :: String,
    b_attributes :: [Attribute]
} deriving Show

data Attribute = Attribute {
    at_name :: String,
    at_value :: String
} deriving Show

pBalise :: Parser Balise
pBalise = pBetween (pChar '<') (pSpaces >> pChar '>') pCoreBalise
    where
        pCoreBalise = do
            name <- pWord
            attrs <- pSepBy pAttribute pSpaces
            return $ Balise name attrs

pAttribute :: Parser Attribute
pAttribute = do
    name <- pWord
    pChar '='
    value <- pBetween (pChar '"') (pChar '"') (pMany $ pSatisfy (/= '"'))
    return $ Attribute name value