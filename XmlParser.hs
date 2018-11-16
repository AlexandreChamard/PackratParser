module XmlParser where

import System.IO
import System.Environment
import System.Exit

import MonadicParser
import Control.Applicative

data Node = Node String [(String, String)] [Element] deriving Show

data Element = Element Node | Text String deriving Show

pNode :: Parser Node
pNode = do
    pChar '<'
    (name, attrs) <- pNodeInfo
    core <- (pTry (pString "/>" >> return [])) <|> (pTry $ pChar '>' >> pNodeCore name)
    return $ Node name attrs core
        where
            pNodeInfo = do
                name <- pWord
                pSpaces
                attrs <- pSepBy pAttribute pSpaces
                return $ (name, attrs)
                where
                    pAttribute = do
                        name <- pWord
                        pChar '='
                        value <- do { q <- pOneOf "\"'" ; val <- pManyTill pAnyChar (pChar q) ; return val}
                        return $ (name, value)

            pNodeCore name = pManyTill (pSpaces >> pElement) (pSpaces >> pEndNode name)
                where
                    pEndNode nodeName = pTry $
                        pString "</"
                        >> pString nodeName
                        >> pChar '>'
                        >> return ()

            pElement = (pTry pElement') <|> (pTry pText)
                where
                    pElement' = do { node <- pNode ; return $ Element node }
                    pText = do { str <- pMany $ pSatisfy (/= '<') ; return $ Text str }

input = "<p key='val' x=\"foo\" k=\"\"><a><hr/>hi</a><b>sup</b>hi</p>"