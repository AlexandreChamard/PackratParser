module Main where

import System.IO
import System.Environment
import System.Exit

import MonadicParser
import XmlParser


main :: IO ()
main = do
    args <- getArgs
    if length args == 1 then
        print $ parse pBalise (args !! 0)
    else
        putStrLn "Bad number of arguments."
