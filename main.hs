{-
MQL - the iterative query language

@authors:
@Andrei123f(ap4u19@soton.ac.uk/andrei.popabd@gmail.com),
-}
{-# LANGUAGE ScopedTypeVariables #-}


-- custom modules start
import Tokens
import Grammar
import Evaluator
import CsvReader
-- custom modules end

import Data.List
import Control.Monad
import System.IO
import System.IO
import System.Environment
import Control.Exception
import System.Exit
import System.IO.Error
import Text.Read
import Control.DeepSeq


main :: IO()
main = catch main' generalError

main' = do (filename : _ ) <- getArgs
           -- get the file input
           sourceCode :: String <- readFile filename
           -- create tokens
           let tokens :: [Token] = alexScanTokens sourceCode
           -- create parse tree
           let parse :: Lines = parseCalc tokens
           -- decide semantics
           programOutput <- startEvaluation parse
           -- format the output
           let finalString = prettyPrintTable programOutput
           -- print result
           putStrLn finalString

-- the general error function that will just print out the error that we got while lexing/parsing or even doing the semantics.
generalError :: ErrorCall -> IO ()
generalError e =
     do let err = "GENERAL ERROR : " ++ show e
        hPutStr stderr err
        return ()

-- print the pretty table
prettyPrintTable :: Types -> String
prettyPrintTable (TypeTable  [[]] ) = "" -- for when we have null as output
prettyPrintTable (TypeTable []) = "" -- for when we have null as output
prettyPrintTable (TypeTable  table@(firstRow:otherRows))
 | null otherRows =  getRowAsString firstRow
 |otherwise = getRowAsString firstRow ++ "\n" ++ prettyPrintTable(TypeTable  otherRows)
 where
  getRowAsString :: [String] -> String
  getRowAsString row@(currFirstEl : otherEl)
    | null otherEl = currFirstEl
    |otherwise = currFirstEl ++ "," ++ getRowAsString otherEl 






