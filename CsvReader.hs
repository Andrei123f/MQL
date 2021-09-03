module CsvReader(readCsv) where
import Grammar
import Data.Char
import qualified Data.Text as T

type CsvData = IO[[String]]

-- Input : 1,2,3. Expected Output : [1,2,3]
-- Input : 1,,3. ExpectedOutput : [1,,3]
-- because of the second input type we are going to apply "maximum right munch" meaning that we are going to take the string that is as close to the next comma as possible, so we are using foldr.
splitLeftMostByComma  = foldr f [[]]
                            where f c l@(x:xs) | c == ',' = []:l
                                               |otherwise = (c:x):xs


trim :: [String] -> [String]
trim inputRow@(firstEl : otherEl)
  | null otherEl = [reverse (dropWhile isSpace (reverse (dropWhile isSpace firstEl)))]
  |otherwise = reverse (dropWhile isSpace (reverse (dropWhile isSpace firstEl))) : trim otherEl

-- Entry point for the readCsv function
--readCsv :: String -> CsvData
readCsv :: String -> CsvData
readCsv file = 
    do
        content <- readFile file
        let entryLines = Prelude.lines content
        let result = [entry| x<-entryLines, entry <- [splitLeftMostByComma x]]
        let trimmedResults = [finalEntry| currRow <- result, finalEntry <- [trim currRow]]
        return trimmedResults