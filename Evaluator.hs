{-
MQL evaluator 
The evaluator will check:
  - syntax errors
  - type errors
  - semantics of the program

@authors:
 @Andrei123f(ap4u19@soton.ac.uk/andrei.popabd@gmail.com)
 @cat1g19 
 @Carloscb407
-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Evaluator where

-- import custom modules
import Grammar
import CsvReader

-- import haskell modules
import Data.List
import Data.Char
import Text.Read


-- Types of MQL
data Types = TypeInt Int
 | TypeBool Bool
 | TypeFloat Float
 | TypeString String
 | TypeTable [[String]]
 | TypeRow [String]deriving (Show,Eq)

-- The variable environment of MQL
{-
e.g if we would have
table A;
on the Environment we would push the tuple (TypeTable, "A")
-}
type Environment = [(Types,String)]

-- The State of each MQL line of code
type State = (Lines,[Lines],Environment)


-- helper functions define start 

-- takes the output from the happy parsing and converts it into an array of instructions for the evaluator to follow/check.
createListOfInstructions :: Lines -> [Lines]
createListOfInstructions (LinesStream lines1 lines2) = createListOfInstructions lines1 ++ createListOfInstructions lines2
createListOfInstructions lines1 = [lines1]

finalise :: State -> IO Types
finalise ((PrintTable varName), kontinuation, env )
  = do
    return var
    where
         var = fst (getVarfromEnv varName env)


-- this will get the variable from our current environment
-- e.g. if we would have something like int a = 5;
-- and we would call this it would give us the tuple that contains the type of the variable and also the variable name
getVarfromEnv :: String -> Environment -> (Types,String)
getVarfromEnv varName env = head results
    where 
         results = filter (\x -> snd x == varName ) env

-- this will just check if we have that certain variable in the environement
-- similar to getVarfromEnv, but it just returns true or false
isVarInEnv :: String -> Environment -> Bool
isVarInEnv varName env = length results == 0
    where 
         results = filter (\x -> snd x == varName ) env



-- checking if we are trying to declare the same variable twice.
{-
e.g. we would not have something like this :
table A;
table A;
-}
checkIfVarExists :: String -> Environment -> Bool
checkIfVarExists varName (var:vars) = snd var == varName || checkIfVarExists varName vars
checkIfVarExists _ [] = False

-- CALCULATE VALUES START
{-
Current values that need to be calculated for MQL: 
1) float numbers;
2) integer numbers;
3) table values;
4) row values;
5) boolean values;
6) string values;

-}




-- CALCULATE FLOAT VALUE
calculateFloat :: Line -> Environment -> Float
calculateFloat (Int a) env = fromIntegral a
calculateFloat (Float a) env = a
calculateFloat (Negate a) env = (-1) * calculateFloat a env
calculateFloat (Plus a b) env = calculateFloat a env + calculateFloat b env
calculateFloat (Minus a b) env = calculateFloat a env - calculateFloat b env
calculateFloat (Times a b) env = calculateFloat a env * calculateFloat b env
calculateFloat (Div a b) env = calculateFloat a env / calculateFloat b env
calculateFloat (Mod a b) env = calculateFloat a env / calculateFloat b env

{-
float a = 5.5;
float b = a; <- this case
-}
calculateFloat (Var varName) env = value where
  (TypeFloat value) = getVariableValue (TypeFloat 0) varName env

{-
float a;
row aRow;
string aEl;
aEl = aRow.getElement(0);
a = aEl.getFloat(); <- this case
-}
calculateFloat (GetFloat varname) env = floatFloat
  where
      stringFloat = calculateString (Var varname) env
      floatFloat = read stringFloat :: Float
calculateFloat _ _ = error "ArithmeticException : wrong operator used in float arithmetic operation."



-- CALCULATE int value
-- MQL supports basic arithmentic operations
calculateInt :: Line -> Environment -> Int
calculateInt (Int a) env = a
calculateInt (Negate a) env = (-1) * calculateInt a env
calculateInt (Plus a b) env = calculateInt a env + calculateInt b env
calculateInt (Minus a b) env = calculateInt a env - calculateInt b env
calculateInt (Times a b) env = calculateInt a env * calculateInt b env
calculateInt (Div a b) env = calculateInt a env `div` calculateInt b env
calculateInt (Mod a b) env = calculateInt a env `mod` calculateInt b env

{-
int a = 5;
int b = a; <- this case
-}
calculateInt (Var varName) env = value 
 where
      (TypeInt value) = getVariableValue (TypeInt 0) varName env
{-
table A = getTable A;
int a = A.tableLength(); <- this case
-}
calculateInt (TableLength varName) env = value
 where
      value = lenTable var
      var = fst $ getVarfromEnv varName env

{-
int a;
row aRow;
string aEl;
aEl = aRow.getElement(0);
a = aEl.getInt(); <- this case
-}
calculateInt (GetInt varname) env = intInt
  where
      stringInt = calculateString (Var varname) env
      intInt = read stringInt :: Int


calculateInt _ _ = error "ArithmeticException : wrong operator used in int operation."



calculateLiteralString :: Line -> Environment -> String
calculateLiteralString (Int value) env = show value
calculateLiteralString (Float value) env = show value
calculateLiteralString (Var varName) env = varName

calculateLiteralString _ _ = error "ArithmeticException : wrong operator used in literal string assignment."



-- MQL supports only string assignment at the moment.
calculateString :: Line -> Environment -> String
calculateString (Var varName) env = value
  where
  (TypeString value) = getVariableValue (TypeString "") varName env

calculateString (Int value) env = show value

calculateString (Float value) env = show value

calculateString (GetElement varName index) env = value
 where
    value = getFromRow var (calculateInt index env)
    var = getVariableValue (TypeRow []) varName env
calculateString _ _ = error "ArithmeticException : wrong operator used in string assignment."

-- CALCULATE boolean value
calculateBool :: Line -> Environment -> Bool
calculateBool (TTrue) _ = True
calculateBool (TFalse) _ = False
calculateBool (Equals exp1 exp2) env = (calculateBool exp1 env) == (calculateBool exp2 env)
calculateBool (NotEquals exp1 exp2) env = (calculateBool exp1 env) /= (calculateBool exp2 env)
calculateBool (Or exp1 exp2) env = (calculateBool exp1 env) || (calculateBool exp2 env)
calculateBool (And exp1 exp2) env = (calculateBool exp1 env) && (calculateBool exp2 env)
calculateBool (Not exp1) env = not (calculateBool exp1 env)
calculateBool (IntEquals exp1 exp2) env = (calculateInt exp1 env) == (calculateInt exp2 env)
calculateBool (StringEquals exp1 exp2) env = (calculateString exp1 env) == (calculateString exp2 env)
calculateBool (GreaterEquals exp1 exp2) env = (calculateInt exp1 env) >= (calculateInt exp2 env)
calculateBool (LessEquals exp1 exp2) env = (calculateInt exp1 env) <= (calculateInt exp2 env)
calculateBool (Greater exp1 exp2) env = (calculateInt exp1 env) > (calculateInt exp2 env)
calculateBool (Less exp1 exp2) env = (calculateInt exp1 env) < (calculateInt exp2 env)


calculateBool (IsInt varName) env = result
  where
      exp1 = calculateString (Var varName) env
      result = checkIfIsInt exp1

calculateBool (IsFloat varName) env = result && result2
  where
      exp1 = calculateString (Var varName) env
      result = checkIfFloat exp1
      result2 = '.' `elem` exp1

calculateBool (IsString varName) env = not(resultInt || resultFloat)
  where
      exp1 = calculateString (Var varName) env
      resultInt = checkIfIsInt exp1
      resultFloat = checkIfFloat exp1 && '.' `elem` exp1


calculateBool (Var varName) env = value
 where
    (TypeBool value) = getVariableValue (TypeBool False) varName env



calculateBool _ _ = error "ArithmeticException : Wrong operator used in boolean expressions "

-- this function just checks if a string is an integer or not
checkIfIsInt :: String -> Bool
checkIfIsInt valueToTest =
  case (readMaybe valueToTest :: Maybe Int) of
    Just value -> True
    Nothing -> False

-- this function just checks if a string is a float or not
checkIfFloat :: String -> Bool
checkIfFloat valueToTest =
  case (readMaybe valueToTest :: Maybe Float) of
    Just value -> True
    Nothing -> False

{- --OLD CHECK IF INT FUNCTION 
checkIfIsInt :: String -> Bool
checkIfIsInt str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False
-}

-- CALCULATE row value
calculateRow :: Line -> Environment -> [String]
calculateRow (Var varName) env = value
    where
        (TypeRow value) = getVariableValue (TypeRow []) varName env
calculateRow (GetRow varName index) env = value
    where
        value = getFromTable table (calculateInt index env)
        table = getVariableValue (TypeTable []) varName env
-- TODO FOR FUTURE : MAYBE IMPLEMENT SOMETHING LIKE row = ["5","4,"] etc
calculateRow _ _ = error "ArithmeticException : Wrong operator uused in row expressions."


-- CALCULATE table value
calculateTable :: Line -> Environment -> IO[[String]]
calculateTable (Var varName) env = return value
  where
    (TypeTable value) = getVariableValue (TypeTable []) varName env
calculateTable (GetTable fileName) env =
    do
    readCsv csvName
  where
      csvName = fileName ++ ".csv"


calculateTable _ _ = error "ArithmeticException : Wrong operator used in table expression."
-- CALCULATE VALUES END

getFromTable :: Types -> Int -> [String]
getFromTable (TypeTable xss)  i = item
    where
        item | i < 0 = error "TableOutOfBoundsException : Used negative number when trying to get a row from table."
             | i >= (length xss) = error customMessage
             |otherwise  = xss !! i
             where
               customMessage = "TableOutOfBoundsException : Used number greater than the table size when trying to get a row from table. Index requested: " ++ show i ++  " Table : " ++ show xss ++ " Table length " ++ show(length xss)            

getFromTable _ _ = error "ArithmethicException : Tried getting a row variable from a variable that is not a table."

-- for getting the binded value from environment
getVariableValue :: Types -> String -> Environment -> Types
getVariableValue (TypeInt _) varName env = checkedVar 
  where
     checkedVar = checkType var (TypeInt 0)
     var = fst $ getVarfromEnv varName env
getVariableValue (TypeBool _) varName env = checkedVar 
  where
     checkedVar = checkType var (TypeBool False)
     var = fst $ getVarfromEnv varName env

getVariableValue (TypeRow _) varName env = checkedVar
 where
    checkedVar = checkType var (TypeRow [])
    var = fst $ getVarfromEnv varName env

getVariableValue (TypeTable _) varName env = checkedVar
 where
    checkedVar = checkType var (TypeTable [])
    var = fst $ getVarfromEnv varName env

getVariableValue (TypeString _) varName env = checkedVar
  where
    checkedVar = checkType var (TypeString "")
    var = fst $ getVarfromEnv varName env


getVariableValue (TypeFloat _) varName env = checkedVar
  where
    checkedVar = checkType var (TypeFloat 0)
    var = fst $ getVarfromEnv varName env

-- type checker 
checkType :: Types -> Types -> Types

-- checking type of int
checkType var@(TypeInt _) (TypeInt _) = var
checkType (TypeBool _) (TypeInt _) = error "TypeCheckException : Used boolean variable where int was expected"
checkType (TypeTable _) (TypeInt _) = error "TypeCheckException : Used table variable where int was expected"
checkType (TypeRow _) (TypeInt _) = error "TypeCheckException : Used row variable where int was expected"
checkType (TypeFloat _) (TypeInt _) = error "TypeCheckException : Used float variable where int was expected"


-- checking type of boolean
checkType var@(TypeBool _) (TypeBool _) = var
checkType (TypeInt _) (TypeBool _) = error "TypeCheckException : Used int variable where boolean was expected"
checkType (TypeTable _) (TypeBool _) = error "TypeCheckException : Used table variable where boolean was expected"
checkType (TypeRow _) (TypeBool _) = error "TypeCheckException : Used row variable where boolean was expected"
checkType (TypeFloat _) (TypeBool _) = error "TypeCheckException : Used float variable where boolean was expected"

-- checking type of table
checkType var@(TypeTable _) (TypeTable _) = var
checkType (TypeInt _) (TypeTable _) = error "TypeCheckException : Used int variable where table was expected"
checkType (TypeBool _) (TypeTable _) = error "TypeCheckException : Used boolean variable where table was expected"
checkType (TypeRow _) (TypeTable _) = error "TypeCheckException : Used row variable where table was expected"
checkType (TypeFloat _) (TypeTable _) = error "TypeCheckException : Used float variable where table was expected"

-- checking type of row
checkType var@(TypeRow _) (TypeRow _) = var
checkType (TypeInt _) (TypeRow _) = error "TypeCheckException : Used int variable where row was expected"
checkType (TypeBool _) (TypeRow _) = error "TypeCheckException : Used boolean variable where row was expected"
checkType (TypeTable _) (TypeRow _) = error "TypeCheckException : Used table variable where row was expected"
checkType (TypeFloat _) (TypeRow _) = error "TypeCheckException : Used float variable where row was expected"

-- checking type of string
checkType var@(TypeString _) (TypeString _) = var
checkType (TypeInt _) (TypeString _) = error "TypeCheckException : Used int variable where string was expected"
checkType (TypeBool _) (TypeString _) = error "TypeCheckException : Used boolean variable where string was expected"
checkType (TypeTable _) (TypeString _) = error "TypeCheckException : Used table variable where string was expected"
checkType (TypeFloat _) (TypeString _) = error "TypeCheckException : Used float variable where string was expected"

-- checking type of float
checkType var@(TypeFloat _) (TypeFloat _) = var
checkType (TypeInt _) (TypeFloat _) = error "TypeCheckException : Used int variable where float was expected"
checkType (TypeBool _) (TypeFloat _) = error "TypeCheckException : Used boolean variable where float was expected"
checkType (TypeTable _) (TypeFloat _) = error "TypeCheckException : Used table variable where float was expected"
checkType (TypeRow _) (TypeFloat _) = error "TypeCheckException : Used row variable where float was expected"

-- base case(we should really don't get this far but just for safety :) )
checkType _ _ = error "TypeCheckException : Unknown data type"

lenTable :: Types -> Int
lenTable (TypeTable xs)
  | xs == [[]] = 0
  |otherwise = length xs
lenTable _ = error "ArithmeticException : Tried to get the length of a variable that is not a Table"

-- get element from row by index
getFromRow :: Types -> Int -> String
getFromRow (TypeRow xs) i = item where
  item  | i < 0 = error "RowOutOfBoundsException : Used negative number when trying to get an element from row."
        | i >= (length xs) = error customMessage
        | otherwise = xs!!i
        where
          customMessage = "RowOutOfBoundsException : Used number greater than the row size when trying to get an element from row. Index requsted : " ++ show i ++ " Row : " ++ show  xs ++ " Row length : " ++ show(length xs)

getFromRow _ _ = error "ArithmethicException : Tried getting a string variable from a variable that is not a row"



-- convertToString
convertToString :: Types -> String -> Environment -> String
convertToString (TypeInt intVal) varName env = intValConvertedToString
  where
    intValToConvert = calculateInt(Var varName) env
    intValConvertedToString = show intValToConvert

convertToString (TypeString stringVal) varName env = stringVal
  where
    stringVal = calculateString(Var varName) env

convertToString (TypeFloat stringVal) varName env = floatValToConvertedToString
  where
    (TypeFloat value) = getVariableValue (TypeFloat 0) varName env
    floatValToConvertedToString = show value


-- calculate new value
calculateNewValue :: Types -> Line -> Environment -> IO Types
calculateNewValue (TypeInt _) expr env = return (TypeInt calcValue) 
 where
    calcValue = calculateInt expr env
calculateNewValue (TypeBool _) expr env = return (TypeBool calcValue) 
 where
    calcValue = calculateBool expr env

calculateNewValue (TypeRow _) expr env = return (TypeRow calcValue) 
 where
    calcValue = calculateRow expr env


calculateNewValue (TypeTable _) expr env =
    do
    calcValue <- calculateTable expr env
    return (TypeTable calcValue)

calculateNewValue (TypeString _) expr env =
    do
    let calcValue = calculateString expr env
    return (TypeString calcValue)


calculateNewValue (TypeFloat _) expr env =
    do
    let calcValue = calculateFloat expr env
    return (TypeFloat calcValue)


calculateNewValue _ _ _ = error "TypeCheckException : Unknown type"

-- remove var from env
removeVarFromEnv :: (Types,String) -> Environment -> Environment
removeVarFromEnv variable2Find (var:vars) | var == variable2Find = vars
                                          | otherwise = (var: (removeVarFromEnv variable2Find vars) )
removeVarFromEnv _ [] = []

-- sort lexico graphically
sortAsc :: Types -> String -> Environment -> IO Types
sortAsc (TypeTable xss) varName env = do
    table <- calculateTable (Var varName) env
    let sortedTable = sort table;
    return (TypeTable sortedTable)

-- sortc desc lexico graphically
sortDesc :: Types -> String -> Environment -> IO Types
sortDesc (TypeTable xss) varName env = do
    table <- calculateTable (Var varName) env
    -- sort asc first
    let sortedTable = sort table;
    -- then just reverse the list
    let descSortedTable = foldl (\x y -> y:x) [] sortedTable
    return (TypeTable descSortedTable)




-- put row into table.
putRow :: Types -> Line -> Environment -> IO Types
putRow (TypeTable xss) expr env = do
    let xs = calculateRow expr env
    return (TypeTable (xss ++ [xs]))
putRow _ _ _ = error "ArithmethicException : Tried to append to something that is not a Table."


-- merge the rows
mergeRow :: Types -> Line -> Environment -> IO Types
mergeRow (TypeRow xss) expr env =
  do
   let xs = calculateRow expr env
   return (TypeRow (xss ++ xs))

mergeRow _ _ _ = error "ArithmethicException : Tried to merge something that is not a Row."


-- put element into row.
putElement :: Types -> Line -> Environment -> Types
putElement (TypeRow xss) expr env = (TypeRow (xss ++ [xs]) ) 
 where
    xs = calculateString expr env

putElement _ _ _ = error "ArithmethicException : Tried to put an element into a variable that is not a Row.";

-- get element from row.
getElement :: Types -> Line -> Environment -> String
getElement (TypeRow xss) expr env = undefined

-- helper functions define end

-- The entry point of our evaluator
startEvaluation :: Lines -> IO Types
startEvaluation parsedLines =
    do
    programFinal <- evaluate initialSate
    finalise programFinal

 where
    initialSate = (head linesArray, tail linesArray, [])
    linesArray = createListOfInstructions parsedLines


-- this will be the master function that will do all the evaluation of our MQL language.
evaluate :: State -> IO State
-- TYPE DECLARATIONS START

-- TypeInt no assign. e.g. int a;
evaluate ( (IntDeclare varName), kontinuation, env )
 | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
 | checkIfVarExists varName env = error ("VariableExistenceException : int variable: " ++ varName ++ " already exists")
 |otherwise = evaluate nextState
 where
    nextState = (head kontinuation, tail kontinuation, env++[( (TypeInt 0) ,varName)])

-- TypeFloat no assign. e.g. float a;
evaluate ( (FloatDeclare varName), kontinuation, env )
 | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
 | checkIfVarExists varName env = error ("VariableExistenceException : float variable: " ++ varName ++ " already exists")
 |otherwise = evaluate nextState
 where
    nextState = (head kontinuation, tail kontinuation, env++[( (TypeFloat 0) ,varName)])



-- TypeString no assign. e.g. string a;
evaluate ( (StringDeclare varName), kontinuation, env )
 | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
 | checkIfVarExists varName env = error ("VariableExistenceException : string variable: " ++ varName ++ " already exists")
 |otherwise = evaluate nextState
 where
    nextState = (head kontinuation, tail kontinuation, env++[( (TypeString "") ,varName)])

-- TypeBool no assign. e.g. boolean a;
evaluate ( (BoolDeclare varName), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | checkIfVarExists varName env = error ("VariableExistenceException : boolean variable: " ++ varName ++ " already exists")
    |otherwise = evaluate nextState
    where
        nextState = (head kontinuation, tail kontinuation, env++[( (TypeBool False),varName) ])

-- TypeRow no assign. e.g. row a;
evaluate ( (RowDeclare varName), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | checkIfVarExists varName env = error ("VariableExistenceException : row variable: " ++ varName ++ " already exists")
    |otherwise = evaluate nextState
    where
        nextState = (head kontinuation, tail kontinuation, env++[( (TypeRow []),varName) ])

-- TypeTable no assign. e.g. table a;
evaluate ( (TableDeclare varName), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | checkIfVarExists varName env = error ("VariableExistenceException : table variable: " ++ varName ++ " already exists")
    |otherwise = evaluate nextState
    where
        nextState = (head kontinuation, tail kontinuation, env++[( (TypeTable []),varName) ]) -- todo check if you need TypeTable [[]] or TypeTable []
-- ===============================================================================================================================

-- TypeInt with assign. e.g. int a = 5; | int a = 5 + 3; | int a = b + 3;
evaluate ( (IntDeclareAssign varName value), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | checkIfVarExists varName env = error ("VariableExistenceException : int variable: " ++ varName ++ " already exists")
    |otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeInt calculatedValue) ,varName)] )
          calculatedValue = calculateInt value env


-- TypeFloat with assign. e.g. float a = 5.5; | float a = 5.5 + 3.5; | float a = b + 5.6;
evaluate ( (FloatDeclareAssign varName value), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | checkIfVarExists varName env = error ("VariableExistenceException : float variable: " ++ varName ++ " already exists")
    |otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeFloat calculatedValue) ,varName)] )
          calculatedValue = calculateFloat value env


-- TypeString with assign. e.g. string s = 5;
evaluate ( (StringDeclareAssign varName value), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | checkIfVarExists varName env = error ("VariableExistenceException : string variable: " ++ varName ++ " already exists")
    |otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeString calculatedValue) ,varName)] )
          calculatedValue = calculateString value env

-- TypeString with assign. e.g. string s = "545454asdasdsda";
evaluate ( (StringLiteralDeclareAssign varName value), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | checkIfVarExists varName env = error ("VariableExistenceException : string variable: " ++ varName ++ " already exists")
    |otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeString calculatedValue) ,varName)] )
          calculatedValue = calculateLiteralString value env


-- TypeBool with assign. e.g. boolean a = false; | boolean a = 4 < 5; | boolean a = b < 3;
evaluate ( (BoolDeclareAssign varName value), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | checkIfVarExists varName env = error ("VariableExistenceException : boolean variable: " ++ varName ++ "already exists")
    |otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeBool calculatedValue) ,varName)] )
          calculatedValue = calculateBool value env

-- TypeRow with asssign. e.g. row a = ['a', 'b'] | row a = A.getRow(5)
evaluate ( (RowDeclareAssign varName value), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | checkIfVarExists varName env = error ("VariableExistenceException : row variable: " ++ varName ++ "already exists")
    |otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, env++[( (TypeRow calculatedValue) ,varName)] )
          calculatedValue = calculateRow value env



-- TypeTable with assign. e.g table A = getFromTable A;
evaluate ( (TableDeclareAssign varName value), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table"
    | checkIfVarExists varName env = error ("VariableExistenceException : " ++ varName ++ "already exists")
    |otherwise =
        do
        calculatedValue <- calculateTable value env
        let nextState = (head kontinuation, tail kontinuation, env++[( (TypeTable calculatedValue) ,varName)] )
        evaluate nextState

-- VarAssign String Line
evaluate ( (VarAssign varName value), kontinuation, env )
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | not (checkIfVarExists varName env) = error ("VariableExistenceException : variable " ++ varName ++ " does not exist")
    |otherwise =
        do
        calculatedValue <- calculateNewValue varType value env
        let newEnv = newishEnv ++ [(calculatedValue,varName)];
        let nextState = (head kontinuation, tail kontinuation, newEnv );
        evaluate nextState
    where
          calculatedValue = calculateNewValue varType value env
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env
          varFromEnv = getVarfromEnv varName env       
-- TYPE DECLARATIONS END

-- FUNCTIONS DECLARATIONS START
-- function putRow. Syntax : A.putRow(c) where A is a table and c is a row
evaluate ((PutRow varName value), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | not (checkIfVarExists varName env) = error ("VariableExistenceException : Row with name: " ++ varName ++ " does not exist")
    |otherwise =
        do
         calculatedValue <- putRow varType value env
         let newEnv = newishEnv ++ [(calculatedValue,varName)];
         let nextState = (head kontinuation, tail kontinuation, newEnv );
         evaluate nextState
    where
          calculatedValue = putRow varType value env
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env
          varFromEnv = getVarfromEnv varName env       


-- function putRow. Syntax : A.putRow(c) where A is a table and c is a row
evaluate ((MergeRow varName value), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | not (checkIfVarExists varName env) = error ("VariableExistenceException : Row with name: " ++ varName ++ " does not exist")
    |otherwise =
        do
         calculatedValue <- mergeRow varType value env
         let newEnv = newishEnv ++ [(calculatedValue,varName)];
         let nextState = (head kontinuation, tail kontinuation, newEnv );
         evaluate nextState
    where
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env
          varFromEnv = getVarfromEnv varName env        

evaluate ((ResetRow varName), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | not (checkIfVarExists varName env) = error ("VariableExistenceException : Row with name: " ++ varName ++ " does not exist")
    |otherwise =
        do
         let calulatedValue = TypeRow []
         let newEnv = newishEnv ++ [(calulatedValue,varName)];
         let nextState = (head kontinuation, tail kontinuation, newEnv );
         evaluate nextState
    where
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env
          varFromEnv = getVarfromEnv varName env        

evaluate ((ToString varName), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | not (checkIfVarExists varName env) = error ("VariableExistenceException : String with name: " ++ varName ++ " does not exist")
    |otherwise =
        do
         let stringValue = convertToString varType varName env
         let calulatedValue = TypeString stringValue
         let newEnv = newishEnv ++ [(calulatedValue,varName)];
         let nextState = (head kontinuation, tail kontinuation, newEnv );
         evaluate nextState
    where
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env
          varFromEnv = getVarfromEnv varName env        

evaluate ((AscSort varName), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | not (checkIfVarExists varName env) = error ("VariableExistenceException : Table with name: " ++ varName ++ " does not exist")
    |otherwise =
        do
         calulatedValue <- sortAsc varType varName env
         let newEnv = newishEnv ++ [(calulatedValue,varName)];
         let nextState = (head kontinuation, tail kontinuation, newEnv );
         evaluate nextState
    where
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env
          varFromEnv = getVarfromEnv varName env        
evaluate ((DescSort varName), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | not (checkIfVarExists varName env) = error ("VariableExistenceException : Table with name: " ++ varName ++ " does not exist")
    |otherwise =
        do
         calulatedValue <- sortDesc varType varName env
         let newEnv = newishEnv ++ [(calulatedValue,varName)];
         let nextState = (head kontinuation, tail kontinuation, newEnv );
         evaluate nextState
    where
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env
          varFromEnv = getVarfromEnv varName env        




-- function putElement. Syntax : a.putElement(j) where a is a row and j is a string
evaluate ((PutElement varName value), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | not (checkIfVarExists varName env) = error ("VariableExistenceException : Row with name: " ++ varName ++ " does not exist")
    |otherwise = evaluate nextState
    where nextState = (head kontinuation, tail kontinuation, newEnv )
          newEnv = newishEnv ++ [(calculatedValue,varName)]
          calculatedValue = putElement varType value env
          varType = fst varFromEnv
          newishEnv = removeVarFromEnv varFromEnv env
          varFromEnv = getVarfromEnv varName env        

-- FUNCTIONS DECLARATIONS END

-- LOGIC PARTS START

-- If Statement
evaluate ((If condition lines), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | resultOfCheck == True = evaluate ( head (createListOfInstructions lines), tail (createListOfInstructions lines) ++ kontinuation, env )
    |otherwise = evaluate (head kontinuation, tail kontinuation, env )
    where
      resultOfCheck = calculateBool condition env

-- IfElse Statement
evaluate ((IfElse condition thenLines elseLines), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | calculateBool condition env = evaluate ( head (createListOfInstructions thenLines), tail (createListOfInstructions thenLines) ++ kontinuation, env )
    |otherwise = evaluate ( head (createListOfInstructions elseLines), tail (createListOfInstructions elseLines) ++ kontinuation, env )

-- While Loop
evaluate ((Loop condition lines), kontinuation, env)
    | kontinuation == [] = error "OutputException : the last line of the program must be printing the output Table."
    | calculateBool condition env = evaluate (head (createListOfInstructions lines), tail (createListOfInstructions lines) ++ [(Loop condition lines)] ++ kontinuation  , env )
    |otherwise = evaluate (head kontinuation, tail kontinuation, env)

evaluate state@( (PrintTable varName), kontinuation, env)
    | not (checkIfVarExists varName env) = error ("VariableExistenceException : Table with name: " ++ varName ++ " does not exist")
    |otherwise = return state


evaluate unexpectedError = error (show unexpectedError)

-- LOGIC PARTS END