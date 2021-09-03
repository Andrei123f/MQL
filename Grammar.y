{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token
    break          { TokenBreak _ }
    tableLength    { TokenTableLength _ }
    '++'           { TokenIncrement _ }
    isInt          { TokenIsInt _ }
    isString       { TokenIsString _ }
    isFloat        { TokenIsFloat _ }
    toString       { TokenToString _ }
    getInt         { TokenGetInt _ }
    getFloat       { TokenGetFloat _ }
    putRow         { TokenPutRow _ }
    ascSort        { TokenAscSort _}
    descSort       { TokenDescSort _}
    getRow         { TokenGetRow _ }
    putElement     { TokenPutElement _ }
    getElement     { TokenGetElement _ }
    resetRow       { TokenResetRow _ }
    mergeRow       { TokenMergeRow _ }
    '='            { TokenAssign _}
    TypeInt        { TokenTypeInt _}
    TypeString     { TokenTypeString _}
    TypeBool       { TokenTypeBool _}
    TypeFloat      { TokenTypeFloat _}
    TypeList       { TokenTypeList _}
    TypeLists      { TokenTypeLists _}
    TypeTable      { TokenTypeTable _}
    TypeRow        { TokenTypeRow _} 
    getTable       { TokenGetTable _ }
    printTable     { TokenPrintTable _}
	loop           { TokenLoop _}
    do             { TokenDo _}
    if             { TokenIf _}
    endLoop        { TokenEndLoop _}
    endIf          { TokenEndIf _}
    else           { TokenElse _}
    div            { TokenIntDiv _}
    mod            { TokenModulo _}
    '>='           { TokenGreaterEquals _}
    '<='           { TokenLessEquals _}
    '>'            { TokenGreater _}
    '!='           { TokenNotEquals _}
    '!'            { TokenNot _}
    OR             { TokenOR _}
    AND            { TokenAND _}
    '<'            { TokenLess _}
    '=='           { TokenEquals _ }
    '==='          { TokenIntEquals _ }
    '===='         { TokenStringEquals _ }
    varName        { TokenVar _ $$ }
    float          { TokenFloatValue _ $$}
    int            { TokenDigit _ $$ }
    '+'            { TokenPlus _}
    '-'            { TokenMinus _}
    '*'            { TokenTimes _}
    '/'            { TokenDiv _}
    '('            { TokenParenthesisOpen _}
    ')'            { TokenParenthesisClose _}
    '"'            { TokenQuotation _}
    true           { TokenTrue _}
    false          { TokenFalse _}
    ';'            { TokenEndLine _}

%left ';'
%nonassoc '(' ')' 
%nonassoc tableLength getTable getRow
%nonassoc printTable 
%nonassoc '=' '++'
%nonassoc putRow putElement getElement mergeRow resetRow ascSort descSort isInt  toString isString isFloat
%nonassoc TypeInt TypeBool TypeFloat TypeList TypeLists TypeTable TypeRow TypeString
%nonassoc if endIf loop endLoop do printTable 
%left AND OR
%left '!=' '=='
%right '!'
%nonassoc '<=' '>=' '>' '<' '===' '===='
%left '++'
%left '+' '-'
%left '*' '/'
%left mod div
%nonassoc int true false varName float
%left NEG

%%
Lines : if '(' Line ')' Lines endIf             { If $3 $5 }
      | if '(' Line ')' Lines else Lines endIf  { IfElse $3 $5  $7}
      | loop '(' Line ')' do Lines endLoop      { Loop $3 $6 }
      | printTable varName                      { PrintTable $2 }
      | TypeInt varName                         { IntDeclare $2 }
      | TypeInt varName '=' Line                { IntDeclareAssign $2 $4 }
      | TypeString varName                      { StringDeclare $2 }
      | TypeString varName '=' Line             { StringDeclareAssign $2 $4 }
      | TypeString varName '=' '"' Line '"'     { StringLiteralDeclareAssign $2 $5 }
      | TypeBool varName                        { BoolDeclare $2 }
      | TypeBool varName '=' Line               { BoolDeclareAssign $2 $4 }
      | TypeFloat varName                       { FloatDeclare $2 }
      | TypeFloat varName '=' Line              { FloatDeclareAssign $2 $4 }
      | TypeList varName                        { ListDeclare $2 }
      | TypeList varName '=' Line               { ListDeclareAssign $2 $4 }
      | TypeTable varName                       { TableDeclare $2 }
      | TypeTable varName '=' Line              { TableDeclareAssign $2 $4 }
      | TypeRow varName                         { RowDeclare $2 }
      | TypeRow varName '=' Line                { RowDeclareAssign $2 $4 }
      | TypeLists varName                       { ListsDeclare $2 }
      | TypeLists varName '=' Line              { ListsDeclareAssign $2 $4 }
      | varName '=' Line                        { VarAssign $1 $3 }
      | varName putRow '(' Line ')'             { PutRow $1 $4 }
      | varName putElement '(' Line ')'         { PutElement $1 $4 }
      | varName mergeRow '(' Line ')'           { MergeRow $1 $4 }
      | varName resetRow '('')'                 { ResetRow $1 }
      | varName toString '('')'                 { ToString $1 }
      | varName ascSort '('')'                  { AscSort $1 }
      | varName descSort '('')'                 { DescSort $1 }
      | varName '++'                            { VarAssign $1 (Plus (Var $1) (Int 1)) }
      | Lines ';' Lines                         { LinesStream $1 $3 }
      | Lines ';'                               { $1 }

Line : 
       Line mod Line                    { Mod $1 $3 } 
     | Line div Line                    { Div $1 $3 }
     | Line '>=' Line                   { GreaterEquals $1 $3 } 
     | Line '<=' Line                   { LessEquals $1 $3 } 
     | Line '>' Line                    { Greater $1 $3 } 
     | Line '!=' Line                   { NotEquals $1 $3 }
     | Line AND Line                    { And $1 $3 }
     | Line OR Line                     { Or $1 $3 }
     | '!' Line                         { Not $2 } 
     | Line '+' Line                    { Plus $1 $3 } 
     | Line '-' Line                    { Minus $1 $3 } 
     | Line '*' Line                    { Times $1 $3 } 
     | Line '/' Line                    { Divide $1 $3 }
     | Line '<' Line                    { Less $1 $3 }
     | Line '==' Line                   { Equals $1 $3 }
     | Line '===' Line                  { IntEquals $1 $3 }
     | Line '====' Line                 { StringEquals $1 $3 }
     | '(' Line ')'                     { $2 } 
     | true                             { TTrue }
     | false                            { TFalse }
     | float                            { Float $1 }
     | int                              { Int $1 }
     | varName                          { Var $1 }
     | varName tableLength              { TableLength $1 }
     | varName getRow '(' Line ')'      { GetRow $1 $4 }
     | varName getInt '('')'            { GetInt $1 }
     | varName getFloat '('')'          { GetFloat $1 }
     | varName getElement '(' Line ')'  {GetElement $1 $4}
     | getTable varName                 { GetTable $2 }
     | break                            { BreakLoop }
     | varName isInt '('')'             { IsInt $1 }
     | varName isString '('')'          { IsString $1 }
     | varName isFloat '('')'           { IsFloat $1 }
     | '-' Line %prec NEG               { Negate $2 } 
    
{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Lines = If Line Lines | IfElse Line Lines Lines 
           | Loop Line Lines
           | IntDeclareAssign String Line 
           | IntDeclare String 
           | StringDeclareAssign String Line
           | StringLiteralDeclareAssign String Line
           | StringDeclare String
		   | BoolDeclareAssign String Line 
		   | BoolDeclare String 
		   | ListDeclareAssign String Line 
		   | ListDeclare String
           | TableDeclareAssign String Line
           | TableDeclare String
           | RowDeclareAssign String Line
           | RowDeclare String
		   | FloatDeclareAssign String Line 
		   | FloatDeclare String  
           | ListsDeclareAssign String Line 
		   | ListsDeclare String 
           | VarAssign String Line
           | PrintTable String
           | PutRow String Line | PutElement String Line | MergeRow String Line | ResetRow String | AscSort String | DescSort String | ToString String
           | LinesStream Lines Lines
           deriving (Show,Eq)

data Line = Int Int | Var String |	TTrue | TFalse | Float Float
		  | Negate Line
          | Plus Line Line 
          | Minus Line Line 
          | Times Line Line 
          | Divide Line Line 
          | Mod Line Line 
          | Div Line Line  
          
          | IntEquals Line Line 
          | StringEquals Line Line
          | GreaterEquals Line Line
          | LessEquals Line Line 
          | Greater Line Line 
          | Less Line Line 

          | Equals Line Line 
          | NotEquals Line Line 
          | Or Line Line
          | And Line Line 
          | Not Line
          | IsInt String
          | IsString String
          | IsFloat String
          | TableLength String
          | GetRow String Line
          | GetInt String
          | GetFloat String
          | GetElement String Line
          | GetTable String
          | BreakLoop
          deriving (Show,Eq) 
} 