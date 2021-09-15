{
module Parse where
import AST
import Data.Char
import Data.Map.Strict as M hiding (map)
}

%name func
%tokentype { Token }
%error { parseError }

%token
  '='           { TEquals }
  '.'           { TDot }
  ','           { TComma }
  ';'           { TSemiColon }
  ':'           { TDDot }
  '::'          { T2DDot }
  '['           { TOpen }
  ']'           { TClose }
  '{'           { TROpen }
  '}'           { TRClose }
  '<'           { TTypeOpen }
  '>'           { TTypeClose }
  '^'           { TPower }
  '('           { TOpen2 }
  ')'           { TClose2 }
  NUM           { TNum $$ }
  ZERO_LEFT     { TZeroLeft }
  ZERO_right    { TZeroright }
  SUCC_LEFT     { TSuccLeft }
  SUCC_right    { TSuccright }
  DELETE_LEFT   { TDeleteLeft }
  DELETE_right  { TDeleteright }
  VAR           { TVar $$ }
  TYPE          { TType $$ }
  DEF           { TDef }
  CONST         { TConst }

%%
commsSeq    :: { [Comms] }
commsSeq    : comms                            { [$1] }
            | comms commsSeq                   { $1 : $2}

comms       :: { Comms }
comms       : DEF VAR '=' funcSeq ';'          { Def $2 $4 }
            | CONST VAR '=' exp ';'            { Const $2 $4 }
            | exp '::' NUM ';'                 { Infer $1 $3 }
            | exp ';'                          { Eval $1 }

exp         :: { Exp }
exp         : funcSeq ':' atoms                { Term $1 $3 }
            | atoms                            { $1 }

atoms       :: { Exp }
atoms       : '[' lista ']'                    { List ($2, DEFAULT) }
            | VAR                              { Var ($1, DEFAULT) }
            | '[' lista ']' '<' TYPE '>'       { List ($2, $5) }
            | VAR '<' TYPE '>'                 { Var ($1, $3) }

funcSeq     :: { [Funcs] }
funcSeq     : func                             { [$1] }
            | func '.' funcSeq                 { $1 : $3 }
            | func funcSeq                     { $1 : $2 }

lista       :: { [Elements] }
lista       : {- empty -}                      { [] }
            | NUM                              { [$1] }
            | NUM ',' lista                    { $1 : $3 }

func        :: { Funcs }
func        : ZERO_LEFT                        { Zero L }
            | ZERO_right                       { Zero R }
            | SUCC_LEFT                        { Succ L }
            | SUCC_right                       { Succ R }
            | DELETE_LEFT                      { Delete L }
            | DELETE_right                     { Delete R }
            | '{' funcSeq '}'                  { Rep $2 }
            | '(' funcSeq ')' '^' NUM          { Power $2 $5 }
            | VAR                              { Defined $1 }

{
data Token = TEquals
           | TDot
           | TComma
           | TSemiColon
           | TDDot
           | T2DDot
           | TOpen
           | TClose
           | TROpen
           | TRClose
           | TTypeOpen
           | TTypeClose
           | TPower
           | TOpen2
           | TClose2
           | TNum Int
           | TCons
           | TNil
           | TZeroLeft
           | TZeroright
           | TSuccLeft
           | TSuccright
           | TDeleteLeft
           | TDeleteright
           | TVar String
           | TType Type
           | TDef
           | TConst
           deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"

lexer :: String -> [Token]
lexer [] = []
lexer ('\n':cs) = lexer cs
lexer ('\r':cs) = lexer cs
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexer4num (c:cs)
             | isAlpha c = lexer4var (c:cs)
lexer ('-':'-':cs) = lexer $ dropWhile ((/=) '\n') cs
lexer ('=':cs) = TEquals : lexer cs
lexer ('.':cs) = TDot : lexer cs
lexer (',':cs) = TComma : lexer cs
lexer (';':cs) = TSemiColon : lexer cs
lexer (':':':':cs) = T2DDot : lexer cs
lexer (':':cs) = TDDot : lexer cs
lexer ss@('[':cs) = lexer4list ss
lexer ('{':cs) = TROpen : lexer cs
lexer ('}':cs) = TRClose : lexer cs
lexer ('<':cs) = TTypeOpen : lexer cs
lexer ('>':cs) = TTypeClose : lexer cs
lexer ('(':cs) = TOpen2 : lexer cs
lexer (')':cs) = TClose2 : lexer cs
lexer ('^':cs) = TPower : lexer cs

lexer4num :: String -> [Token]
lexer4num cs = let (nums, rest) = span (isDigit) cs
               in TNum (read nums) : lexer rest

lexer4var :: String -> [Token]
lexer4var cs = case span (\c -> isAlpha c || c == '_' || isDigit c) cs of
                ("zero_left", rest) -> TZeroLeft : lexer rest
                ("z_l", rest) -> TZeroLeft : lexer rest
                ("zero_right", rest) -> TZeroright : lexer rest
                ("z_r", rest) -> TZeroright : lexer rest
                ("succ_left", rest) -> TSuccLeft : lexer rest
                ("s_l", rest) -> TSuccLeft : lexer rest
                ("succ_right", rest) -> TSuccright : lexer rest
                ("s_r", rest) -> TSuccright : lexer rest
                ("delete_left", rest) -> TDeleteLeft : lexer rest
                ("d_l", rest) -> TDeleteLeft : lexer rest
                ("delete_right", rest) -> TDeleteright : lexer rest
                ("d_r", rest) -> TDeleteright : lexer rest
                ("def", rest) -> TDef : lexer rest
                ("const", rest) -> TConst : lexer rest
                (var, rest) -> case M.lookup var mapType of
                                    Just t -> TType t : lexer rest
                                    Nothing -> if head rest == '>' then TType (INVALID var) : lexer rest
                                               else TVar var : lexer rest

lexer4list :: String -> [Token]
lexer4list ('[':cs) = TOpen : lexer4list cs
lexer4list (']':cs) = TClose : lexer cs
lexer4list (',':cs) = TComma : lexer4list cs
lexer4list ('-':c:cs) | isDigit c = let (nums, rest) = span (isDigit) (c:cs)
                                    in TNum (-(read nums)) : lexer4list rest
                      | isSpace c = lexer4list cs
lexer4list (c:cs) | isDigit c = let (nums, rest) = span (isDigit) (c:cs)
                                in TNum (read nums) : lexer4list rest
                  | isSpace c = lexer4list cs

parser :: String  -> [Comms]
parser = func . lexer
}
