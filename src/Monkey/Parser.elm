module Monkey.Parser exposing
  ( Program(..), Stmt(..), Expr(..), UnaryOp(..), BinOp(..), Id, Block

  , Error
  , parse
  )


-- TODO:
--
-- 1. Implement optional.
-- 2. Implement many.
-- 3. Add tests.


import Monkey.Lexer exposing (..)
import Parser as P exposing ((|=), (|.), Parser)


type Program
  = Program (List Stmt)

type Stmt
  = Let Id Expr
  | Return Expr
  | ExprStmt Expr

type Expr
  = Var Id
  | Num Int
  | Bool Bool
  | String String
  | Array (List Expr)
  | Hash (List (Expr, Expr))
  | Prefix UnaryOp Expr
  | Infix BinOp Expr Expr
  | Call Expr (List Expr)
  | Index Expr Expr
  | If Expr Block (Maybe Block)
  | Fn (List Id) Block

type UnaryOp
  = Not
  | Negate

type BinOp
  = Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | Add
  | Sub
  | Mul
  | Div

type alias Id = String
type alias Block = List Stmt


type alias Error = List P.DeadEnd


parse : String -> Result Error Program
parse = P.run program


program : Parser Program
program =
  P.succeed Program
    |. spaces
    |= many stmt
    |. P.end


stmt : Parser Stmt
stmt =
  P.oneOf
    [ letStmt
    , returnStmt
    , exprStmt
    ]


letStmt : Parser Stmt
letStmt =
  P.succeed Let
    |. rLet
    |= identifier
    |. equal
    |= expr
    |. semicolon


returnStmt : Parser Stmt
returnStmt =
  P.succeed Return
    |. rReturn
    |= expr
    |. semicolon


exprStmt : Parser Stmt
exprStmt =
  P.succeed ExprStmt
    |= expr
    |. optional semicolon


expr : Parser Expr
expr = primary


primary : Parser Expr
primary =
  P.oneOf
    [ var
    , num
    , bool
    ]


var : Parser Expr
var =
  P.map Var identifier


num : Parser Expr
num =
  P.map Num number


bool : Parser Expr
bool =
  P.map Bool boolean
