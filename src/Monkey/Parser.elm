module Monkey.Parser exposing
  ( Program(..), Stmt(..), Expr(..), UnaryOp(..), BinOp(..), Id, Block

  , Error
  , parse
  )


-- TODO:
--
-- 1. If
-- 2. Function
-- 3. Group
-- 4. Equality
-- 5. Array
-- 6. Hash


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
    , str
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


str : Parser Expr
str =
  P.map String string


many : Parser a -> Parser (List a)
many p =
  let
    helper rev =
      P.oneOf
        [ P.succeed (\x -> P.Loop (x :: rev))
            |= p
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse rev))
        ]
  in
  P.loop [] helper


optional : Parser a -> Parser (Maybe a)
optional p =
  P.oneOf
    [ P.map Just p
    , P.succeed Nothing
    ]
