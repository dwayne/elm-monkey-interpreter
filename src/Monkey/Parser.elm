module Monkey.Parser exposing
  ( Program(..), Stmt(..), Expr(..), UnaryOp(..), BinOp(..), Id, Block

  , Error
  , parse
  )


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
  | Function (List Id) Block

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
expr = equality


equality : Parser Expr
equality =
  let
    buildExpr left maybeRest =
      case maybeRest of
        Just (op, right) ->
          Infix op left right

        Nothing ->
          left
  in
  P.succeed buildExpr
    |= comparison
    |= optional
        ( P.succeed Tuple.pair
            |= P.oneOf
                [ P.map (always Equal) doubleEqual
                , P.map (always NotEqual) bangEqual
                ]
            |= comparison
        )


comparison : Parser Expr
comparison = primary


primary : Parser Expr
primary =
  P.oneOf
    [ var
    , num
    , bool
    , str
    , ifExpr
    , function
    , group
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


ifExpr : Parser Expr
ifExpr =
  P.succeed If
    |. rIf
    |. leftParen
    |= P.lazy (\_ -> expr)
    |. rightParen
    |= block
    |= optional
        ( P.succeed identity
            |. rElse
            |= block
        )


function : Parser Expr
function =
  P.succeed Function
    |. rFn
    |= params
    |= block


group : Parser Expr
group =
  P.succeed identity
    |. leftParen
    |= P.lazy (\_ -> expr)
    |. rightParen


block : Parser (List Stmt)
block =
  P.succeed identity
    |. leftBracket
    |= many (P.lazy (\_ -> stmt))
    |. rightBracket


params : Parser (List Id)
params =
  parens identifier


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
