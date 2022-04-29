module Monkey.Lexer exposing
  ( identifier
  )


import Parser as P exposing (Parser)
import Set exposing (Set)


identifier : Parser String
identifier =
  P.variable
    { start = isLetter
    , inner = isLetter
    , reserved = reserved
    }


-- HELPERS


isLetter : Char -> Bool
isLetter c =
  Char.isLower c || Char.isUpper c || c == '_'


reserved : Set String
reserved =
  Set.fromList
    [ "else"
    , "false"
    , "fn"
    , "if"
    , "let"
    , "return"
    , "true"
    ]
