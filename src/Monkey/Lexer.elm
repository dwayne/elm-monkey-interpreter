module Monkey.Lexer exposing
  ( identifier, number, boolean, string

  , rElse, rFn, rIf, rLet, rReturn

  , asterisk, bang, bangEqual, colon, doubleEqual, equal, greaterThan
  , hyphen, leftBracket, leftParen, leftSquareBracket, lessThan, plus
  , rightBracket, rightParen, rightSquareBracket, semicolon, slash

  , brackets, parens, squares
  , spaces
  )


import Parser as P exposing ((|=), (|.), Parser)
import Set exposing (Set)


identifier : Parser String
identifier =
  lexeme <|
    P.variable
      { start = isLetter
      , inner = isLetter
      , reserved = reserved
      }


number : Parser Int
number =
  lexeme P.int


boolean : Parser Bool
boolean =
  lexeme <|
    P.oneOf
      [ P.succeed True
          |. P.keyword "true"
      , P.succeed False
          |. P.keyword "false"
      ]


string : Parser String
string =
  let
    chars =
      P.getChompedString <|
        P.chompWhile ((/=) '"')
  in
  lexeme <|
    P.succeed identity
      |. P.chompIf ((==) '"')
      |= chars
      |. P.chompIf ((==) '"')


-- RESERVED NAMES


rElse : Parser ()
rElse =
  keyword "else"


rFn : Parser ()
rFn =
  keyword "fn"


rIf : Parser ()
rIf =
  keyword "if"


rLet : Parser ()
rLet =
  keyword "let"


rReturn : Parser ()
rReturn =
  keyword "return"


-- SYMBOLS


asterisk : Parser ()
asterisk =
  symbol "*"


bang : Parser ()
bang =
  symbol "!"


bangEqual : Parser ()
bangEqual =
  symbol "!="


colon : Parser ()
colon =
  symbol ":"


doubleEqual : Parser ()
doubleEqual =
  symbol "=="


equal : Parser ()
equal =
  symbol "="


greaterThan : Parser ()
greaterThan =
  symbol ">"


hyphen : Parser ()
hyphen =
  symbol "-"


leftBracket : Parser ()
leftBracket =
  symbol "{"


leftParen : Parser ()
leftParen =
  symbol "("


leftSquareBracket : Parser ()
leftSquareBracket =
  symbol "["


lessThan : Parser ()
lessThan =
  symbol "<"


plus : Parser ()
plus =
  symbol "+"


rightBracket : Parser ()
rightBracket =
  symbol "}"


rightParen : Parser ()
rightParen =
  symbol ")"


rightSquareBracket : Parser ()
rightSquareBracket =
  symbol "]"


semicolon : Parser ()
semicolon =
  symbol ";"


slash : Parser ()
slash =
  symbol "/"


-- MISC


brackets : Parser a -> Parser (List a)
brackets item =
  between "{" item "}"


parens : Parser a -> Parser (List a)
parens item =
  between "(" item ")"


squares : Parser a -> Parser (List a)
squares item =
  between "[" item "]"


between : String -> Parser a -> String -> Parser (List a)
between left item right =
  lexeme <|
    P.sequence
      { start = left
      , separator = ","
      , end = right
      , spaces = spaces
      , item = item
      , trailing = P.Forbidden
      }


spaces : Parser ()
spaces =
  P.spaces


-- HELPERS


keyword : String -> Parser ()
keyword =
  lexeme << P.keyword


symbol : String -> Parser ()
symbol =
  lexeme << P.symbol


lexeme : Parser a -> Parser a
lexeme p =
  P.succeed identity
    |= p
    |. spaces


isLetter : Char -> Bool
isLetter c =
  Char.isAlpha c || c == '_'


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
