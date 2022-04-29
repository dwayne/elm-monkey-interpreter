module Test.Monkey.Lexer exposing (suite)


import Expect
import Test exposing (..)


import Monkey.Lexer as Lexer
import Parser as P exposing (Parser)


suite : Test
suite =
  describe "Lexer"
    [ identifierSuite
    ]


identifierSuite : Test
identifierSuite =
  describe "identifier"
    [ test "example 1" <|
        \_ ->
          parse Lexer.identifier "x"
            |> Expect.equal (Just "x")

    , test "example 2" <|
        \_ ->
          parse Lexer.identifier "letter"
            |> Expect.equal (Just "letter")

    , test "example 3" <|
        \_ ->
          parse Lexer.identifier "let"
            |> Expect.equal Nothing

    , test "example 4" <|
        \_ ->
          parse Lexer.identifier "true"
            |> Expect.equal Nothing
    ]


-- HELPERS


parse : Parser a -> String -> Maybe a
parse p =
  P.run p >> Result.toMaybe
