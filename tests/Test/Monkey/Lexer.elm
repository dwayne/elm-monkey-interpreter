module Test.Monkey.Lexer exposing (suite)


import Expect
import Test exposing (..)


import Monkey.Lexer as Lexer
import Parser as P exposing ((|.), Parser)


suite : Test
suite =
  describe "Lexer"
    [ identifierSuite
    , numberSuite
    , booleanSuite
    , stringSuite
    , extraSuite
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


numberSuite : Test
numberSuite =
  describe "number"
    [ test "example 1" <|
        \_ ->
          parse Lexer.number "0"
            |> Expect.equal (Just 0)

    , test "example 2" <|
        \_ ->
          parse Lexer.number "00"
            |> Expect.equal (Just 0)

    , test "example 3" <|
        \_ ->
          parse Lexer.number "123"
            |> Expect.equal (Just 123)

    , test "example 4" <|
        \_ ->
          parse Lexer.number "-1"
            |> Expect.equal Nothing
    ]


booleanSuite : Test
booleanSuite =
  describe "boolean"
    [ test "example 1" <|
        \_ ->
          parse Lexer.boolean "true"
            |> Expect.equal (Just True)

    , test "example 2" <|
        \_ ->
          parse Lexer.boolean "false"
            |> Expect.equal (Just False)

    , test "example 3" <|
        \_ ->
          parse Lexer.boolean "truer"
            |> Expect.equal Nothing

    , test "example 4" <|
        \_ ->
          parse Lexer.boolean "fals"
            |> Expect.equal Nothing
    ]


stringSuite : Test
stringSuite =
  describe "string"
    [ test "example 1" <|
        \_ ->
          parse Lexer.string "\"\""
            |> Expect.equal (Just "")

    , test "example 2" <|
        \_ ->
          parse Lexer.string "\"   \""
            |> Expect.equal (Just "   ")

    , test "example 3" <|
        \_ ->
          parse Lexer.string "\"foobar\""
            |> Expect.equal (Just "foobar")

    , test "example 4" <|
        \_ ->
          parse Lexer.string "\"foo bar\""
            |> Expect.equal (Just "foo bar")
    ]


extraSuite : Test
extraSuite =
  describe "various combinations"
    [ test "example 1" <|
        \_ ->
          let
            parser =
              P.succeed ()
                |. Lexer.spaces
                |. Lexer.identifier
                |. Lexer.number
                |. Lexer.boolean
                |. Lexer.string
                |. P.end
          in
          parse parser " x  1   true    \"hello, world!\"     "
            |> Expect.equal (Just ())

    , test "example 2" <|
        \_ ->
          let
            parser =
              P.succeed ()
                |. Lexer.spaces
                |. Lexer.rLet
                |. Lexer.identifier
                |. Lexer.equal
                |. Lexer.identifier
                |. Lexer.plus
                |. Lexer.number
                |. Lexer.semicolon
                |. P.end

            input =
              """
                let y
                  = x
                    + 5
                      ;
              """
          in
          parse parser input
            |> Expect.equal (Just ())
    ]


-- HELPERS


parse : Parser a -> String -> Maybe a
parse p =
  P.run p >> Result.toMaybe
