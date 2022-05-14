module Test.Monkey.Interpreter exposing (suite)


import Expect
import Test exposing (..)


import Monkey.Interpreter exposing (..)


suite : Test
suite =
  describe "Interpreter"
    [ emptyProgramSuite
    , literalsSuite
    ]


emptyProgramSuite : Test
emptyProgramSuite =
  describe "empty program"
    [ test "example 1" <|
        \_ ->
          run ""
            |> Expect.equal (Ok Void)
    ]


literalsSuite : Test
literalsSuite =
  describe "literals"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Value (VNum 5)
          in
          run "5"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Value (VBool True)
          in
          run "true"
            |> Expect.equal (Ok expected)

    , test "example 3" <|
        \_ ->
          let
            expected =
              Value (VBool False)
          in
          run "false"
            |> Expect.equal (Ok expected)

    , test "example 4" <|
        \_ ->
          let
            expected =
              Value (VString "Hello, world!")
          in
          run
            """
            "Hello, world!"
            """
            |> Expect.equal (Ok expected)
    ]
