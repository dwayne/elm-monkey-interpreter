module Test.Monkey.Interpreter exposing (suite)


import Expect
import Test exposing (..)


import Monkey.Interpreter exposing (..)


suite : Test
suite =
  describe "Interpreter"
    [ emptyProgramSuite
    , literalsSuite
    , letSuite
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


letSuite : Test
letSuite =
  describe "let" <|
    [ makeGoodExamples
        [ ("let a = 5; a", VNum 5)
        , ("let b = false; b", VBool False)
        , ("""
           let c = "hello";
           c
           """
          , VString "hello"
          )
        ]

    , makeBadExamples
        [ ("let a = a; a", IdentifierNotFound "a")
        , ("foobar", IdentifierNotFound "foobar")
        ]
    ]


makeGoodExamples : List (String, Value) -> Test
makeGoodExamples examples =
  describe "good examples"
    (examples
      |> List.map (Tuple.mapSecond (Ok << Value))
      |> makeExamplesHelper 1 []
    )


makeBadExamples : List (String, RuntimeError) -> Test
makeBadExamples examples =
  describe "bad examples"
    (examples
      |> List.map (Tuple.mapSecond (Err << RuntimeError))
      |> makeExamplesHelper 1 []
    )


makeExamplesHelper : Int -> List Test -> List (String, Result Error Answer) -> List Test
makeExamplesHelper n revTests examples =
  case examples of
    [] ->
      List.reverse revTests

    (input, expected) :: restExamples ->
      makeExamplesHelper
        (n + 1)
        ( test
            ("example " ++ String.fromInt n)
            (\_ ->
              run input
                |> Expect.equal expected
            )
          :: revTests
        )
        restExamples
