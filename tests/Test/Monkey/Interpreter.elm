module Test.Monkey.Interpreter exposing (suite)


import Expect
import Test exposing (..)


import Monkey.Interpreter exposing (..)


suite : Test
suite =
  describe "Interpreter"
    [ emptyProgramSuite
    , letSuite
    , literalsSuite
    , prefixSuite
    ]


emptyProgramSuite : Test
emptyProgramSuite =
  describe "empty program"
    [ test "example 1" <|
        \_ ->
          run ""
            |> Expect.equal (Ok Void)

    , test "example 2" <|
        \_ ->
          run "     "
            |> Expect.equal (Ok Void)
    ]


letSuite : Test
letSuite =
  describe "let" <|
    [ makeGoodExamples
        [ ("let a = 5; a", VNum 5)
        , ("let b = false; b", VBool False)
        , ( """
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


literalsSuite : Test
literalsSuite =
  describe "literals"
    [ makeGoodExamples
        [ ("5", VNum 5)
        , ("true", VBool True)
        , ("false", VBool False)
        , ( """
            "Hello, world!"
            """
          , VString "Hello, world!"
          )
        ]
    ]


prefixSuite : Test
prefixSuite =
  describe "prefix expressions"
    [ describe "! (not)"
        [ makeGoodExamples
            [ ("!true", VBool False)
            , ("!false", VBool True)
            , ("!5", VBool False)
            , ("!!true", VBool True)
            , ("!!false", VBool False)
            , ("!!5", VBool True)
            , ("!-5", VBool False)
            , ("!!-5", VBool True)
            , ("!!!!-5", VBool True)
            , ( """
                !""
                """
              , VBool False
              )
            ]
        ]

    , describe "- (negate)"
        [ makeGoodExamples
            [ ("5", VNum 5)
            , ("10", VNum 10)
            , ("-5", VNum (-5))
            , ("-10", VNum (-10))
            ]

        , makeBadExamples
            [ ("-true", TypeError TInt TBool)
            ]
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
      makeExamplesHelper (n + 1) (makeTest n input expected :: revTests) restExamples


makeTest : Int -> String -> Result Error Answer -> Test
makeTest n input expected =
  test ("example " ++ String.fromInt n) <|
    \_ ->
      run input
        |> Expect.equal expected
