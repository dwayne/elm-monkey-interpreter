module Test.Monkey.Interpreter exposing (suite)


import Expect
import Test exposing (..)

import Array
import Monkey.Interpreter exposing (..)


suite : Test
suite =
  describe "Interpreter"
    [ emptyProgramSuite
    , letSuite
    , literalsSuite
    , arraysSuite
    , prefixSuite
    , infixSuite
    , ifSuite
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


arraysSuite : Test
arraysSuite =
  describe "arrays"
    [ makeGoodExamples
        [ ("[]", VArray <| Array.fromList [])
        , ("[1]", VArray <| Array.fromList [VNum 1])
        , ("[1, 2]", VArray <| Array.fromList [VNum 1, VNum 2])
        , ("[1, false, true, \"four\", [], [[]]]"
          , VArray <| Array.fromList
              [ VNum 1
              , VBool False
              , VBool True
              , VString "four"
              , VArray <| Array.fromList []
              , VArray <| Array.fromList
                  [ VArray <| Array.fromList []
                  ]
              ]
          )
        , ( """
            [ if (true) { let x = 1; }
            , x
            ]
            """
          , VArray <| Array.fromList [VNull, VNum 1]
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
            [ ("-true", UnknownOperation "-" [TBool])
            ]
        ]
    ]


infixSuite : Test
infixSuite =
  describe "infix expressions"
    [ describe "== (equal)"
        [ makeGoodExamples
            [ ("1 == 1", VBool True)
            , ("1 == 2", VBool False)
            , ("true == true", VBool True)
            , ("false == false", VBool True)
            , ("true == false", VBool False)
            , ( """
                "apple" == "apple"
                """
              , VBool True
              )
            , ( """
                "apple" == "bat"
                """
              , VBool False
              )
            , ("(1 < 2) == true", VBool True)
            , ("(1 < 2) == false", VBool False)
            , ("(1 > 2) == true", VBool False)
            , ("(1 > 2) == false", VBool True)
            , ("3 + 4 * 5 == 3 * 1 + 4 * 5", VBool True)
            , ("(10 + 2) * 30 == 300 + 20 * 3", VBool True)
            ]
        ]

    , describe "!= (not equal)"
        [ makeGoodExamples
            [ ("true != false", VBool True)
            , ("false != true", VBool True)
            , ("(5 > 5 == true) != false", VBool False)
            , ("500 / 2 != 250", VBool False)
            ]
        ]

    , describe "+ (add)"
        [ makeGoodExamples
            [ ("1 + 2", VNum 3)
            , ( """
                "Hello, " + "world!"
                """
              , VString "Hello, world!"
              )
            ]

        , makeBadExamples
            [ ("1 + true", UnknownOperation "+" [TInt, TBool])
            , ("true + 1", UnknownOperation "+" [TBool, TInt])
            , ( """
                "a" + 1
                """
              , UnknownOperation "+" [TString, TInt]
              )
            , ("true + false", UnknownOperation "+" [TBool, TBool])
            ]
        ]

    , describe "- (subtract)"
        [ makeGoodExamples
            [ ("3 - 1", VNum 2)
            ]

        , makeBadExamples
            [ ("3 - false", UnknownOperation "-" [TInt, TBool])
            ]
        ]

    , describe "* (multiply)"
        [ makeGoodExamples
            [ ("5 * 2", VNum 10)
            ]

        , makeBadExamples
            [ ( """
                3 * "h"
                """
              , UnknownOperation "*" [TInt, TString]
              )
            ]
        ]

    , describe "/ (divide)"
        [ makeGoodExamples
            [ ("6 / 2", VNum 3)
            , ("20 / 3", VNum 6)
            ]

        , makeBadExamples
            [ ("6 / (1 - 1)", ZeroDivisionError)
            , ( """
                "ab" / 2
                """
              , UnknownOperation "/" [TString, TInt]
              )
            ]
        ]
    ]


ifSuite : Test
ifSuite =
  describe "if"
    [ makeGoodExamples
        [ ("if (true) { 10 }", VNum 10)
        , ("if (false) { 10 }", VNull)
        , ("if (1) { 10 }", VNum 10)
        , ("if (1 < 2) { 10 }", VNum 10)
        , ("if (1 > 2) { 10 }", VNull)
        , ("if (1 > 2) { 10 } else { 20 }", VNum 20)
        , ("if (1 < 2) { 10 } else { 20 }", VNum 10)
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
