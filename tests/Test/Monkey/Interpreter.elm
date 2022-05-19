module Test.Monkey.Interpreter exposing (suite)


import Expect
import Test exposing (..)

import Array
import Monkey.Hash as Hash
import Monkey.Interpreter exposing (..)


suite : Test
suite =
  describe "Interpreter"
    [ emptyProgramSuite
    , letSuite
    , literalsSuite
    , arraysSuite
    , hashSuite
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

    , describe "index"
        [ makeGoodExamples
            [ ( "[1, 2, 3][0]"
              , VNum 1
              )
            , ( "[1, 2, 3][1]"
              , VNum 2
              )
            , ( "[1, 2, 3][2]"
              , VNum 3
              )
            , ( "let i = 0; [1][i];"
              , VNum 1
              )
            , ( "[1, 2, 3][1 + 1];"
              , VNum 3
              )
            , ( "let myArray = [1, 2, 3]; myArray[2];"
              , VNum 3
              )
            , ( "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];"
              , VNum 6
              )
            , ( "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]"
              , VNum 2
              )
            , ( "[1, 2, 3][3]"
              , VNull
              )
            , ( "[1, 2, 3][-1]"
              , VNull
              )
            ]

        , makeBadExamples
            [ ( "1[0]"
              , TypeError [TArray, THash] TInt
              )
            , ( "[1][false]"
              , TypeError [TInt] TBool
              )
            ]
        ]
    ]


hashSuite : Test
hashSuite =
  describe "hash"
    [ makeGoodExamples
        [ ( "{}"
          , VHash <| Hash.fromList []
          )
        , ( """
            {"name": "Jimmy", "age": 72, "band": "Led Zeppelin"}
            """
          , VHash <| Hash.fromList
              [ (Hash.KString "name", VString "Jimmy")
              , (Hash.KString "age", VNum 72)
              , (Hash.KString "band", VString "Led Zeppelin")
              ]
          )
        , ( """
            {true: "yes, a boolean", 99: "correct, an integer"}
            """
          , VHash <| Hash.fromList
              [ (Hash.KBool True, VString "yes, a boolean")
              , (Hash.KNum 99, VString "correct, an integer")
              ]
          )
        , ( """
            let two = "two";
            { "one": 10 - 9
            , two: 1 + 1
            , "thr" + "ee": 6 / 2
            , 4: 4
            , true: 5
            , false: 6
            }
            """
          , VHash <| Hash.fromList
              [ (Hash.KString "one", VNum 1)
              , (Hash.KString "two", VNum 2)
              , (Hash.KString "three", VNum 3)
              , (Hash.KNum 4, VNum 4)
              , (Hash.KBool True, VNum 5)
              , (Hash.KBool False, VNum 6)
              ]
          )
        ]

    , describe "index"
        [ makeGoodExamples
            [ ( """
                {"foo": 5}["foo"]
                """
              , VNum 5
              )
            , ( """
                {"foo": 5}["bar"]
                """
              , VNull
              )
            , ( """
                let key = "foo"; {"foo": 5}[key]
                """
              , VNum 5
              )
            , ( """
                {}["foo"]
                """
              , VNull
              )
            , ( """
                {5: 5}[5]
                """
              , VNum 5
              )
            , ( """
                {true: 5}[true]
                """
              , VNum 5
              )
            , ( """
                {false: 5}[false]
                """
              , VNum 5
              )
            ]

        , makeBadExamples
            [ ( """
                {"name": "Monkey"}[[]]
                """
              , TypeError [TInt, TBool, TString] TArray
              )
            ]
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
