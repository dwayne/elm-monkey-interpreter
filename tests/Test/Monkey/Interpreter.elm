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
    , returnSuite
    , literalsSuite
    , arraysSuite
    , hashSuite
    , prefixSuite
    , infixSuite
    , ifSuite
    , functionSuite
    , builtInFunctionsSuite
    , answerToStringSuite
    ]


emptyProgramSuite : Test
emptyProgramSuite =
  describe "empty program"
    [ test "example 1" <|
        \_ ->
          runForResult ""
            |> Expect.equal (Ok Void)

    , test "example 2" <|
        \_ ->
          runForResult "     "
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


returnSuite : Test
returnSuite =
  describe "return" <|
    [ makeGoodExamples
        [ ("return 10;", VNum 10)
        , ("return 10; 9", VNum 10)
        , ("return 2 * 5; 9", VNum 10)
        , ("9; return 2 * 5; 9", VNum 10)

        , ( """
            if (10 > 1) {
              if (10 > 1) {
                return 10;
              }

              return 1;
            }
            """
          , VNum 10
          )

        , ("return if (true) { return 10; };", VNum 10)
        , ("return if (true) { return if (true) { return 10; }; };", VNum 10)
        , ("return if (true) { return if (false) { return 10; }; };", VNull)

        , ( "[1, if (true) { return 2; }, 3]"
          , VArray <| Array.fromList
              [ VNum 1
              , VNum 2
              , VNum 3
              ]
          )
        , ("[1][if (true) { return 0; }]", VNum 1)

        , ( """
            { if (true) { return "one"; }: if (true) { return 1; }
            }
            """
          , VHash <| Hash.fromList
              [ (Hash.KString "one", VNum 1)
              ]
          )
        , ("{true:1}[if (true) { return true; }]", VNum 1)

        , ("!(if (true) { return false; })", VBool True)
        , ("-(if (true) { return 1; })", VNum -1)

        , ("(if (true) { return fn(x){x+1}; })(1)", VNum 2)
        , ("(fn(x){x+1})(if (true) { return 1; })", VNum 2)
        , ("(if (true) { return fn(x){x+1}; })(if (true) { return 1; })", VNum 2)

        , ( """
            let f = fn (n) {
              if (n == 1) {
                return n;
              } else {
                if (n == 2) {
                  let m = n * n;
                  return m;
                } else {
                  return 5 * n;
                }
              }
            };
            f(1) + f(2) + f(3)
            """
          , VNum 20
          )
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


functionSuite : Test
functionSuite =
  describe "function"
    [ test "example 1" <|
        \_ ->
          runForResult "fn(){}"
            |> Result.map answerToString
            |> Expect.equal (Ok "<function>")

    , test "example 2" <|
        \_ ->
          runForResult "fn (x) { x }"
            |> Result.map answerToString
            |> Expect.equal (Ok "<function>")

    , describe "calls"
        [ makeGoodExamples
            [ ("let identity = fn(x) { x }; identity(5)", VNum 5)
            , ("let double = fn(x) { x * 2 }; double(5)", VNum 10)
            , ("let add = fn(x, y) { x + y }; add(5, 5)", VNum 10)
            , ("let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5))", VNum 20)
            , ("fn(x) { x }(5)", VNum 5)
            , ( """
                let newAdder = fn(x) {
                  fn(y) { x + y }
                };
                let addTwo = newAdder(2);
                addTwo(2)
                """
              , VNum 4
              )

            , ( """
                let add = fn(a, b) { a + b };
                let sub = fn(a, b) { a - b };
                let applyFunc = fn(a, b, func) { func(a, b) };
                applyFunc(2, 2, add)
                """
              , VNum 4
              )
            , ( """
                let add = fn(a, b) { a + b };
                let sub = fn(a, b) { a - b };
                let applyFunc = fn(a, b, func) { func(a, b) };
                applyFunc(10, 2, sub)
                """
              , VNum 8
              )
            ]

        , makeBadExamples
            [ ("1(1)", TypeError [TFunction] TInt)
            , ("fn(x){}(1, 2)", ArgumentError 1 2)
            , ("fn(x, y){}(1)", ArgumentError 2 1)
            ]
        ]

    , describe "recursion"
        [ makeGoodExamples
            [ ( """
                let sum = fn(n) {
                  if (n == 0) {
                    0
                  } else {
                    n + sum(n - 1)
                  }
                };
                sum(10)
                """
              , VNum 55
              )
            , ( """
                let factorial = fn(n) {
                  if (n == 0) {
                    1
                  } else {
                    n * factorial(n - 1)
                  }
                };
                factorial(5)
                """
              , VNum 120
              )
            -- FIXME: This doesn't work as expected.
            --
            -- I get the following error:
            --
            --   RangeError: Maximum call stack size exceeded
            --
            -- , ( """
            --     let fib = fn(n) {
            --       if (n == 0) {
            --         1
            --       } else {
            --         if (n == 1) {
            --           1
            --         } else {
            --           fib(n - 1) + fib(n - 2)
            --         }
            --       }
            --     };
            --     fib(10)
            --     """
            --   , VNum 89
            --   )
            ]
        ]
    ]


builtInFunctionsSuite : Test
builtInFunctionsSuite =
  describe "built-in functions"
    [ describe "len"
        [ makeGoodExamples
            [ ( """
                len("")
                """
              , VNum 0
              )
            , ( """
                len("four")
                """
              , VNum 4
              )
            , ( """
                len("hello world")
                """
              , VNum 11
              )
            ]

        , makeBadExamples
            [ ( """
                len(1)
                """
              , TypeError [TString, TArray] TInt
              )
            , ( """
                len("one", "two")
                """
              , ArgumentError 1 2
              )
            ]
        ]

    , describe "first"
        [ makeGoodExamples
            [ ("first([])", VNull)
            , ("first([1])", VNum 1)
            , ("first([1, 2])", VNum 1)
            ]

        , makeBadExamples
            [ ("first(1)", TypeError [TArray] TInt)
            , ("first([], 1)", ArgumentError 1 2)
            ]
        ]

    , describe "last"
        [ makeGoodExamples
            [ ("last([])", VNull)
            , ("last([1])", VNum 1)
            , ("last([1, 2])", VNum 2)
            , ("last([1, 2, 3])", VNum 3)
            ]

        , makeBadExamples
            [ ("last(1)", TypeError [TArray] TInt)
            , ("last([], 1)", ArgumentError 1 2)
            ]
        ]

    , describe "rest"
        [ makeGoodExamples
            [ ("rest([])", VNull)
            , ("rest([1])", VArray <| Array.fromList [])
            , ("rest([1, 2])", VArray <| Array.fromList [VNum 2])
            , ("rest([1, 2, 3])", VArray <| Array.fromList [VNum 2, VNum 3])
            , ("rest(rest([1, 2, 3]))", VArray <| Array.fromList [VNum 3])
            ]

        , makeBadExamples
            [ ("rest(1)", TypeError [TArray] TInt)
            , ("rest([], 1)", ArgumentError 1 2)
            ]
        ]

    , describe "push"
        [ makeGoodExamples
            [ ("push([], 1)", VArray <| Array.fromList [VNum 1])
            , ("push([1], 2)", VArray <| Array.fromList [VNum 1, VNum 2])
            , ("push([1, 2], 3)", VArray <| Array.fromList [VNum 1, VNum 2, VNum 3])
            , ("push(push(push([], 1), 2), 3)", VArray <| Array.fromList [VNum 1, VNum 2, VNum 3])
            ]

        , makeBadExamples
            [ ("push(1, true)", TypeError [TArray] TInt)
            , ("push([])", ArgumentError 2 1)
            , ("push([], 1, true)", ArgumentError 2 3)
            ]

        ]

    , describe "puts"
        [ test "example 1" <|
            \_ ->
              run "puts()"
                |> Expect.equal
                    ( Ok (Value VNull)
                    , []
                    )

        , test "example 2" <|
            \_ ->
              run
                """
                let x = 5;
                puts(x, 10, true, false, "hello")
                """
                |> Expect.equal
                    ( Ok (Value VNull)
                    , [ "5"
                      , "10"
                      , "true"
                      , "false"
                      , "\"hello\""
                      ]
                    )

        , test "example 3" <|
            \_ ->
              run
                """
                puts([], [1], [1,2])
                """
                |> Expect.equal
                    ( Ok (Value VNull)
                    , [ "[]"
                      , "[1]"
                      , "[1, 2]"
                      ]
                    )

        , test "example 4" <|
            \_ ->
              run
                """
                puts({}, {1: 2}, {true: 1, false: 0})
                """
                |> Expect.equal
                    ( Ok (Value VNull)
                    , [ "{}"
                      , "{1: 2}"
                      , "{true: 1, false: 0}"
                      ]
                    )

        , test "example 5" <|
            \_ ->
              run
                """
                puts();
                puts(1);
                puts(2, 3)
                """
                |> Expect.equal
                    ( Ok (Value VNull)
                    , [ "1"
                      , "2"
                      , "3"
                      ]
                    )
        ]
    ]


answerToStringSuite : Test
answerToStringSuite =
  describe "answerToString"
    [ test "example 1" <|
        \_ ->
          answerToString Void
            |> Expect.equal ""

    , test "example 2" <|
        \_ ->
          answerToString (Value VNull)
            |> Expect.equal "null"

    , test "example 3" <|
        \_ ->
          answerToString (Value <| VNum 5)
            |> Expect.equal "5"

    , test "example 4" <|
        \_ ->
          answerToString (Value <| VBool True)
            |> Expect.equal "true"

    , test "example 5" <|
        \_ ->
          answerToString (Value <| VBool False)
            |> Expect.equal "false"

    , test "example 6" <|
        \_ ->
          answerToString (Value <| VString "hello")
            |> Expect.equal "\"hello\""

    , test "example 7" <|
        \_ ->
          let
            array =
              Array.fromList []
          in
          answerToString (Value <| VArray array)
            |> Expect.equal "[]"

    , test "example 8" <|
        \_ ->
          let
            array =
              Array.fromList
                [ VNull
                , VNum 1
                , VBool True
                , VString "x"
                , VArray <| Array.fromList []
                ]
          in
          answerToString (Value <| VArray array)
            |> Expect.equal "[null, 1, true, \"x\", []]"

    , test "example 9" <|
        \_ ->
          let
            hash =
              Hash.fromList []
          in
          answerToString (Value <| VHash hash)
            |> Expect.equal "{}"

    , test "example 10" <|
        \_ ->
          let
            hash =
              Hash.fromList
                [ (Hash.KNum 1, VNull)
                , (Hash.KBool True, VNum 1)
                , (Hash.KBool False, VBool True)
                , (Hash.KString "hello", VString "world")
                , (Hash.KString "empty", VArray <| Array.fromList [])
                ]
          in
          answerToString (Value <| VHash hash)
            |> Expect.equal
                "{1: null, true: 1, false: true, \"hello\": \"world\", \"empty\": []}"
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
      runForResult input
        |> Expect.equal expected


runForResult : String -> Result Error Answer
runForResult = run >> Tuple.first
