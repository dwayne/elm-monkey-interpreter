module Test.Monkey.Parser exposing (suite)


import Expect
import Test exposing (..)


import Monkey.Parser exposing (..)


suite : Test
suite =
  describe "Parser"
    [ programSuite
    ]


programSuite : Test
programSuite =
  describe "program"
    [ emptyProgramSuite
    , letStatementSuite
    , returnStatementSuite
    , expressionStatementSuite
    , miscSuite
    , bookSuite
    ]


emptyProgramSuite : Test
emptyProgramSuite =
  describe "empty program"
    [ test "example 1" <|
        \_ ->
          parse ""
            |> Expect.equal (Ok (Program []))

    , test "example 2" <|
        \_ ->
          parse "     "
            |> Expect.equal (Ok (Program []))
    ]


letStatementSuite : Test
letStatementSuite =
  describe "let statement"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ Let "x" (Num 123)
                ]
          in
          parse "let x = 123;"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ Let "x" (Num 123)
                , Let "y" (Num 456)
                ]
          in
          parse
            """
            let x = 123;
            let y = 456;
            """
            |> Expect.equal (Ok expected)
    ]


returnStatementSuite : Test
returnStatementSuite =
  describe "return statement"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ Return (Num 5)
                ]
          in
          parse "return 5;"
            |> Expect.equal (Ok expected)
    ]


expressionStatementSuite : Test
expressionStatementSuite =
  describe "expression statement"
    [ identifierSuite
    , numberSuite
    , booleanSuite
    , stringSuite
    , arraySuite
    , hashSuite
    , ifSuite
    , functionSuite
    , groupSuite
    , operationSuite
    ]


identifierSuite : Test
identifierSuite =
  describe "identifier"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Var "x")
                ]
          in
          parse "x"
            |> Expect.equal (Ok expected)
    ]


numberSuite : Test
numberSuite =
  describe "number"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Num 123)
                ]
          in
          parse "123"
            |> Expect.equal (Ok expected)
    ]


booleanSuite : Test
booleanSuite =
  describe "boolean"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Bool True)
                ]
          in
          parse "true"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Bool False)
                ]
          in
          parse "false"
            |> Expect.equal (Ok expected)
    ]


stringSuite : Test
stringSuite =
  describe "string"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (String "Hello, world!")
                ]
          in
          parse
            """
            "Hello, world!"
            """
            |> Expect.equal (Ok expected)
    ]


arraySuite : Test
arraySuite =
  describe "array"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Array [])
                ]
          in
          parse "[]"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Array [Num 1])
                ]
          in
          parse "[1]"
            |> Expect.equal (Ok expected)

    , test "example 3" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Array [Num 1, Num 2])
                ]
          in
          parse "[1, 2]"
            |> Expect.equal (Ok expected)

    , test "example 4" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Array
                      [ String "Thorsten"
                      , String "Ball"
                      , Num 28
                      , Function ["x"]
                          [ ExprStmt <| Infix Mul (Var "x") (Var "x")
                          ]
                      , Array []
                      , Array [Num 1, Array []]
                      ]
                ]
          in
          parse
            """
            [ "Thorsten"
            , "Ball"
            , 28
            , fn(x) { x * x }
            , []
            , [1, []]
            ]
            """
            |> Expect.equal (Ok expected)
    ]


hashSuite : Test
hashSuite =
  describe "hash"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Hash [])
                ]
          in
          parse "{}"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Hash
                      [ (String "name", String "Jimmy")
                      , (String "age", Num 72)
                      , (String "band", String "Led Zeppelin")
                      ]
                ]
          in
          parse
            """
            { "name": "Jimmy"
            , "age" : 72
            , "band": "Led Zeppelin"
            }
            """
            |> Expect.equal (Ok expected)

    , test "example 3" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Hash
                      [ (Bool True, String "yes, a boolean")
                      , (Num 99, String "correct, an integer")
                      ]
                ]
          in
          parse
            """
            { true: "yes, a boolean", 99: "correct, an integer" }
            """
            |> Expect.equal (Ok expected)

    , test "example 4" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Hash
                      [ (String "one", Infix Add (Num 0) (Num 1))
                      , (String "two", Infix Sub (Num 10) (Num 8))
                      , (String "three", Infix Div (Num 15) (Num 5))
                      ]
                ]
          in
          parse
            """
            { "one": 0 + 1
            , "two" : 10 - 8
            , "three": 15 / 5
            }
            """
            |> Expect.equal (Ok expected)
    ]


ifSuite : Test
ifSuite =
  describe "if"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (If (Var "x") [] Nothing)
                ]
          in
          parse "if (x) {}"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (If (Var "x") [] (Just []))
                ]
          in
          parse "if (x) {} else {}"
            |> Expect.equal (Ok expected)

    , test "example 3" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (If (Var "x") [ ExprStmt (Num 1) ] (Just [ ExprStmt (Num 2) ]))
                ]
          in
          parse "if (x) { 1 } else { 2; }"
            |> Expect.equal (Ok expected)
    ]


functionSuite : Test
functionSuite =
  describe "function"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Function [] [])
                ]
          in
          parse "fn () {}"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Function ["x"] [ ExprStmt (Var "x") ])
                ]
          in
          parse "fn (x) { x }"
            |> Expect.equal (Ok expected)

    , test "example 3" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt
                    ( Function ["x", "y"]
                        [ ExprStmt (Var "x")
                        , ExprStmt (Var "y")
                        ]
                    )
                ]
          in
          parse "fn (x, y) { x; y }"
            |> Expect.equal (Ok expected)

    , test "example 4" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Function ["x"] [ Return (Var "x") ])
                ]
          in
          parse "fn (x) { return x; }"
            |> Expect.equal (Ok expected)
    ]


groupSuite : Test
groupSuite =
  describe "group"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Var "x")
                ]
          in
          parse "(x)"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Num 1)
                ]
          in
          parse "(((1)))"
            |> Expect.equal (Ok expected)
    ]


operationSuite : Test
operationSuite =
  describe "operation"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Infix Equal (Num 1) (Num 1))
                ]
          in
          parse "1 == 1"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Infix NotEqual (Num 1) (Num 2))
                ]
          in
          parse "1 != 2"
            |> Expect.equal (Ok expected)

    , test "example 3" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Infix LessThan (Num 1) (Num 2))
                ]
          in
          parse "1 < 2"
            |> Expect.equal (Ok expected)

    , test "example 4" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Infix GreaterThan (Num 2) (Num 1))
                ]
          in
          parse "2 > 1"
            |> Expect.equal (Ok expected)

    , test "example 5" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Infix
                      Equal
                      (Infix LessThan (Num 1) (Num 2))
                      (Infix GreaterThan (Num 2) (Num 1))
                ]
          in
          parse "1 < 2 == 2 > 1"
            |> Expect.equal (Ok expected)

    , test "example 6" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Infix Add (Var "x") (Var "y"))
                ]
          in
          parse "x + y"
            |> Expect.equal (Ok expected)

    , test "example 7" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Infix Sub (Var "x") (Var "y"))
                ]
          in
          parse "x - y"
            |> Expect.equal (Ok expected)

    , test "example 8" <|
        \_ ->
          let
            -- (((a + b) - c) + d) - e
            expected =
              Program
                [ ExprStmt <|
                    Infix
                      Sub
                      (Infix
                        Add
                        (Infix
                          Sub
                          (Infix Add (Var "a") (Var "b"))
                          (Var "c")
                        )
                        (Var "d")
                      )
                      (Var "e")
                ]
          in
          parse "a + b - c + d - e"
            |> Expect.equal (Ok expected)

    , test "example 9" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Infix Mul (Var "x") (Var "y"))
                ]
          in
          parse "x * y"
            |> Expect.equal (Ok expected)

    , test "example 10" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Infix Div (Var "x") (Var "y"))
                ]
          in
          parse "x / y"
            |> Expect.equal (Ok expected)

    , test "example 11" <|
        \_ ->
          let
            -- (((a * b) / c) * d) / e
            expected =
              Program
                [ ExprStmt <|
                    Infix
                      Div
                      (Infix
                        Mul
                        (Infix
                          Div
                          (Infix Mul (Var "a") (Var "b"))
                          (Var "c")
                        )
                        (Var "d")
                      )
                      (Var "e")
                ]
          in
          parse "a * b / c * d / e"
            |> Expect.equal (Ok expected)

    , test "example 12" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Prefix Not (Var "x"))
                ]
          in
          parse "!x"
            |> Expect.equal (Ok expected)

    , test "example 13" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Prefix Negate (Var "x"))
                ]
          in
          parse "-x"
            |> Expect.equal (Ok expected)

    , test "example 14" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Prefix Not (Prefix Not (Prefix Not (Var "x")))
                ]
          in
          parse "!!!x"
            |> Expect.equal (Ok expected)

    , test "example 15" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Call (Var "f") [])
                ]
          in
          parse "f()"
            |> Expect.equal (Ok expected)

    , test "example 16" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Call (Var "f") [Num 1])
                ]
          in
          parse "f(1)"
            |> Expect.equal (Ok expected)

    , test "example 17" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Call (Var "f") [Num 1, Num 2])
                ]
          in
          parse "f(1, 2)"
            |> Expect.equal (Ok expected)

    , test "example 18" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Call
                      (Call
                        (Call (Var "f") [Num 1])
                        [Num 2]
                      )
                      [Num 3]
                ]
          in
          parse "f(1)(2)(3)"
            |> Expect.equal (Ok expected)

    , test "example 19" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt (Index (Var "f") (Num 1))
                ]
          in
          parse "f[1]"
            |> Expect.equal (Ok expected)

    , test "example 20" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Index
                      (Index
                        (Index (Var "f") (Num 0))
                        (Num 1)
                      )
                      (Num 2)
                ]
          in
          parse "f[0][1][2]"
            |> Expect.equal (Ok expected)

    , test "example 21" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Index
                      (Call
                        (Index (Call (Var "e") [Num 0]) (Num 1))
                        [Num 2]
                      )
                      (Num 3)
                ]
          in
          parse "e(0)[1](2)[3]"
            |> Expect.equal (Ok expected)

    , test "example 22" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Call
                      (Index
                        (Call (Index (Var "e") (Num 0)) [Num 1])
                        (Num 2)
                      )
                      [Num 3]
                ]
          in
          parse "e[0](1)[2](3)"
            |> Expect.equal (Ok expected)
    ]


miscSuite : Test
miscSuite =
  describe "miscellaneous examples"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Infix
                      LessThan
                      (Infix
                        Add
                        (Num 1)
                        (Infix Mul (Num 2) (Num 3))
                      )
                      (Infix
                        Div
                        (Infix Mul (Prefix Negate (Num 4)) (Num 4))
                        (Num 2)
                      )
                ]
          in
          parse "1 + 2 * 3 < -4 * 4 / 2"
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Prefix
                      Not
                      (Infix
                        GreaterThan
                        (Prefix Negate (Num 5))
                        (Num 3)
                      )
                ]
          in
          parse "!(-5 > 3)"
            |> Expect.equal (Ok expected)

    , test "example 3" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Infix
                      Mul
                      (Infix Add (Num 1) (Num 2))
                      (Num 3)
                ]
          in
          parse "(1 + 2) * 3"
            |> Expect.equal (Ok expected)

    , test "example 4" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Index
                      (Var "myArray")
                      (Infix Add (Num 1) (Num 1))
                ]
          in
          parse "myArray[1 + 1]"
            |> Expect.equal (Ok expected)

    , test "example 5" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Infix
                      Mul
                      (Infix
                        Mul
                        (Var "a")
                        (Index
                          (Array [Num 1, Num 2, Num 3, Num 4])
                          (Infix Mul (Var "b") (Var "c"))
                        )
                      )
                      (Var "d")
                ]
          in
          parse "a * [1, 2, 3, 4][b * c] * d"
            |> Expect.equal (Ok expected)

    , test "example 6" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Call
                      (Var "add")
                      [ Infix Mul (Var "a") (Index (Var "b") (Num 2))
                      , Index (Var "b") (Num 1)
                      , Infix
                          Mul
                          (Num 2)
                          (Index
                            (Array [Num 1, Num 2])
                            (Num 1)
                          )
                      ]
                ]
          in
          parse "add(a * b[2], b[1], 2 * [1, 2][1])"
            |> Expect.equal (Ok expected)
    ]


bookSuite : Test
bookSuite =
  describe "examples from the book"
    [ test "example 1" <|
        \_ ->
          let
            expected =
              Program
                [ Let "x" (Num 5)
                , Let "y" (Num 10)
                , Let "foobar" (Call (Var "add") [Num 5, Num 5])
                , Let "barfoo"
                    (Infix
                      Add
                      (Infix
                        Sub
                          (Infix
                            Add
                            (Infix
                              Div
                              (Infix Mul (Num 5) (Num 5))
                              (Num 10)
                            )
                            (Num 18)
                          )
                          (Call (Var "add") [Num 5, Num 5])
                      )
                      (Call (Var "multiply") [Num 124])
                    )
                , Let "anotherName" (Var "barfoo")
                ]
          in
          parse
            """
            let x = 5;
            let y = 10;
            let foobar = add(5, 5);
            let barfoo = 5 * 5 / 10 + 18 - add(5, 5) + multiply(124);
            let anotherName = barfoo;
            """
            |> Expect.equal (Ok expected)

    , test "example 2" <|
        \_ ->
          let
            expected =
              Program
                [ Let "x" (Num 10)
                , Let "y" (Num 15)
                , Let "add"
                    (Function
                      ["a", "b"]
                      [Return (Infix Add (Var "a") (Var "b"))]
                    )
                ]
          in
          parse
            """
            let x = 10;
            let y = 15;
            let add = fn(a, b) {
              return a + b;
            };
            """
            |> Expect.equal (Ok expected)

    , test "example 3" <|
        \_ ->
          let
            expected =
              Program
                [ Return (Num 5)
                , Return (Num 10)
                , Return (Call (Var "add") [Num 15])
                ]
          in
          parse
            """
            return 5;
            return 10;
            return add(15);
            """
            |> Expect.equal (Ok expected)

    , test "example 4" <|
        \_ ->
          let
            expected =
              Program
                [ ExprStmt <|
                    Infix Add (Infix Mul (Num 5) (Num 5)) (Num 10)
                , ExprStmt <|
                    Infix Mul (Num 5) (Infix Add (Num 5) (Num 10))
                , ExprStmt <|
                    Infix Sub (Prefix Negate (Num 5)) (Num 10)
                , ExprStmt <|
                    Infix
                      Mul
                      (Num 5)
                      (Infix
                        Add
                        (Call (Var "add") [Num 2, Num 3])
                        (Num 10)
                      )
                ]
          in
          parse
            """
            5 * 5 + 10
            5 * (5 + 10);
            -5 - 10
            5 * (add(2, 3) + 10)
            """
            -- NOTICE: I had to add a semicolon to make it parse correctly.
            -- Otherwise, it would be interpreted as 5 * (5 + 10) - 5 - 10.
            |> Expect.equal (Ok expected)
    ]
