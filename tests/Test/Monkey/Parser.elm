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
    ]
