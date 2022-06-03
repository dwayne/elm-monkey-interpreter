module Main exposing (main)


import Browser as B
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Monkey.Interpreter as I


main : Program () Model Msg
main =
  B.sandbox
    { init = init
    , view = view
    , update = update
    }


-- MODEL


type alias Model =
  { sourceCode : String
  , logs : List Log
  }


type Log
  = Output String
  | Answer String
  | Error String


init : Model
init =
  { sourceCode = ""
  , logs = []
  }


-- UPDATE


type Msg
  = SelectedAnExampleProgram String
  | EnteredSourceCode String
  | ClickedRun


update : Msg -> Model -> Model
update msg model =
  case msg of
    EnteredSourceCode sourceCode ->
      { model | sourceCode = sourceCode }

    ClickedRun ->
      let
        (result, logs) =
          I.run model.sourceCode
            |> Tuple.mapSecond (List.map Output)
      in
      case result of
        Ok answer ->
          { model
          | logs = appendAnswerToLogs answer logs
          }

        Err error ->
          { model
          | logs = appendErrorToLogs error logs
          }

    SelectedAnExampleProgram value ->
      case value of
        "" ->
          { model | sourceCode = "" }

        "hello" ->
          { model | sourceCode = examplePrograms.hello }

        "count" ->
          { model | sourceCode = examplePrograms.count }

        "gcd" ->
          { model | sourceCode = examplePrograms.gcd }

        "factorial" ->
          { model | sourceCode = examplePrograms.factorial }

        "fibonacci" ->
          { model | sourceCode = examplePrograms.fibonacci }

        "map" ->
          { model | sourceCode = examplePrograms.map }

        "reduce" ->
          { model | sourceCode = examplePrograms.reduce }

        "filter" ->
          { model | sourceCode = examplePrograms.filter }

        "hashes" ->
          { model | sourceCode = examplePrograms.hashes }

        _ ->
          model


appendAnswerToLogs : I.Answer -> List Log -> List Log
appendAnswerToLogs answer logs =
  let
    s = I.answerToString answer
  in
  if String.isEmpty s then
    logs
  else
    logs ++ [ Answer s ]


appendErrorToLogs : I.Error -> List Log -> List Log
appendErrorToLogs error logs =
  logs ++ [ Error <| errorToString error ]


errorToString : I.Error -> String
errorToString error =
  case error of
    I.SyntaxError _ ->
      "Syntax Error: Please check your code and try again."

    I.RuntimeError runtimeError ->
      case runtimeError of
        I.ArgumentError expected actual ->
          [ "Incorrect Number Of Arguments: I expected the function to be called with "
          , String.fromInt expected
          , " "
          , pluralize expected "argument" "arguments"
          , " but instead it was called with "
          , String.fromInt actual
          , " "
          , pluralize actual "argument" "arguments"
          , "."
          ]
          |> String.join ""

        I.IdentifierNotFound identifier ->
          "Identifier Not Found: " ++ identifier

        I.TypeError expectedTypes actualType ->
          [ "Type Error: I expected "
          , case expectedTypes of
              [ expectedType ] ->
                "the type " ++ typeToString expectedType

              _ ->
                [ "a type from the set {"
                , expectedTypes
                    |> List.map typeToString
                    |> String.join ", "
                , "}"
                ]
                |> String.join ""
          , " but instead I got the type "
          , typeToString actualType
          , "."
          ]
          |> String.join ""

        I.UnknownOperation operation types ->
          [ "Unknown Operation: "
          , operation
          , " on "
          , types
              |> List.map typeToString
              |> String.join " \u{00D7} "
          , "."
          ]
          |> String.join ""

        I.ZeroDivisionError ->
          "Zero Division Error: Division by 0 is not allowed."


typeToString : I.Type -> String
typeToString type_ =
  case type_ of
      I.TNull ->
        "Null"

      I.TInt ->
        "Int"

      I.TBool ->
        "Bool"

      I.TString ->
        "String"

      I.TArray ->
        "Array"

      I.THash ->
        "Hash"

      I.TFunction ->
        "Function"

      I.TReturn subtype ->
        -- N.B.
        -- This should NEVER be seen by the user. If it is then
        -- there is a logical error in the implementation of your
        -- interpreter.
        "Return " ++ typeToString subtype


pluralize : Int -> String -> String -> String
pluralize n singular plural =
  if n == 1 then
    singular
  else
    plural



-- VIEW


view : Model -> H.Html Msg
view { sourceCode, logs } =
  H.div []
    [ H.h1 [] [ H.text "A Monkey Interpreter" ]
    , H.p []
        [ H.text "It's written in "
        , H.a
            [ HA.href "https://elm-lang.org/" ]
            [ H.text "Elm" ]
        , H.text " and is based on the interpreter described in the book "
        , H.a
            [ HA.href "https://interpreterbook.com/" ]
            [ H.text "Writing an Interpreter in Go" ]
        , H.text "."
        ]
    , H.ul []
        [ H.li []
            [ H.text "Learn the syntax through "
            , H.a
                [ HA.href "https://github.com/dwayne/elm-monkey-interpreter/blob/master/tests/Test/Monkey/Parser.elm" ]
                [ H.text "examples" ]
            , H.text "."
            ]
        , H.li []
            [ H.text "Learn the syntax by reading the "
            , H.a
                [ HA.href "https://github.com/dwayne/elm-monkey-interpreter/blob/master/grammar.ebnf" ]
                [ H.text "context-free grammar" ]
            , H.text "."
            ]
        , H.li []
            [ H.text "Learn the semantics through "
            , H.a
                [ HA.href "https://github.com/dwayne/elm-monkey-interpreter/blob/master/tests/Test/Monkey/Interpreter.elm" ]
                [ H.text "examples" ]
            , H.text "."
            ]
        , H.li []
            [ H.text "Read the "
            , H.a
                [ HA.href "https://github.com/dwayne/elm-monkey-interpreter/blob/master/src/Monkey/Interpreter.elm" ]
                [ H.text "source code for the interpreter" ]
            , H.text " to learn how it's implemented with a custom "
            , H.a
                [ HA.href "https://github.com/dwayne/elm-monkey-interpreter/blob/master/src/Monkey/Eval.elm" ]
                [ H.text "Eval" ]
            , H.text " monad."
            ]
        ]
    , H.p []
        [ H.text "Enjoy!"
        ]
    , H.p []
        [ H.select
            [ HE.onInput SelectedAnExampleProgram
            ]
            [ H.option
                [ HA.value "" ]
                [ H.text "Please choose an example program..." ]
            , H.option [ HA.value "hello" ] [ H.text "hello" ]
            , H.option [ HA.value "count" ] [ H.text "count" ]
            , H.option [ HA.value "gcd" ] [ H.text "gcd" ]
            , H.option [ HA.value "factorial" ] [ H.text "factorial" ]
            , H.option [ HA.value "fibonacci" ] [ H.text "fibonacci" ]
            , H.option [ HA.value "map" ] [ H.text "map" ]
            , H.option [ HA.value "reduce" ] [ H.text "reduce" ]
            , H.option [ HA.value "filter" ] [ H.text "filter" ]
            , H.option [ HA.value "hashes" ] [ H.text "hashes" ]
            ]
        ]
    , H.p []
        [ H.textarea
            [ HA.class "sourceCode"
            , HA.cols 80
            , HA.rows 25
            , HA.placeholder "... or write your own Monkey program."
            , HA.autofocus True
            , HA.spellcheck False
            , HA.value sourceCode
            , HE.onInput EnteredSourceCode
            ]
            []
        ]
    , H.p []
        [ H.button
            [ HE.onClick ClickedRun
            ]
            [ H.text "Run" ]
        ]
    , H.h2 [] [ H.text "Output" ]
    , if List.isEmpty logs then
        H.p []
          [ H.text "Any output as a result of running your program will appear here."
          ]
      else
        logs
            |> List.map viewLog
            |> H.div []
    ]


viewLog : Log -> H.Html msg
viewLog log =
  case log of
    Output s ->
      H.div [] [ H.text s ]

    Answer s ->
      H.div
        [ HA.style "color" "green" ]
        [ H.text s ]

    Error s ->
      H.div
        [ HA.style "color" "red" ]
        [ H.text s ]


-- EXAMPLE PROGRAMS


examplePrograms =
  { hello = "\"Hello, world!\""
  , count = "let count = fn (n) {\n  if (n < 11) {\n    puts(n);\n    count(n + 1)\n  }\n};\ncount(1)"
  , gcd = "let mod = fn (a, b) {\n  a - a / b * b\n};\nlet gcd = fn (a, b) {  \n  if (b == 0) {\n    a\n  } else {\n    gcd(b, mod(a, b))\n  }\n};\ngcd(18, 12)"
  , factorial = "let fact = fn (n) {\n  if (n == 0) {\n    1\n  } else {\n    n * fact(n - 1)\n  }\n};\nfact(10)"
  , fibonacci = "let fib = fn (n) {\n  if (n == 0) {\n    0\n  } else {\n    if (n == 1) {\n      1\n    } else {\n      fib(n - 1) + fib(n - 2)\n    }\n  }\n};\nfib(20)"
  , map = "let map = fn(f, arr) {\n  let iter = fn(arr, accumulated) {\n    if (len(arr) == 0) {\n      accumulated\n    } else {\n      iter(rest(arr), push(accumulated, f(first(arr))))\n    }\n  };\n  iter(arr, [])\n};\nlet sqr = fn(x) { x * x };\nlet a = [1, 2, 3, 4, 5];\nmap(sqr, a)"
  , reduce = "let reduce = fn (f, default, arr) {\n  let iter = fn (arr, accumulated) {\n    if (len(arr) == 0) {\n      accumulated\n    } else {\n      iter(rest(arr), f(accumulated, first(arr)))\n    }\n  };\n  iter(arr, default)\n};\nlet add = fn (a, b) { a + b };\nlet sum = fn (arr) { reduce(add, 0, arr) };\nsum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])"
  , filter = "let filter = fn (pred, arr) {\n  let iter = fn (arr, accumulated) {\n    if (len(arr) == 0) {\n      accumulated\n    } else {\n      if (pred(first(arr))) {\n        iter(rest(arr), push(accumulated, first(arr)))\n      } else {\n        iter(rest(arr), accumulated)\n      }\n    }\n  };\n  iter(arr, [])\n};\nlet mod = fn (a, b) {\n  a - a / b * b\n};\nlet isEven = fn (n) {\n  mod(n, 2) == 0\n};\nfilter(isEven, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])"
  , hashes = "let fruits = [\n  {\"name\": \"apple\", \"calories\": 104},\n  {\"name\": \"banana\", \"calories\": 105}\n];\n\nlet getName = fn (fruit) { fruit[\"name\"] };\n\nputs(\n  getName(fruits[0]),\n  getName(fruits[1])\n)"
  }
