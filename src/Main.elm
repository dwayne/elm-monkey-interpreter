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
  = EnteredSourceCode String
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
        [ H.text "... written in "
        , H.a
            [ HA.href "https://elm-lang.org/" ]
            [ H.text "Elm" ]
        , H.text ". It is based on the interpreter described in the book "
        , H.a
            [ HA.href "https://interpreterbook.com/" ]
            [ H.text "Writing an Interpreter in Go" ]
        , H.text "."
        ]
    , H.p []
        [ H.textarea
            [ HA.cols 80
            , HA.rows 20
            , HA.placeholder "Enter your Monkey program here."
            , HA.autofocus True
            , HE.onInput EnteredSourceCode
            ]
            [ H.text sourceCode ]
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
