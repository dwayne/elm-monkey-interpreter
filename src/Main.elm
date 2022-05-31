module Main exposing (main)


import Browser as B
import Html as H
import Html.Attributes as HA


main : Program () Model Msg
main =
  B.sandbox
    { init = init
    , view = view
    , update = update
    }


-- MODEL


type alias Model =
  {}


init : Model
init = {}


-- UPDATE


type Msg
  = NoOp


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model


-- VIEW


view : Model -> H.Html msg
view _ =
  H.div []
    [ H.p []
        [ H.button
            []
            [ H.text "Run" ]
        ]
    , H.p []
        [ H.textarea
            [ HA.cols 80
            , HA.rows 20
            , HA.placeholder "Enter your Monkey program here."
            , HA.autofocus True
            ]
            []
        ]
    ]
