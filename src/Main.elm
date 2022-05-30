module Main exposing (main)


import Browser as B
import Html as H


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
  H.text "Hello, world!"
