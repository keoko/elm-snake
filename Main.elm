module Main exposing (..)

import Html.App as App
import Html exposing (Html, text, div)
import Html.Attributes exposing (id, style)
import Keyboard exposing (KeyCode)


type alias Model =
    { left : Int
    , top : Int
    }


type Msg
    = None
      | KeyUp KeyCode
      | KeyDown KeyCode

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

model =
    { left = 500
    , top = 0
    }

init =
    (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyUp keycode ->
            (keyUp keycode model) ! []
        KeyDown keyCode ->
            model ! []
        _ ->
            model ! []


toPixel : Int -> String
toPixel x =
    (toString x) ++ "px"


view : Model -> Html Msg
view model =
    div
        [ id "rectangle"
        , style
            [ ("position", "absolute")
            , ("top", toPixel model.top)
            , ("left", toPixel model.left)
            , ( "width", "10px" )
            , ( "height", "10px" )
            , ( "background-color", "blue" )
            ]
        ]
        []

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        ]


arrowLeft : Int
arrowLeft = 37

arrowRight : Int
arrowRight = 39

keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case keyCode of
        37 ->
            moveLeft model
        39 ->
            moveRight model
        _ ->
            model

moveLeft : Model -> Model
moveLeft model =
    { model | left = model.left - 10 }

moveRight : Model -> Model
moveRight model =
    { model | left = model.left + 10 }


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    model
