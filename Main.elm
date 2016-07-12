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

moveUp : Model -> Model
moveUp model =
    { model | top = model.top - 10 }

moveDown : Model -> Model
moveDown model =
    { model | top = model.top + 10 }

moveLeft : Model -> Model
moveLeft model =
    { model | left = model.left - 10 }

moveRight : Model -> Model
moveRight model =
    { model | left = model.left + 10 }

keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case keyCode of
        38 ->
            moveUp model
        40 ->
            moveDown model
        37 ->
            moveLeft model
        39 ->
            moveRight model
        _ ->
            model

keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    model
