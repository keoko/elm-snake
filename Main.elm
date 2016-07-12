module Main exposing (..)

import Html.App as App
import Html exposing (Html, text, div)
import Html.Attributes exposing (id, style)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)


type alias Model =
    { left : Int
    , top : Int
    , direction : Direction
    }


type Msg = TimeUpdate  Time
      | KeyUp KeyCode
      | KeyDown KeyCode

type Direction = Up | Down | Left | Right

main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

model : Model
model =
    { left = 500
    , top = 0
    , direction = Right
    }

init : (Model, Cmd Msg)
init =
    (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyUp keycode ->
            (keyUp keycode model) ! []
        KeyDown keyCode ->
            model ! []
        TimeUpdate t ->
            (moveSnake model) ! []


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
        , AnimationFrame.diffs TimeUpdate
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
            changeDirection model Up
        40 ->
            changeDirection model Down
        37 ->
            changeDirection model Left
        39 ->
            changeDirection model Right
        _ ->
            model

keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    model

moveSnake : Model -> Model
moveSnake model =
    case model.direction of
        Up ->
            moveUp model
        Down ->
            moveDown model
        Left ->
            moveLeft model
        Right ->
            moveRight model


changeDirection : Model -> Direction -> Model
changeDirection model  direction =
    { model | direction = direction }
