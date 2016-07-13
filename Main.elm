module Main exposing (..)

import Html.App as App
import Html exposing (Html, text, div)
import Html.Attributes exposing (id, style)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)


type alias Model =
    { head : Point
    , direction : Direction
    }


type Msg = TimeUpdate  Time
      | KeyUp KeyCode
      | KeyDown KeyCode

type Direction = Up | Down | Left | Right

type alias Point = { x : Int
                   , y : Int
                   }

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
    { head = newPoint 500 0
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

toPixelCoordinates : Point -> (Int, Int)
toPixelCoordinates p =
    let
        {x, y} = newPoint 200 50 
    in
        (p.x - x, y - p.y)


view : Model -> Html Msg
view model =
    let
        (left, top) = toPixelCoordinates model.head
    in
        div
        [ id "rectangle"
        , style
              [ ("position", "absolute")
              , ("top", toPixel top)
              , ("left", toPixel left)
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
    let
        point' = newPoint model.head.x (model.head.y + 10)
    in
    { model | head = point' }

moveDown : Model -> Model
moveDown model =
    let
        point' = newPoint model.head.x (model.head.y - 10)
    in
    { model | head = point' }

moveLeft : Model -> Model
moveLeft model =
    let
        point' = newPoint (model.head.x - 10) model.head.y
    in
    { model | head = point' }

moveRight : Model -> Model
moveRight model =
    let
        point' = newPoint (model.head.x + 10) model.head.y
    in
    { model | head = point' }

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


newPoint : Int -> Int -> Point
newPoint x y =
    { x = x
    , y = y
    }
