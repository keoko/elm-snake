module Main exposing (..)

import Html.App as App
import Html exposing (Html, text, div)
import Html.Attributes exposing (id, style)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)


type alias Model =
    { snake : Snake
    , direction : Direction
    }

type alias Snake = List Point

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

initialPoint : Point
initialPoint = newPoint 500 0

model : Model
model =
    { snake = [initialPoint, (newPoint 501 0), (newPoint 502 0)]
    , direction = Left
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


viewSnakeSegment : (Int, Int) -> Html Msg
viewSnakeSegment (left, top) =
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
    ] []

view : Model -> Html Msg
view model =
    let
        snake = List.map (\p -> p |> toPixelCoordinates |> viewSnakeSegment) model.snake
    in
        div [] snake

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        , AnimationFrame.diffs TimeUpdate
        ]

mutateSnake : Snake -> Point -> Snake
mutateSnake snake point =
    let
        l = List.length snake
    in
        point :: (List.take (l-1) snake)

head : Snake -> Point
head snake =
    Maybe.withDefault initialPoint (List.head snake)

moveUp : Model -> Model
moveUp model =
    let
        h = head model.snake
        point' = newPoint h.x (h.y + 10)
    in
        { model | snake = mutateSnake model.snake point' }

moveDown : Model -> Model
moveDown model =
    let
        h = head model.snake
        point' = newPoint h.x (h.y - 10)
    in
        { model | snake = mutateSnake model.snake point' }

moveLeft : Model -> Model
moveLeft model =
    let
        h = head model.snake
        point' = newPoint (h.x - 10) h.y
    in
        { model | snake = mutateSnake model.snake point' }

moveRight : Model -> Model
moveRight model =
    let
        h = head model.snake
        point' = newPoint (h.x + 10) h.y
    in
        { model | snake = mutateSnake model.snake point' }

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
