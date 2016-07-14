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
    , food : Point
    }

type alias Snake = List Point

type Msg = TimeUpdate  Time
      | KeyUp KeyCode
      | KeyDown KeyCode

type Direction = Up | Down | Left | Right

type alias Point = { x : Int
                   , y : Int
                   }

foodColor : String
foodColor = "red"

snakeColor : String
snakeColor = "green"

maxX : Int
maxX = 1000

maxY : Int
maxY = 500


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

initialFoodPoint : Point
initialFoodPoint = newPoint 200 0

model : Model
model =
    { snake = [initialPoint, (newPoint 501 0), (newPoint 502 0)]
    , direction = Left
    , food = initialFoodPoint
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
            (nextStep model) ! []


toPixel : Int -> String
toPixel x =
    (toString x) ++ "px"

toPixelCoordinates : Point -> (Int, Int)
toPixelCoordinates p =
    let
        {x, y} = newPoint 200 50 
    in
        (p.x - x, y - p.y)


viewBlock : (Int, Int) -> String -> Html Msg
viewBlock (left, top) color =
    div
    [ id "rectangle"
    , style
          [ ("position", "absolute")
          , ("top", toPixel top)
          , ("left", toPixel left)
          , ( "width", "10px" )
          , ( "height", "10px" )
          , ( "background-color", color )
          ]
    ] []

viewFood : (Int, Int) -> Html Msg
viewFood p = viewBlock p foodColor

viewSnakeSegment : (Int, Int) -> Html Msg
viewSnakeSegment p = viewBlock p snakeColor

view : Model -> Html Msg
view model =
    let
        snake = List.map (\p -> p |> toPixelCoordinates |> viewSnakeSegment) model.snake
    in
        div [] ((viewFood (toPixelCoordinates initialFoodPoint)) :: snake)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        , AnimationFrame.diffs TimeUpdate
        ]

moveHead : Model -> Point -> Model
moveHead model point =
    { model | snake = (point :: model.snake) }


head : Snake -> Point
head snake =
    Maybe.withDefault initialPoint (List.head snake)

moveUp : Model -> Model
moveUp model =
    let
        h = head model.snake
        point' = if (h.y + 10 > maxY) then
                     newPoint h.x (0 - maxY)
                 else
                     newPoint h.x (h.y + 10)
    in
        moveHead model point'

moveDown : Model -> Model
moveDown model =
    let
        h = head model.snake
        point' = if (h.y - 10 < (0 - maxY)) then
                     newPoint h.x maxY
                 else
                     newPoint h.x (h.y - 10)
    in
        moveHead model point'

moveLeft : Model -> Model
moveLeft model =
    let
        h = head model.snake
        point' = if (h.x - 10 < (0 - maxX)) then
                     newPoint maxX h.y
                 else
                     newPoint (h.x - 10) h.y
    in
        moveHead model point'

moveRight : Model -> Model
moveRight model =
    let
        h = head model.snake
        point' = if (h.x + 10 > maxX) then
                     newPoint (0 - maxX) h.y
                 else
                     newPoint (h.x + 10) h.y
    in
        moveHead model point'


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

nextStep : Model -> Model
nextStep model =
    model
        |> moveSnake
        |> checkForFood


checkForFood : Model -> Model
checkForFood model =
    if canEatFood model then
        eatFood model
    else
        moveTail model

eatFood : Model -> Model
eatFood model =
    { model | food = nextFood model.food }

nextFood : Point -> Point
nextFood food =
    initialFoodPoint


canEatFood : Model -> Bool
canEatFood model =
    (head model.snake) == model.food

growSnake : Model -> Model
growSnake model =
    model

moveTail : Model -> Model
moveTail model =
    let
        l = List.length model.snake
    in
        { model | snake = List.take (l-1) model.snake }


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

changeDirection : Model -> Direction -> Model
changeDirection model  direction =
    { model | direction = direction }


newPoint : Int -> Int -> Point
newPoint x y =
    { x = x
    , y = y
    }
