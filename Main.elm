module Main exposing (..)

import Html.App as App
import Html exposing (Html, text, div)
import Html.Attributes exposing (id, style)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Task
import Window
import Random exposing (initialSeed, Seed, Generator, generate)
import Debug exposing (log)


type alias Model =
    { snake : Snake
    , food : Point
    , origin : (Int, Int)
    , maxX : Int
    , maxY : Int
    , seed : Seed
    }

type alias Snake =
    { body : List Point
    , direction : Direction
    , color : String
    }

type Msg = TimeUpdate  Time
      | KeyUp KeyCode
      | KeyDown KeyCode
      | OriginRecalculate Window.Size
      | NextFood Point
      | None

type Direction = Up | Down | Left | Right

type alias Point = { x : Int
                   , y : Int
                   }

foodColor : String
foodColor = "red"

snakeColor : String
snakeColor = "green"

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
    { snake = newSnake [initialPoint, (newPoint 501 0), (newPoint 502 0)] Left "green"
    , food = initialFoodPoint
    , origin = (0, 0)
    , maxX = 100
    , maxY = 100
    , seed = initialSeed 1000
    }

init : (Model, Cmd Msg)
init =
    (model, initialOrigin)

initialOrigin : Cmd Msg
initialOrigin =
    Task.perform  (\_ -> None) recalculateOriginMsg Window.size

recalculateOriginMsg : Window.Size -> Msg
recalculateOriginMsg size =
    OriginRecalculate size

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        None ->
            model ! []
        KeyUp keycode ->
            (keyUp keycode model) ! []
        KeyDown keyCode ->
            model ! []
        TimeUpdate t ->
            nextStep model
        OriginRecalculate size ->
            (recalculateOrigin model size) ! []
        NextFood point ->
            let
                a = Debug.log "next food" <| toString point
            in
                (nextFood model point) ! []


recalculateOrigin : Model -> Window.Size -> Model
recalculateOrigin model size =
    let
        left = (size.width // 2)
        top = (size.height // 2)
    in
        { model | origin = (left, top)
        , maxX = left
        , maxY = top
        }

toPixel : Int -> String
toPixel x =
    (toString x) ++ "px"

toBrowserCoordinates : (Int, Int) -> Point -> (Int, Int)
toBrowserCoordinates origin p =
    let
        (x, y) = origin
    in
        (x + p.x, y - p.y)


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
        food = viewFood (toBrowserCoordinates model.origin model.food)
        snake = List.map (\p -> p |> toBrowserCoordinates model.origin |> viewSnakeSegment) model.snake.body
    in
        div []
        [
         text <| (toString model.food) ++ (toString <| head model.snake)
         ,div [] (food :: snake)
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        , Window.resizes recalculateOriginMsg
        , AnimationFrame.diffs (\time ->
                                    if ((round time) `rem` 1 == 0) then
                                        TimeUpdate time
                                    else
                                        None)
        ]


moveHead : Model -> Point -> Model
moveHead model point =
    let
        snake = newSnake (point :: model.snake.body) model.snake.direction model.snake.color
    in
        { model | snake = snake }


head : Snake -> Point
head snake =
    Maybe.withDefault initialPoint (List.head snake.body)

moveUp : Model -> Model
moveUp model =
    let
        h = head model.snake
        point' = if (h.y + 1 > model.maxY) then
                     newPoint h.x (0 - model.maxY)
                 else
                     newPoint h.x (h.y + 1)
    in
        moveHead model point'

moveDown : Model -> Model
moveDown model =
    let
        h = head model.snake
        point' = if (h.y - 1 < (0 - model.maxY)) then
                     newPoint h.x model.maxY
                 else
                     newPoint h.x (h.y - 1)
    in
        moveHead model point'

moveLeft : Model -> Model
moveLeft model =
    let
        h = head model.snake
        point' = if (h.x - 1 < (0 - model.maxX)) then
                     newPoint model.maxX h.y
                 else
                     newPoint (h.x - 1) h.y
    in
        moveHead model point'

moveRight : Model -> Model
moveRight model =
    let
        h = head model.snake
        point' = if (h.x + 1 > model.maxX) then
                     newPoint (0 - model.maxX) h.y
                 else
                     newPoint (h.x + 1) h.y
    in
        moveHead model point'


moveSnake : Model -> Model
moveSnake model =
    case model.snake.direction of
        Up ->
            moveUp model
        Down ->
            moveDown model
        Left ->
            moveLeft model
        Right ->
            moveRight model

nextStep : Model -> (Model, Cmd Msg)
nextStep model =
    model
        |> moveSnake
        |> checkForFood


checkForFood : Model -> (Model, Cmd Msg)
checkForFood model =
    if canEatFood model then
        eatFood model
    else
        moveTail model


randomPoint : Model -> Generator Point
randomPoint model =
    let
        --r = Random.pair (Random.int (0 - model.maxX) model.maxX) (Random.int (0 - model.maxY) model.maxY)
        r = Random.pair (Random.int (0 - model.maxX) model.maxX) (Random.int 0 0)
    in
        Random.map (\(x,y) -> newPoint x y) r

eatFood : Model -> (Model, Cmd Msg)
eatFood model =
    (model, generate NextFood (randomPoint model) )

nextFood : Model -> Point -> Model
nextFood model point =
    { model | food = point }


canEatFood : Model -> Bool
canEatFood model =
    (head model.snake) == model.food

growSnake : Model -> Model
growSnake model =
    model

moveTail : Model -> (Model, Cmd Msg)
moveTail model =
    let
        l = List.length model.snake.body
        snake = newSnake (List.take (l-1) model.snake.body) model.snake.direction model.snake.color
    in
        { model | snake = snake } ! []


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
    { model | snake = newSnake model.snake.body direction model.snake.color }


newPoint : Int -> Int -> Point
newPoint x y =
    { x = x
    , y = y
    }

newSnake : List Point -> Direction -> String -> Snake
newSnake body direction color =
    { body = body
    , direction = direction
    , color = color
    }
