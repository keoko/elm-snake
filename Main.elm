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
    , snake2 : Snake
    , food : Point
    , origin : (Int, Int)
    , maxX : Int
    , maxY : Int
    , seed : Seed
    }

type alias Snake =
    { id : String
    , body : List Point
    , direction : Direction
    , color : String
    }

type Msg = TimeUpdate  Time
      | KeyUp Snake KeyCode
      | KeyDown Snake KeyCode
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
    { snake = newSnake "foo" [initialPoint, (newPoint 501 0), (newPoint 502 0)] Left "green"
    , snake2 = newSnake "bar" [(newPoint 300 0), (newPoint 301 0), (newPoint 302 0)] Left "blue"
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
        KeyUp snake keycode ->
            (keyUp keycode model snake) ! []
        KeyDown snake keycode ->
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

viewSnakeSegment : String -> (Int, Int) -> Html Msg
viewSnakeSegment color p = viewBlock p color

view : Model -> Html Msg
view model =
    let
        food = viewFood (toBrowserCoordinates model.origin model.food)
        snake = List.map (\p -> p |> toBrowserCoordinates model.origin |> (viewSnakeSegment model.snake.color)) model.snake.body
        snake2 = List.map (\p -> p |> toBrowserCoordinates model.origin |> (viewSnakeSegment model.snake2.color)) model.snake2.body
    in
        div []
        [
         text <| (toString model.food) ++ (toString <| head model.snake)
         ,div [] (food :: (snake ++ snake2))
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups (KeyUp model.snake)
        , Keyboard.downs (KeyDown model.snake)
        , Window.resizes recalculateOriginMsg
        , AnimationFrame.diffs (\time ->
                                    if ((round time) `rem` 1 == 0) then
                                        TimeUpdate time
                                    else
                                        None)
        ]


moveHead : Model -> Snake -> Point -> Model
moveHead model snake point =
    let
        snake = newSnake snake.id (point :: snake.body) snake.direction snake.color
    in
        updateSnake model snake

head : Snake -> Point
head snake =
    Maybe.withDefault initialPoint (List.head snake.body)

moveUp : Model -> Snake -> Model
moveUp model snake =
    let
        h = head snake
        point' = if (h.y + 1 > model.maxY) then
                     newPoint h.x (0 - model.maxY)
                 else
                     newPoint h.x (h.y + 1)
    in
        moveHead model snake point'

moveDown : Model -> Snake -> Model
moveDown model snake =
    let
        h = head snake
        point' = if (h.y - 1 < (0 - model.maxY)) then
                     newPoint h.x model.maxY
                 else
                     newPoint h.x (h.y - 1)
    in
        moveHead model snake point'

moveLeft : Model -> Snake -> Model
moveLeft model snake =
    let
        h = head snake
        point' = if (h.x - 1 < (0 - model.maxX)) then
                     newPoint model.maxX h.y
                 else
                     newPoint (h.x - 1) h.y
    in
        moveHead model snake point'

moveRight : Model -> Snake -> Model
moveRight model snake =
    let
        h = head snake
        point' = if (h.x + 1 > model.maxX) then
                     newPoint (0 - model.maxX) h.y
                 else
                     newPoint (h.x + 1) h.y
    in
        moveHead model snake point'


moveSnake : Model -> Snake -> Model
moveSnake model snake =
    case snake.direction of
        Up ->
            moveUp model snake
        Down ->
            moveDown model snake
        Left ->
            moveLeft model snake
        Right ->
            moveRight model snake

nextStep : Model -> (Model, Cmd Msg)
nextStep model =
    moveSnakes model

moveSnakes : Model -> (Model, Cmd Msg)
moveSnakes model =
    let
        (model', cmd' ) =
            model
                |> flip moveSnake model.snake
                |> flip checkForFood model.snake

        (model'', cmd'') =
             model'
                 |> flip moveSnake model.snake2
                 |> flip checkForFood model.snake2
    in
        model' ! [cmd']

checkForFood : Model -> Snake -> (Model, Cmd Msg)
checkForFood model snake =
    if canEatFood model snake then
        eatFood model snake
    else
        moveTail model snake


randomPoint : Model -> Generator Point
randomPoint model =
    let
        --r = Random.pair (Random.int (0 - model.maxX) model.maxX) (Random.int (0 - model.maxY) model.maxY)
        r = Random.pair (Random.int (0 - model.maxX) model.maxX) (Random.int 0 0)
    in
        Random.map (\(x,y) -> newPoint x y) r

eatFood : Model -> Snake -> (Model, Cmd Msg)
eatFood model snake =
    (model, generate NextFood (randomPoint model) )

nextFood : Model -> Point -> Model
nextFood model point =
    { model | food = point }


canEatFood : Model -> Snake -> Bool
canEatFood model snake =
    (head snake) == model.food

growSnake : Model -> Model
growSnake model =
    model

moveTail : Model -> Snake -> (Model, Cmd Msg)
moveTail model snake =
    let
        l = List.length model.snake.body
        snake = newSnake snake.id (List.take (l-1) model.snake.body) model.snake.direction model.snake.color
    in
        (updateSnake model snake) ! []


keyUp : KeyCode -> Model -> Snake -> Model
keyUp keyCode model snake =
    case keyCode of
        38 ->
            changeDirection model snake Up
        40 ->
            changeDirection model snake Down
        37 ->
            changeDirection model snake Left
        39 ->
            changeDirection model snake Right
        _ ->
            model

keyDown : KeyCode -> Model -> Snake -> Model
keyDown keyCode model snake =
    model

changeDirection : Model -> Snake -> Direction -> Model
changeDirection model snake direction =
    model
    -- let
    --     snake' = { snake | direction = direction }
    -- in
    --     { model | snake = snake' }


newPoint : Int -> Int -> Point
newPoint x y =
    { x = x
    , y = y
    }

newSnake : String -> List Point -> Direction -> String -> Snake
newSnake id body direction color =
    { id = id
    , body = body
    , direction = direction
    , color = color
    }


updateSnake : Model -> Snake -> Model
updateSnake model snake =
    model
