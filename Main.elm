module Main exposing (..)

import Html.App as App
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Task
import Window
import Random exposing (initialSeed, Seed, Generator, generate)
import Debug exposing (log)
import Json.Encode as Json
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push

host : String
host = "http://localhost:4000"

socketServer : String
socketServer = "ws://localhost:4000/socket/websocket"

channelName : String
channelName = "room:lobby"



type alias Model =
    { snake : Snake
    , snake2 : Snake
    , food : Point
    , origin : (Int, Int)
    , maxX : Int
    , maxY : Int
    , seed : Seed
    , phxSocket : Phoenix.Socket.Socket Msg
    }

type alias Snake =
    { id : String
    , body : List Point
    , direction : Direction
    , color : String
    }

type Msg = TimeUpdate  Time
      | KeyUp KeyCode
      | KeyDown KeyCode
      | OriginRecalculate Window.Size
      | NextFood Point
      | PhoenixMsg (Phoenix.Socket.Msg Msg)
      | JoinChannel
      | SnakeCommand Json.Value
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
    , phxSocket = initPhxSocket
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
        KeyDown keycode ->
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
        JoinChannel ->
            let
                channel = Phoenix.Channel.init channelName
                ( phxSocket, phxCmd ) = Phoenix.Socket.join channel model.phxSocket
                a = Debug.log "joiining channel" channel
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )
        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )
        SnakeCommand _ ->
            model ! []


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
--        a = Debug.log "view model" model.snake
        food = viewFood (toBrowserCoordinates model.origin model.food)
        snake = List.map (\p -> p |> toBrowserCoordinates model.origin |> (viewSnakeSegment model.snake.color)) model.snake.body
        snake2 = List.map (\p -> p |> toBrowserCoordinates model.origin |> (viewSnakeSegment model.snake2.color)) model.snake2.body
    in
        div []
        [
         text <| (toString model.food) ++ (toString <| head model.snake)
        , button [ onClick JoinChannel ] [ text "Join lobby" ]
        , div [] (food :: (snake ++ snake2))
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Keyboard.downs KeyDown
        , Window.resizes recalculateOriginMsg
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , AnimationFrame.diffs (\time ->
                                    if ((round time) `rem` 1 == 0) then
                                        TimeUpdate time
                                    else
                                        None)
        ]


moveHead : Model -> Snake -> Point -> Model
moveHead model snake point =
    let
        snake' = { snake | body = (point :: snake.body) }
        --a = Debug.log "moveHead" snake'
    in
        updateSnake model snake'

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
        model' = moveSnake model model.snake
        (model'', cmd) = checkForFood model' model'.snake
        model''' = moveSnake model'' model''.snake2
        (model'''', cmd') = checkForFood model''' model'''.snake2
        -- (model'', cmd'') =
        --      model'
        --          |> flip moveSnake model.snake2
        --          |> flip checkForFood model.snake2
    in
        model'''' ! [cmd, cmd']

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
    let
        a = 1 -- Debug.log "eatFood" model
    in
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
        l = List.length snake.body
        snake' = { snake | body = (List.take (l-1) snake.body) }
        a =  1 --Debug.log "moveTail" snake'
    in
        (updateSnake model snake') ! []


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case keyCode of
        38 ->
            changeDirection model model.snake Up
        40 ->
            changeDirection model model.snake Down
        37 ->
            changeDirection model model.snake Left
        39 ->
            changeDirection model model.snake Right
        87 ->
            changeDirection model model.snake2 Up
        83 ->
            changeDirection model model.snake2 Down
        65 ->
            changeDirection model model.snake2 Left
        68 ->
            changeDirection model model.snake2 Right
        _ ->
            model

keyDown : KeyCode -> Model -> Snake -> Model
keyDown keyCode model snake =
    model

changeDirection : Model -> Snake -> Direction -> Model
changeDirection model snake direction =
    let
        snake' = { snake | direction = direction }
    in
        if snake.id == "foo" then
            { model | snake = snake' }
        else
            { model | snake2 = snake' }


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
    if snake.id == "foo" then
        { model | snake = snake }
    else
        { model | snake2 = snake }

initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "snake:command" channelName SnakeCommand
