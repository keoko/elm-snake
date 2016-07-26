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
import Json.Decode exposing (Decoder, at, decodeString, decodeValue, succeed, int, string, object1, object4, list, (:=))

host : String
host = "http://localhost:4000"

socketServer : String
socketServer = "ws://localhost:4000/socket/websocket"

channelName : String
channelName = "room:lobby"



type alias Model =
    { snake : Snake
    , snakes : List Snake
    , food : Point
    , origin : (Int, Int)
    , maxX : Int
    , maxY : Int
    , seed : Seed
    , phxSocket : Phoenix.Socket.Socket Msg
    }

type alias SnakeId = String

type alias Snake =
    { id : SnakeId
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
    , snakes = [ newSnake "bar" [(newPoint 300 0), (newPoint 301 0), (newPoint 302 0)] Left "blue"
               , newSnake "baz" [(newPoint 300 20), (newPoint 301 20), (newPoint 302 20)] Right "orange"
               , newSnake "quux" [(newPoint 300 50), (newPoint 301 50), (newPoint 302 50)] Up "violet"
               ]

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
                a = False --a = Debug.log "next food" <| toString point
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
        SnakeCommand raw ->
            model ! []
            -- case decodeValue snakeCommandDecoder raw of
            --     Ok (s,d) ->
            --         moveSnake s model
            --     _ ->
            --         let
            --             l = Debug.log "error when decoding snake command" raw
            --         in
            --             model


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


viewSnake : Model -> Snake -> List (Html Msg)
viewSnake model snake =
    List.map (\p -> p
             |> toBrowserCoordinates model.origin
             |> (viewSnakeSegment snake.color))
        snake.body

view : Model -> Html Msg
view model =
    let
--        a = Debug.log "view model" model.snake
        food = viewFood (toBrowserCoordinates model.origin model.food)
        snake = viewSnake model model.snake
        snakes = List.concatMap (viewSnake model) model.snakes
    in
        div []
        [
         text <| (toString model.food) ++ (toString <| head model.snake)
        , button [ onClick JoinChannel ] [ text "Join lobby" ]
        , div [] (food :: (snake ++ snakes))
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


moveSnakeInItsDirection : Snake -> Model -> Model
moveSnakeInItsDirection snake model =
    case snake.direction of
        Up ->
            moveUp model snake
        Down ->
            moveDown model snake
        Left ->
            moveLeft model snake
        Right ->
            moveRight model snake



moveSnake : Snake -> Model -> (Model, Cmd Msg)
moveSnake snake model =
    model
        |> mayChangeDirection snake
        |> (\ m' -> moveSnakeInItsDirection (getSnake snake m') m')
        |> (\ m'' -> checkForFood (getSnake snake m'') m'')

nextStep : Model -> (Model, Cmd Msg)
nextStep model =
    moveSnakes model

moveSnakes : Model -> (Model, Cmd Msg)
moveSnakes model =
    let
       snakes = model.snake :: model.snakes

       f s (m, c) = m
                  |> moveSnake s
                  |> (\ (m',c') -> m' ! [c, c'])
    in
        List.foldr f (model ! []) snakes

checkForFood : Snake -> Model -> (Model, Cmd Msg)
checkForFood snake model =
    if canEatFood model snake then
        eatFood model snake
    else
        moveTail model snake

intToDirection : Int -> Direction
intToDirection x =
    if x == 0 then Up
    else if x == 1 then Down
    else if x == 2 then Right
    else Left

randomDirection : Model -> Generator Direction
randomDirection model =
    Random.map intToDirection (Random.int 0 3)

randomPoint : Model -> Generator Point
randomPoint model =
    let
        --r = Random.pair (Random.int (0 - model.maxX) model.maxX) (Random.int (0 - model.maxY) model.maxY)
        -- todo: just for testing, it has been fixed to y = 0, the test snake eating food
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
        if snake.id == model.snake.id then
            { model | snake = snake' }
        else
            updateSnake model snake'

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
    if snake.id == model.snake.id then
        { model | snake = snake }
    else
        { model | snakes = (List.map (\s -> if s.id == snake.id then snake else s) model.snakes )}


getSnake : Snake -> Model -> Snake
getSnake snake model =
    if snake.id == model.snake.id then
        model.snake
    else
        let
            snakes = List.filter (\s -> s.id == snake.id) model.snakes
        in
            Maybe.withDefault model.snake <| List.head snakes

initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init socketServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "snake:command" channelName SnakeCommand

mayChangeDirection : Snake -> Model -> Model
mayChangeDirection snake model =
    if snake.id == model.snake.id then
        model
    else
        let
            (direction, seed) = Random.step (randomDirection model) model.seed
            snake' = { snake | direction = direction }
            model' = { model | seed = seed }
        in
            updateSnake model' snake'


-- snakeCommandDecoder : Decoder (SnakeId, Direction)
-- snakeCommandDecoder =
--     object2 (,)
--         ("snakeId" := string)
--         ("direction" := string)
