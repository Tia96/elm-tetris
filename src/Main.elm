module Main exposing (init)

import Browser
import Browser.Events exposing (onKeyDown, onAnimationFrame)
import Html exposing (Html)
import Html.Attributes exposing (style)
import String
import Array exposing (Array)
import Canvas exposing (Shape, shapes, rect)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Text as Text exposing (font, align)
import Json.Decode
import Color
import Time exposing (Posix, posixToMillis)
import Task
import Random

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type CellState 
    = Block
    | Wall
    | Empty

type MinoType
    = T
    | I
    | O
    | L
    | J
    | S
    | Z

type alias Mino =
    { type_ : MinoType
    , minoPos : List(Pos)
    , topPos : Pos
    }

type Move
    = Down
    | Right
    | Left
    | RRotate
    | LRotate

type alias Board = Array (Array CellState)

type alias Pos = 
    { x : Int
    , y : Int
    }

type alias Model =
    { startTime : Posix
    , updateMills : Int
    , board : Board
    , headMino : Mino
    , score : Int
    }


type Msg
    = Update Posix
    | UpdateRight
    | UpdateLeft
    | UpdateRRotate
    | UpdateLRotate
    | NewMino MinoType
    | SetSystemTime Posix
    | Nothing


init : () -> (Model, Cmd Msg)
init _ = 
    ({startTime = Time.millisToPosix 0, updateMills = 500, board = initBoard, headMino = newMino T, score = 0}, setInitialModel)

initBoard : Board
initBoard = 
    Array.append 
    (Array.repeat 15 (Array.fromList (List.append (Wall :: (List.repeat 10 Empty)) [Wall])))
    (Array.repeat 1 (Array.repeat 12 Wall))
newMino : MinoType -> Mino
newMino type_ =
    { topPos = {x = 5, y = 1}
    , minoPos = generateMinoPos type_
    , type_ = type_
    }

generateMinoPos : MinoType -> List(Pos)
generateMinoPos type_ =
    case type_ of
        T -> [{x=0,y=0},{x=-1,y=0},{x=1,y=0},{x=0,y=-1}]
        I -> [{x=0,y=0},{x=0,y=1},{x=0,y=2},{x=0,y=-1}]
        O -> [{x=0,y=0},{x=-1,y=0},{x=-1,y=-1},{x=0,y=-1}]
        L -> [{x=0,y=0},{x=0,y=-1},{x=0,y=1},{x=1,y=1}]
        J -> [{x=0,y=0},{x=0,y=-1},{x=0,y=1},{x=-1,y=1}]
        S -> [{x=0,y=0},{x=0,y=-1},{x=1,y=-1},{x=-1,y=0}]
        Z -> [{x=0,y=0},{x=0,y=-1},{x=-1,y=-1},{x=1,y=0}]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Update time -> updateModel time model
        UpdateRight -> (moveModel Right model, Cmd.none)
        UpdateLeft -> (moveModel Left model, Cmd.none)
        UpdateRRotate -> (moveModel RRotate model, Cmd.none)
        UpdateLRotate -> (moveModel LRotate model, Cmd.none)
        NewMino type_ -> ({model | headMino = newMino type_}, Cmd.none)
        SetSystemTime time -> ({model | startTime = time}, Cmd.none)
        Nothing -> (model, Cmd.none)


replace : Pos -> Board -> Board
replace {x, y} board =
    Array.set y (
        Array.set x Block 
        <| Maybe.withDefault Array.empty  
        <| Array.get y board 
    ) board

putMinoToBoard : Board -> Mino -> Board
putMinoToBoard board mino =
    List.foldr replace board (getEachPos mino)

updateModel : Posix -> Model -> (Model, Cmd Msg)
updateModel time model =
    let
        nextMino = moveMino Down model.headMino
        delta = posixToMillis time - posixToMillis model.startTime
    in
        if delta > model.updateMills then
            if (isCollide model nextMino) == True then
                ({ model
                | startTime = time
                , board = deleteLines (putMinoToBoard model.board model.headMino)
                , score = model.score + calcScore (putMinoToBoard model.board model.headMino)
                },
                Random.generate NewMino rand)
            else
                ({ model | startTime = time, headMino = nextMino }, Cmd.none)
        else
            (model, Cmd.none)

moveModel : Move -> Model -> Model
moveModel move model =
    let
        nextMino = moveMino move model.headMino
    in
        if (isCollide model nextMino) == True then
            model
        else
            { model | headMino = nextMino }

moveMino : Move -> Mino -> Mino
moveMino move mino =
    case move of
        Down -> {mino | topPos = addPos mino.topPos {x=0, y=1}}
        Left -> {mino | topPos = addPos mino.topPos {x=-1, y=0}}
        Right -> {mino | topPos = addPos mino.topPos {x=1, y=0}}
        RRotate -> {mino | minoPos = rotatePos RRotate mino.minoPos}
        LRotate -> {mino | minoPos = rotatePos LRotate mino.minoPos}

addPos : Pos -> Pos -> Pos
addPos pos1 pos2 =
    { x = pos1.x + pos2.x, y = pos1.y + pos2.y }

rotatePos : Move -> List(Pos) -> List(Pos)
rotatePos move poses =
    case move of
        RRotate -> List.map (\pos -> {x=pos.y, y=-pos.x}) poses
        LRotate -> List.map (\pos -> {x=-pos.y, y=pos.x}) poses
        _ -> poses

getEachPos : Mino -> List Pos
getEachPos mino =
    List.map (addPos mino.topPos) mino.minoPos

isCollide : Model -> Mino -> Bool
isCollide model mino =
    List.map (\pos -> getState pos model) (getEachPos mino)
    |> List.filter ((/=) Empty)
    |> List.length
    |> (<) 0

getState : Pos -> Model -> CellState
getState pos model =
    Array.get pos.y model.board
    |> Maybe.withDefault Array.empty
    |> Array.get pos.x
    |> Maybe.withDefault Block

deleteLines : Board -> Board
deleteLines board =
    Array.map (\array -> Array.length(Array.filter ((==) Block) array)) board
    |> Array.toIndexedList
    |> List.filter (\pair -> Tuple.second pair == 10)
    |> List.map Tuple.first
    |> List.foldl deleteLine board

deleteLine : Int -> Board -> Board
deleteLine k board = 
    Array.append 
    (Array.repeat 1 (Array.fromList (List.append (Wall :: (List.repeat 10 Empty)) [Wall])))
    (Array.append (Array.slice 0 k board) (Array.slice (k+1) (Array.length board) board))

calcScore : Board -> Int
calcScore board =
    Array.map (\array -> Array.length(Array.filter ((==) Block) array)) board
    |> Array.filter ((==) 10)
    |> Array.length

view : Model -> Html Msg
view model =
    Canvas.toHtml (360 , 530)
        [ ]
        [ shapes [ fill Color.white ] [rect (0, 0) 360 530]
        , shapes [ fill Color.gray ] (shapeBoard Block model)
        , shapes [ fill Color.brown ] (shapeBoard Wall model)
        , shapes [ fill Color.black ] (shapeMino model) 
        , Canvas.text [ fill Color.red, font {size = 17, family = "monospace"}, align Text.Left ] (0, 500)  (String.append "Score: " (String.fromInt model.score))
        , Canvas.text [ fill Color.black, font {size = 17, family = "monospace"}, align Text.Left] (0, 520) "UP: Rotate, RightLeft: Move"
        ]

shapeBoard : CellState -> Model -> List Shape
shapeBoard cellState model =
    Array.indexedMap (\y array -> Array.indexedMap (\x state -> (Pos x y, state)) array) model.board
    |> Array.toList
    |> List.map Array.toList
    |> List.concat
    |> List.filter (\item -> Tuple.second item == cellState)
    |> List.map Tuple.first
    |> List.map (\pos -> rect (toFloat(pos.x)*30, toFloat(pos.y)*30) 28 28)

shapeMino : Model -> List Shape
shapeMino model =
    List.map (\pos -> rect (toFloat(pos.x)*30, toFloat(pos.y)*30) 28 28) (getEachPos model.headMino)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (keyDecoder)
        , onAnimationFrame (\time -> Update time)
        ]

keyDecoder : Json.Decode.Decoder Msg
keyDecoder = Json.Decode.map toKey (Json.Decode.field "key" Json.Decode.string)
toKey : String -> Msg
toKey keyValue =
    case keyValue of
        "ArrowLeft" ->
            UpdateLeft
        "ArrowRight" ->
            UpdateRight
        "ArrowUp" ->
            UpdateRRotate
        _ ->
            Nothing

rand : Random.Generator MinoType
rand = Random.uniform T [ I, O, L, J, S, Z]

setInitialModel : Cmd Msg
setInitialModel = 
    Cmd.batch 
    [ Task.perform SetSystemTime Time.now
    , Random.generate NewMino rand
    ]