import Browser
import Html exposing (..)
import Array exposing (Array)
import Canvas exposing (Shape, fill, shapes, rect)
import Color

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

type MinoState
    = Moving
    | Stopped

type MinoType
    = T
--    | Z
--    | O

type alias Mino =
    { type_ : MinoType
    , topPos : Pos
    , state : MinoState
    }

type Move
    = Down
    | Right
    | Left
--    | RRotate
--    | LRotate

type alias Board = Array (Array CellState)

type alias Pos = 
    { x : Int
    , y : Int
    }

type alias Model =
    { board : Board
    , nowMino : Mino
    , minos : List Mino
    }


type Msg
    = Update


initBoard : Board
initBoard = 
    Array.repeat 15 <| Array.fromList(Wall :: Empty :: Empty :: Empty :: Empty :: Empty :: Wall :: [])

newMino : Mino
newMino =
    { topPos = {x = 3, y = 0}
    , state = Moving
    , type_ = T
    }

init : () -> (Model, Cmd Msg)
init _ = 
    ({ board = initBoard
    , nowMino = newMino
    , minos = []
    }, Cmd.none)

minoPos : MinoType -> List(Pos)
minoPos type_ =
    case type_ of
        T -> [{x=0,y=0},{x=1,y=0},{x=2,y=0},{x=1,y=1}]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Update ->
            (updateModel model, Cmd.none)

replace : Pos -> Board -> Board
replace {x, y} board =
    Array.set y (Array.set x Block <| Maybe.withDefault Array.empty  <| Array.get y board ) board

putMinoToBoard : Board -> Mino -> Board
putMinoToBoard board mino =
    --List.foldr (\pos b -> replace pos b) board (getEachPos mino)
    List.foldr replace board (getEachPos mino)

updateModel : Model -> Model
updateModel model =
    if (isCollide model <| moveMino Down model.nowMino) == True then
       { model 
       | board = putMinoToBoard model.board model.nowMino
       , nowMino = newMino
       , minos = model.nowMino :: model.minos
       }
    else
        { model | nowMino = moveMino Down model.nowMino }


{--
updateState : Point -> CellState
updateState pos =
    let
        upState = getState
    in
--}
moveMino : Move -> Mino -> Mino
moveMino move mino =
    case move of
        Down -> {mino | topPos = down mino.topPos}
        Left -> {mino | topPos = left mino.topPos}
        Right -> {mino | topPos = right mino.topPos}

down : Pos -> Pos
down {x, y} = {x = x, y = y+1}

right : Pos -> Pos
right {x, y} = {x = x+1, y = y}

left : Pos -> Pos
left {x, y} = {x = x-1, y = y}

addPos : Pos -> Pos -> Pos
addPos pos1 pos2 =
    { x = pos1.x + pos2.x, y = pos1.y + pos2.y }

getEachPos : Mino -> List Pos
getEachPos mino =
    List.map (addPos mino.topPos) (minoPos mino.type_)

isCollide : Model -> Mino -> Bool
isCollide model mino =
    List.map (\pos -> getState pos model) (getEachPos mino)
    |> List.filter ((/=) Empty)
    |> List.length
    |> (>) 0

getState : Pos -> Model -> CellState
getState pos model =
    Maybe.andThen (
        Array.get pos.y model.board
        |> Array.get pos.x
        |> Maybe.andThen Block
    )

--minoToCell : Pos -> Mino -> List Pos

view : Model -> Html Msg
view model =
    Canvas.toHtml (500 , 500)
        []
        [ shapes [ fill Color.gray ] shapeBoard ]

shapeBoard : Model -> List Shape
shapeBoard model =
    List.concatMap (\mino -> 
        List.indexedMap (\pos -> rect (pos.x * 30, pos.y * 30) 30 30) getEachPos mino
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
