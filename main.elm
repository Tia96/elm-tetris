import Browser
import Html exposing (..)
import Array

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

type alias Board = Array Array(CellState)

type alias Pos =  {x : Int, y : Int}

type alias Model =
    { board : Board
    , nowMino : Mino
    , minos : List (Mino)
    }


type Msg
    = Update

minoPos : MinoType -> List(Pos)
minoPos type_ =
    case type_ of
        T -> [{x=0,y=0},{x=1,y=0},{x=2,y=0},{x=1,y=1}]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Update ->
            (updateModel model, Cmd.none)

replace : Pos -> Board -> Int
replace {x, y} board =
    Array.set y (Array.get y board |> Array.set x Block) board

putMinoToBoard : Board -> Mino -> Board
putMinoToBoard board mino =
    List.foldr (replace board) board getEachPos mino

updateModel : Model -> Model
updateModel model =
    if isCollide <| moveMino Down model.nowMino == True then
       { model 
       | board = putMinoToBoard model.board model.nowMino
       , nowMino = newMino
       , minos = model.minos ++ model.nowMino
       }
    else
        { model
        | nowMino = 
            { nowMino
            | topPos =
                { y = model.nowMino.topPosy + 1}
            } 
        }

updateNowMino : Model -> Mino
updateNowMino model =
    if isCollide <| moveMino Down model.nowMino == True then
        addBoard

fixMino : Mino -> Board -> Board

updateState : Pos -> CellState
updateState pos =
    let
        upState = getState
    in

moveMino : Move -> Mino -> Mino
moveMino move mino =
    case move of
        Down -> {mino | topPos = {mino.topPos.x, mino.topPos.y+1}}
        Left -> {mino | topPos = {mino.topPos.x-1, mino.topPos.y}}
        Right -> {mino | topPos = {mino.topPos.x+1, mino.topPos.y}}

getEachPos : Mino -> List(Pos)
getEachPos mino =
    List.map (+) mino.topPos (minoPos mino.type_)

isCollide : Model -> Mino -> Bool
isCollide model mino =
    List.map (\pos -> getState pos model) (getEachPos mino)
    |> List.filter ((\=) Empty)
    |> List.length > 0

getState : Pos -> Model -> CellState
getState pos model =
    Array.get pos.y model.board
    |> Array.get pos.x

minoToCell : Pos -> Mino -> List(Pos)

view : Model -> Html Msg
view model =
    div []
        [ text "New Html Program" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : (Model, Cmd Msg)
init = 
    (Model modelInitialValue, Cmd.none)
