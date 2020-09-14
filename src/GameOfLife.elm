module GameOfLife exposing (..)

import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Array exposing (Array)
import Html.Events exposing (onClick, onMouseDown, onMouseOver, onMouseUp)
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { board: Board
    , paused: Bool
    , speed: Int
    , paintMode: Maybe Bool
    }

type alias Board =
    { width: Int
    , height: Int
    , cells: Array (Array Cell)
    }

type alias Cell = Bool

type Msg
    = Toggle (Int, Int)
    | Tick
    | Pause Bool
    | ChangeSpeed Int
    | Clear
    | StartPaint Bool
    | StopPaint
    | Paint (Int, Int)


init : () -> (Model, Cmd Msg)
init _ = (
    { board = initBoard
    , paused = True
    , speed = 2
    , paintMode = Nothing
    }
    , Cmd.none )

initBoard : Board
initBoard =
    { width = 100
    , height = 100
    , cells = Array.repeat 100 <| Array.repeat 100 False
    }

subscriptions model =
    if model.paused
        then Sub.none
        else Time.every (toFloat (1000 // (model.speed * model.speed))) (always Tick)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Toggle index -> ({ model | board = toggleCell model.board index }, Cmd.none)
        Tick -> ({ model | board = tick model.board}, Cmd.none)
        Pause bool -> ({ model | paused = bool }, Cmd.none)
        ChangeSpeed factor -> ({ model | speed = factor}, Cmd.none)
        Clear -> ({ model | board = initBoard }, Cmd.none)
        StartPaint bool -> ({ model | paintMode = Just bool }, Cmd.none)
        StopPaint -> ({ model | paintMode = Nothing }, Cmd.none)
        Paint coords -> (
            case model.paintMode of
                Nothing -> model
                Just state -> { model | board = setCell model.board coords state }
            , Cmd.none)

toggleCell : Board -> (Int, Int) -> Board
toggleCell board index =
    case getCell board index of
        Just value -> setCell board index (not value)
        Nothing -> board

getCell : Board -> (Int, Int) -> Maybe Cell
getCell board (x, y) = board.cells |> Array.get x |> Maybe.andThen (Array.get y)

setCell : Board -> (Int, Int) -> Cell -> Board
setCell board (x, y) value =
    case Array.get x board.cells of
        Nothing -> board
        Just row ->
            { board | cells = Array.set x (Array.set y value row) board.cells }

neighborsOf : Board -> (Int, Int) -> List Cell
neighborsOf board (x, y) =
    [ getCell board (x - 1, y - 1)
    , getCell board (x - 1, y)
    , getCell board (x - 1, y + 1)
    , getCell board (x, y - 1)
    , getCell board (x, y + 1)
    , getCell board (x + 1, y - 1)
    , getCell board (x + 1, y)
    , getCell board (x + 1, y + 1)
    ]
    |> List.filterMap identity

countAliveNeighbors : Board -> (Int, Int) -> Int
countAliveNeighbors board coords =
    neighborsOf board coords
    |> List.filter (\x -> x== True)
    |> List.length

tickCell : Board -> (Int, Int) -> Cell -> Cell
tickCell board coords value =
    let
        count = countAliveNeighbors board coords
    in
        case value of
            False ->
                count == 3
            True ->
                count == 2 || count == 3


tick : Board -> Board
tick board = { board | cells = Array.indexedMap (\x row -> Array.indexedMap (\y cell -> tickCell board (x, y) cell) row) board.cells }

-- view

view : Model -> Html Msg
view model = div []
    [ table
        [ style "width" ((String.fromInt (model.board.width * 20)) ++ "px")
        ] (viewRows model.board.cells)
    , div [ class "controls" ]
        [ button [ onClick Tick ] [ text "Step" ]
        , button [ onClick (Pause (not model.paused)) ] [ text (if model.paused then "Play" else "Pause") ]
        , button [ onClick (ChangeSpeed (1 + modBy 5 model.speed)) ] [ text <| "Speed " ++ (String.fromInt model.speed) ]
        , button [ onClick Clear ] [ text "Clear" ]
        ]
    ]

viewCell : (Int, Int) -> Bool -> Html Msg
viewCell coords state = td
    ([ onClick (Toggle coords)
    , onMouseDown (StartPaint (not state))
    , onMouseUp (StopPaint)
    , onMouseOver (Paint coords)
    ] ++ (if state then [class "alive"] else [])
     ) []


viewCells : Int -> Array Cell -> List (Html Msg)
viewCells x row =
    Array.indexedMap (\y value -> viewCell (x, y) value) row
    |> Array.toList

viewRows : Array (Array Cell) -> List (Html Msg)
viewRows rows =
    Array.indexedMap viewCells rows
    |> Array.toList
    |> List.map (\row -> tr [] row)
