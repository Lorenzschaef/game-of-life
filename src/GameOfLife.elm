module GameOfLife exposing (..)

import Browser
import Html exposing (Html, button, div, table, td, text, tr)
import Html.Attributes exposing (class, style)
import Array exposing (Array)
import Html.Events exposing (onClick)
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
    }

type alias Board =
    { width: Int
    , height: Int
    , cells: Array Cell
    }

type alias Cell = Bool

type Msg
    = Toggle Int
    | Tick
    | Pause Bool
    | ChangeSpeed Int
    | Clear


init : () -> (Model, Cmd Msg)
init _ = (
    { board = initBoard
    , paused = True
    , speed = 2
    }
    , Cmd.none )

initBoard : Board
initBoard =
    { width = 100
    , height = 100
    , cells = Array.repeat (100 * 100) False
    }

subscriptions model =
    if model.paused
        then Sub.none
        else Time.every (toFloat (1000 // (model.speed * 2))) (always Tick)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Toggle index -> ({ model | board = toggleCell model.board index }, Cmd.none)
        Tick -> ({ model | board = tick model.board}, Cmd.none)
        Pause bool -> ({ model | paused = bool }, Cmd.none)
        ChangeSpeed factor -> ({ model | speed = factor}, Cmd.none)
        Clear -> ({ model | board = initBoard }, Cmd.none)

toggleCell : Board -> Int -> Board
toggleCell board index =
    case getCell board index of
        Just value -> setCell board index (not value)
        Nothing -> board

getCell : Board -> Int -> Maybe Cell
getCell board index = Array.get index board.cells

setCell : Board -> Int -> Cell -> Board
setCell board index value = { board | cells = Array.set index value board.cells }

neighborsOf : Board -> Int -> List Cell
neighborsOf board index =
    [ getCell board (index - board.width - 1)
    , getCell board (index - board.width)
    , getCell board (index - board.width + 1)
    , getCell board (index - 1)
    , getCell board (index + 1)
    , getCell board (index + board.width - 1)
    , getCell board (index + board.width)
    , getCell board (index + board.width + 1)
    ]
    |> List.filterMap identity

countAliveNeighbors : Board -> Int -> Int
countAliveNeighbors board index =
    neighborsOf board index
    |> List.filter (\x -> x== True)
    |> List.length

tickCell : Board -> Int -> Cell -> Cell
tickCell board index value =
    let
        count = countAliveNeighbors board index
    in
        case value of
            False ->
                count == 3
            True ->
                count == 2 || count == 3


tick : Board -> Board
tick board = { board | cells = Array.indexedMap (tickCell board) board.cells }

-- view

view : Model -> Html Msg
view model = div []
    [ table
        [ style "width" ((String.fromInt (model.board.width * 20)) ++ "px")
        ] (viewCells model.board)
    , div [ class "controls" ]
        [ button [ onClick Tick ] [ text "Step" ]
        , button [ onClick (Pause (not model.paused)) ] [ text (if model.paused then "Play" else "Pause") ]
        , button [ onClick (ChangeSpeed (1 + modBy 5 model.speed)) ] [ text <| "Speed " ++ (String.fromInt model.speed) ]
        , button [ onClick Clear ] [ text "Clear" ]
        ]
    ]

cell : Int -> Bool -> Html Msg
cell index state = td
    ([ onClick (Toggle index)
    ] ++ (if state then [class "alive"] else [])
     ) []


viewCells : Board -> List (Html Msg)
viewCells board =
    Array.indexedMap cell board.cells
    |> Array.toList
    |> chunksOfLeft board.width
    |> List.map (\cells -> tr [] cells)


chunksOfLeft : Int -> List a -> List (List a)
chunksOfLeft k xs =
    if k == 0 then
        [ [] ]
    else if k < 0 then
        []
    else if List.length xs > k then
        List.take k xs :: chunksOfLeft k (List.drop k xs)
    else
        [ xs ]
