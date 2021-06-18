module GameOfLife exposing (..)

import Browser
import Html exposing (Html, button, div, table, td, text, tr, a)
import Html.Attributes exposing (class, style, href, target)
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

type alias Board = Array (Array Cell)

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
    | InsertTemplate (Int, Int) Board


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
    Array.repeat 100 False
    |> Array.repeat 100
    --|> insert (10, 10) example


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
        Clear -> ({ model | board = initBoard, paused = True }, Cmd.none)
        StartPaint bool -> ({ model | paintMode = Just bool }, Cmd.none)
        StopPaint -> ({ model | paintMode = Nothing }, Cmd.none)
        Paint coords -> (
            case model.paintMode of
                Nothing -> model
                Just state -> { model | board = setCell model.board coords state }
            , Cmd.none)
        InsertTemplate coords template -> ({ model | board = insert coords template initBoard }, Cmd.none)

toggleCell : Board -> (Int, Int) -> Board
toggleCell board index =
    case getCell board index of
        Just value -> setCell board index (not value)
        Nothing -> board

getCell : Board -> (Int, Int) -> Maybe Cell
getCell board (x, y) = board |> Array.get x |> Maybe.andThen (Array.get y)

setCell : Board -> (Int, Int) -> Cell -> Board
setCell board (x, y) value =
    case Array.get x board of
        Nothing -> board
        Just row ->
            Array.set x (Array.set y value row) board

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
tick board = Array.indexedMap (\x row -> Array.indexedMap (\y cell -> tickCell board (x, y) cell) row) board


insert : (Int, Int) -> Board -> Board -> Board
insert (offsetX, offsetY) inner outer =
    mapAt offsetX offsetY outer inner


mapAt : Int -> Int -> Array (Array a) -> Array (Array a) -> Array (Array a)
mapAt offsetX offsetY outer inner =
    Array.indexedMap
    (\i v ->
        case (Array.get (i - offsetX) inner) of
            Nothing -> v
            Just innerRow -> insertAt offsetY v innerRow identity
        )
    outer

insertAt : Int -> Array a -> Array a -> (a -> a) -> Array a
insertAt offset outer inner func =
    Array.indexedMap (\i v -> func <| Maybe.withDefault v (Array.get (i - offset) inner)) outer

glider: Board
glider = templateToBoard
    ["-X-"
    ,"--X"
    ,"XXX"
    ]

spaceship1: Board
spaceship1 = templateToBoard
    ["X--X-"
    ,"----X"
    ,"X---X"
    ,"-XXXX"
    ]

pentadecathlon: Board
pentadecathlon = templateToBoard
    ["--X----X--"
    ,"XX-XXXX-XX"
    ,"--X----X--"
    ]

gliderGun: Board
gliderGun = templateToBoard
    [ "-------------------------X------------"
    , "-----------------------X-X------------"
    , "-------------XX------XX------------XX-"
    , "------------X---X----XX------------XX-"
    , "-XX--------X-----X---XX---------------"
    , "-XX--------X---X-XX----X-X------------"
    , "-----------X-----X-------X------------"
    , "------------X---X---------------------"
    , "-------------XX-----------------------"
    ]

pulsar: Board
pulsar = templateToBoard
    [ "--XXX---XXX--"
    , "-------------"
    , "X----X-X----X"
    , "X----X-X----X"
    , "X----X-X----X"
    , "--XXX---XXX--"
    , "-------------"
    , "--XXX---XXX--"
    , "X----X-X----X"
    , "X----X-X----X"
    , "X----X-X----X"
    , "-------------"
    , "--XXX---XXX--"
    ]

templateToBoard : List String -> Board
templateToBoard strings =
    strings |>
    List.map (\string -> string
        |> String.toList
        |> List.map (\char -> char == 'X')
        |> Array.fromList
        )
    |> Array.fromList

-- view

view : Model -> Html Msg
view model = div []
    [ table
        [ style "width" ((String.fromInt ((Array.length model.board) * 20)) ++ "px")
        ] (viewRows model.board)
    , div [ class "controls" ]
        [ button [ onClick Tick ] [ text "Step" ]
        , button [ onClick (Pause (not model.paused)) ] [ text (if model.paused then "Play" else "Pause") ]
        , button [ onClick (ChangeSpeed (1 + modBy 5 model.speed)) ] [ text <| "Speed " ++ (String.fromInt model.speed) ]
        , button [ onClick Clear ] [ text "Clear" ]
        , div [ class "spacer" ] []
        , button [ onClick (InsertTemplate (15, 20) pentadecathlon) ] [ text "Pentadecathlon" ]
        , button [ onClick (InsertTemplate (10, 20) pulsar) ] [ text "Pulsar" ]
        , button [ onClick (InsertTemplate (5, 5) glider) ] [ text "Glider" ]
        , button [ onClick (InsertTemplate (5, 5) gliderGun) ] [ text "Glider Gun" ]
        , button [ onClick (InsertTemplate (5, 5) spaceship1) ] [ text "Spaceship" ]
        , div [ class "spacer" ] []
        , a [ href "https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life", target "_blank" ] [ text "About (Wikipedia)" ]
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
