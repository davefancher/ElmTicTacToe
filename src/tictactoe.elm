import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onInput, onClick)

main =
    App.program { init = init, view = view, update = update, subscriptions = subscriptions }

-- Model

type Player = PlayerX | PlayerO

type XPos = Left | Center | Right
type YPos = Top | Middle | Bottom

type alias CellPosition = (YPos, XPos)

type CellState =
    Occupied Player
    | Empty

type alias Cell =
    { position: CellPosition
    , state : CellState
    }

type alias Board = List Cell

type MoveResult =
    InvalidMove Player
    | NextMove Player
    | Win Player
    | Draw

type alias Model =
    { board : Board
    , lastMoveResult : MoveResult
    }

-- Init

horizontalPositions : List XPos
horizontalPositions = [ Left, Center, Right ]

verticalPositions : List YPos
verticalPositions = [ Top, Middle, Bottom ]

allPositions : List CellPosition
allPositions =
    horizontalPositions
    |> List.map (\h -> List.map (\v -> (v, h)) verticalPositions)
    |> List.concat

init : (Model, Cmd Msg)
init = (Model (allPositions |> List.map (\p -> Cell p Empty)) (NextMove PlayerX), Cmd.none)

-- Update

type Msg =
    Move Player CellPosition
    | Reset

linesToCheck : List (List CellPosition)
linesToCheck =
    let
        makeRow r = horizontalPositions |> List.map (\c -> (r, c))
        rows = verticalPositions |> List.map makeRow

        makeCol c = verticalPositions |> List.map (\r -> (r, c))
        cols = horizontalPositions |> List.map makeCol

        diag1 = [ (Top, Left), (Middle, Center), (Bottom, Right) ]
        diag2 = [ (Top, Right), (Middle, Center), (Bottom, Left) ]

        lines = List.concat [ rows, cols, [ diag1 ], [ diag2 ] ]
    in
        lines

getCell : Board -> CellPosition -> Cell
getCell board (row, col) =
    let
        cell =
            board
            |> List.filter (\c -> c.position == (row, col))
            |> List.head
    in
        case cell of
            Just c -> c
            Nothing -> { position = (row, col), state = Empty }

cellIsOccupied : Board -> CellPosition -> Bool
cellIsOccupied board position =
    case (getCell board position).state of
        Empty -> True
        _ -> False

checkForWin : Player -> Board -> Bool
checkForWin player board =
    linesToCheck
    |> List.map (List.all (\p -> case (p |> getCell board).state of
                                        Empty -> False
                                        Occupied p -> p == player))
    |> List.any (\l -> l)

checkForDraw : Board -> Bool
checkForDraw board =
    (List.all (\c -> not (c.state == Empty)) board)
    && not (checkForWin PlayerX board)
    && not (checkForWin PlayerO board)

replaceCell : Board -> Cell -> Board
replaceCell board newCell =
    board
    |> List.map (\oldCell -> if oldCell.position == newCell.position then newCell else oldCell)

getUnoccupiedCells : Board -> List CellPosition
getUnoccupiedCells board =
    board
    |> List.filter (\c -> c.state == Empty)
    |> List.map (\c -> c.position)

isValidMove : Board -> CellPosition -> Bool
isValidMove board position =
    (getCell board position).state == Empty

getMoveResult : Player -> Board -> MoveResult
getMoveResult player board =
    if checkForWin player board then
        Win player
    else if checkForDraw board then
        Draw
    else
        case player of
            PlayerX -> NextMove PlayerO 
            PlayerO -> NextMove PlayerX

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Move player position ->
            if not (isValidMove model.board position) then
                ({ model | lastMoveResult = InvalidMove player }, Cmd.none)
            else
                replaceCell model.board { position = position, state = Occupied player }
                |> (\b -> ({ board = b, lastMoveResult = getMoveResult player b }, Cmd.none))
        Reset ->
            init

-- View

getPlayerName : Player -> String
getPlayerName player =
    case player of
        PlayerX -> "X"
        PlayerO -> "O"

getCellText : Cell -> String
getCellText cell = 
    case cell.state of
        Occupied player -> getPlayerName player
        Empty -> "-"

buildRows : Model -> List (Html Msg)
buildRows model =
    let
        buildCells : List XPos -> YPos -> List (Html Msg)
        buildCells cols row =
            cols
            |> List.map (\col -> getCell model.board (row, col))
            |> List.map (\cell -> td []
                                  [ case cell.state of
                                        Occupied p ->
                                            getPlayerName p |> text
                                            
                                        Empty -> 
                                            case model.lastMoveResult of
                                                NextMove nextPlayer ->
                                                    button
                                                        [ onClick (Move nextPlayer cell.position) ]
                                                        [ cell |> getCellText |> text ]
                                                _ ->
                                                    button [] [ cell |> getCellText |> text ] ])
    in
        verticalPositions
        |> List.map (\r -> tr [] (buildCells horizontalPositions r))

view : Model -> Html Msg
view model =
    div
    [ style [] ]
    [ table
      [ style [ ("border", "1px solid black")
              , ("border-collapse", "collapse" ) ] ]
      (buildRows model)
      , button [ onClick Reset ] [ text "Reset" ]
      , (case model.lastMoveResult of
            InvalidMove _ -> "Invalid Move"
            NextMove PlayerX -> "Player X's Turn"
            NextMove PlayerO -> "Player O's Turn"
            Win PlayerX -> "Player X Wins!"
            Win PlayerO -> "Player O Wins!"
            Draw -> "It's a draw!") |> text
    ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
