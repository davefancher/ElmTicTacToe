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
    NewMove Player
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

allPositions : List (YPos, XPos)
allPositions =
    horizontalPositions
    |> List.map (\h -> List.map (\v -> (v, h)) verticalPositions)
    |> List.concat

init : (Model, Cmd Msg)
init = (Model (allPositions |> List.map (\p -> Cell p Empty)) (NewMove PlayerX), Cmd.none)

-- Update

type Msg =
    Move Player Cell
    | Reset

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

getLinesToCheck : Board -> List (List Cell)
getLinesToCheck board =
    let
        findCell = getCell board

        makeRow : YPos -> List Cell
        makeRow r = horizontalPositions |> List.map (\c -> findCell (r, c))
        rows = verticalPositions |> List.map makeRow

        makeCol : XPos -> List Cell
        makeCol c = verticalPositions |> List.map (\r -> findCell (r, c))
        cols = horizontalPositions |> List.map makeCol

        diag1 : List Cell
        diag1 = [ findCell (Top, Left), findCell (Middle, Center), findCell (Bottom, Right) ]
        diag2 = [ findCell (Top, Right), findCell (Middle, Center), findCell (Bottom, Left) ]

        lines : List (List Cell)
        lines = List.concat [ rows, cols, [ diag1 ], [ diag2 ] ]
    in
        lines

cellIsOccupied : Board -> CellPosition -> Bool
cellIsOccupied board position =
    case (getCell board position).state of
        Empty -> True
        _ -> False

checkForWin : Player -> Board -> Bool
checkForWin player board =
    board
    |> getLinesToCheck
    |> List.map (List.all (\c -> case c.state of
                                        Empty -> False
                                        Occupied p -> p == player))
    |> List.any (\l -> l)

checkForDraw : Board -> Bool
checkForDraw board =
    (List.all (\c -> not (c.state == Empty)) board)
    && not (checkForWin PlayerX board)
    && not (checkForWin PlayerO board)

updateBoard : Board -> Cell -> Board
updateBoard board newCell =
    board
    |> List.map (\oldCell -> if oldCell.position == newCell.position then newCell else oldCell)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Move p c ->
            let
                -- TODO: Handle occupied space (don't replace)
                newBoard = updateBoard model.board { c | state = Occupied p }
                result =
                    if checkForWin p newBoard then
                        Win p
                    else if checkForDraw newBoard then
                        Draw
                    else
                        case p of
                            PlayerX -> NewMove PlayerO
                            PlayerO -> NewMove PlayerX
                newModel = { board = newBoard, lastMoveResult = result }
            in
                (newModel, Cmd.none)
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
                                                NewMove nextPlayer ->
                                                    button
                                                        [ onClick (Move nextPlayer cell) ]
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
            NewMove PlayerX -> "Player X's Turn"
            NewMove PlayerO -> "Player O's Turn"
            Win PlayerX -> "Player X Wins!"
            Win PlayerO -> "Player O Wins!"
            Draw -> "It's a draw!") |> text
    ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
