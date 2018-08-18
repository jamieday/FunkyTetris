module Game.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Game.Types.Board
open Game.Types.Game
open Game.Types.Tetromino

let toCellClass = function
  | Fragment color ->
    match color with
    | Cyan -> "cyan"
    | Blue -> "blue"
    | Orange -> "orange"
    | Yellow -> "yellow"
    | Green -> "green"
    | Purple -> "purple"
    | Red -> "red"
  | Ghost -> "ghost"  

let cellElm pos (model: Cell option) =
  let elmClass = 
    (match model with
    | Some cell -> toCellClass cell
    | None -> "empty") |> sprintf "board-cell %s"

  div [ Data ("col", pos.X)
        ClassName elmClass ] [ ]

let boardElm (model: Board) =
  let rows =
    model
      |> Map.toSeq
      |> Seq.sortBy (fun (pos, _) -> pos.Y, pos.X)
      |> Seq.groupBy (fun (pos, _) -> pos.Y)

  div [ ClassName "board" ]
      (rows 
        |> Seq.map (fun (row, cols) ->
                div [ Data ("row", row)
                      ClassName "board-row" ]
                    ( cols
                        |> Seq.map (fun (pos, cell) -> cellElm pos cell)
                        |> List.ofSeq )
                    )
        |> List.ofSeq)

type BoardPiece =
  | TetroPiece of ActivePiece
  | GhostPiece of ActivePiece

let applyToBoard board (piece: BoardPiece) =
  let (pos, rot, tetro, cell) =
    match piece with
    | TetroPiece { Position=pos; Rotation=rot; Tetromino=tetro } -> (pos, rot, tetro, Fragment (Tetromino.toMeta tetro).Color)
    | GhostPiece { Position=pos; Rotation=rot; Tetromino=tetro } -> (pos, rot, tetro, Ghost)
  let activeStructure = Tetromino.structure rot tetro
  activeStructure
    |> Seq.fold (fun (acc: Board) offset ->
        let activeCellPosition = pos + offset
        match activeCellPosition with
        | {X=x; Y=y} when x < 0 || y < 0 -> acc
        | _ -> acc.Add (activeCellPosition, Some cell)
      ) board

let root (model: Model) _dispatch =
  model.ActivePiece |> TetroPiece 
    |> applyToBoard
      ( 
        (model.ActivePiece
          |> State.droppedPlacement model.PlacedBoard)
          |> GhostPiece
          |> applyToBoard model.PlacedBoard )
    |> boardElm