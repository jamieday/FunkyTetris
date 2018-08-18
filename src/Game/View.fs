module Game.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Game.Types.Board
open Game.Types.Game
open Game.Types.Tetromino

let toCellClass = function
  | Cyan -> "cyan"
  | Blue -> "blue"
  | Orange -> "orange"
  | Yellow -> "yellow"
  | Green -> "green"
  | Purple -> "purple"
  | Red -> "red"

let cellElm pos (model: Cell option) =
  let elmClass = 
    (match model with
    | Some cell -> toCellClass cell.Color
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

let applyToBoard board activePiece =
  let activeStructure = Tetromino.structure activePiece.Rotation activePiece.Tetromino
  activeStructure
    |> Seq.fold (fun (acc: Board) offset ->
        let activeCellPosition = activePiece.Position + offset
        match activeCellPosition with
        | {X=x; Y=y} when x < 0 || y < 0 -> acc
        | _ -> acc.Add (activeCellPosition, Some { Color = (Tetromino.toMeta activePiece.Tetromino).Color })
      ) board

let root (model: Model) _dispatch =
  model.ActivePiece
    |> applyToBoard model.PlacedBoard
    |> boardElm