module Home.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types

let cellElm pos model =
  let elmClass = 
    (match model with
    | Some cell -> toCssColor cell.Color
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
  let activeStructure = structure activePiece.Rotation activePiece.Tetrimino
  activeStructure
    |> List.fold (fun (acc: Board) offset ->
        let activeCellPosition = activePiece.Position + offset
        match activeCellPosition with
        | {X=x; Y=y} when x < 0 || y < 0 -> acc
        | _ -> acc.Add (activeCellPosition, Some { Color=Color.Blue })
      ) board

let root (model: Model) _dispatch =
  model.ActivePiece
    |> applyToBoard model.PlacedBoard
    |> boardElm