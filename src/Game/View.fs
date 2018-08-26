module Game.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types.Board
open Types.Game
open Types.Tetromino
open Types.Model

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
    | None -> "empty") |> sprintf "cell %s"

  div [ Data ("col", pos.X)
        ClassName elmClass ] [ ]

let boardElm (model: Board) =
  let rows =
    model
      |> Map.toSeq
      |> Seq.sortBy (fun (pos, _) -> pos.Y, pos.X)
      |> Seq.groupBy (fun (pos, _) -> pos.Y)

  div [ ClassName "board main" ]
      (rows 
        |> Seq.map (fun (row, cols) ->
                div [ Data ("row", row)
                      ClassName "row" ]
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
    | TetroPiece { Position=pos; Rotation=rot; Tetromino=tetro } -> (pos, rot, tetro, Fragment (toMeta tetro).Color)
    | GhostPiece { Position=pos; Rotation=rot; Tetromino=tetro } -> (pos, rot, tetro, Ghost)
  let activeStructure = structure rot tetro
  activeStructure
    |> Seq.fold (fun (acc: Board) offset ->
        let activeCellPosition = pos + offset
        match activeCellPosition with
        | {X=x; Y=y} when x < 0 || y < 0 -> acc
        | _ -> acc.Add (activeCellPosition, Some cell)
      ) board

let miniBoard boardClass (tetroOpt: Tetromino option) =
  let body =
    let structure = 
      match tetroOpt with
      | Some tetro -> 
          tetro
          |> structure Rotation.Up
          |> Seq.map ((+) { X=2; Y=2 })
          |> Set.ofSeq
      | None -> Set.empty        

    List.init 6
      (fun y ->
        div [ ClassName "row" ]
            (List.init 6
              (fun x ->
                let cellType =
                  let hasCell = structure |> Set.contains { X=x; Y=y }
                  match (tetroOpt, hasCell) with
                  | (Some tetro, true) ->
                      let { Color=color } = (tetro |> toMeta) in Fragment color |> toCellClass
                  | _ -> "empty"                    
                div [ ClassName ("cell " + cellType) ] [ ])))
  div [ ClassName (sprintf "board %s" boardClass) ]
      body

let holdElm holdPiece =
  let tetroOpt = 
    match holdPiece with
    | Locked tetro -> Some tetro
    | Unlocked tetroOpt -> tetroOpt
  tetroOpt |> miniBoard "hold"

let controlsElm =
  let controls =
    [ "R",      "restart"
      "Esc/P",  "pause"
      "Up",     "rotate CW"
      "Right",  "move right"
      "Left",   "move left"
      "Down",   "move down"
      "Space",  "hard drop"
      "C",      "hold"
      "Z",      "rotate CCW" ]

  ul [ ClassName "board info"
       Style [ MarginTop "15px" ] ]
     (controls
      |> List.fold
          (fun acc (label, desc) ->
            li [ ] [ sprintf "%s - " label |> str
                     span [ ClassName "description" ] [ str desc ] ]::acc)
          [ ])
        
let queuedElm queued =
  div [ ClassName "queued" ]
      (List.foldBack
          (fun tetro acc ->
            let queuedPieceRendered = tetro |> Some |> miniBoard "queued"
            queuedPieceRendered::
              match acc with
              | _::_ -> hr [ ]::acc
              | [] -> [])
          queued [ ])

let root (model: Model) _dispatch =
  let infoColRendered =
    div [ ClassName "info" ]
        [ holdElm model.HoldPiece
          controlsElm ]

  let boardColRendered =
    model.ActivePiece |> TetroPiece 
      |> applyToBoard
        ( 
          (model.ActivePiece
            |> State.droppedPlacement model.PlacedBoard)
            |> GhostPiece
            |> applyToBoard model.PlacedBoard )
      |> boardElm

  let queuedColRendered = queuedElm model.QueuedPieces
  
  let gameClass = sprintf "game %s" (if model.Paused then "paused" else "active")

  div [ ClassName gameClass ] 
      [ infoColRendered
        boardColRendered
        queuedColRendered ]