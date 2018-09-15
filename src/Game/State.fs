module Game.State

open Elmish
open Fable.Core
open Fable.PowerPack
open Fable
open Types.Game
open Types.Tetromino
open Types.Board
open Types.Model

let initBoard () =
  seq {
    for i in 0 .. Board.height - 1 do
      for j in 0 .. Board.width - 1 do
        yield { X=j; Y=i }, None
  } |> Map.ofSeq

let rand = System.Random()
let randTetromino (eligiblePieces: Set<Tetromino>) =
  let piecesArr = eligiblePieces |> Set.toArray
  piecesArr.[rand.Next <| Set.count eligiblePieces]

module ActivePiece =
  let init tetromino =
    { Tetromino = tetromino; Position = { X = Board.width / 2 - 1; Y = 2 }; Rotation = Up }

let nextPiece eligiblePieces queued =
  let (nextTetromino, queued) =
    match queued with
    | x::xs -> x, List.append xs [ randTetromino eligiblePieces ]
    | [] -> let xs = [ for _ in 0..5 -> randTetromino eligiblePieces ] in (List.head xs, List.tail xs)
  ActivePiece.init nextTetromino, queued

let initGameState =
  let activePiece, queued = nextPiece eligiblePieces []
  { Paused          = false
    PlacedBoard     = initBoard ()
    ActivePiece     = activePiece
    EligiblePieces  = eligiblePieces
    QueuedPieces    = queued
    HoldPiece       = Unlocked None
    Clock           = { Ticks = 0L
                        DropFrequency = 75L } }

module FPWindow =
  [<Emit("window.setTimeout($1, $0)")>]
  let setTimeout (ms: float<ms>) (f: unit -> unit) = jsNative

let bindKeys (dispatch: Dispatch<Msg>) =
  Fable.Import.Browser.document.addEventListener_keydown (fun evt ->
    let msg = match evt.keyCode with
              | Keyboard.Codes.p -> TogglePaused |> Some
              | Keyboard.Codes.escape -> TogglePaused |> Some
              | Keyboard.Codes.r -> TriggerRestart |> Some

              | Keyboard.Codes.l -> LambdaMode |> Some

              | 32. -> HardDrop |> UpdateActivePiece |> Some // Spacebar
              | Keyboard.Codes.c -> Hold |> UpdateActivePiece |> Some
              | Keyboard.Codes.up_arrow -> UpdateRotation Clockwise |> UpdateActivePiece |> Some
              | Keyboard.Codes.z -> UpdateRotation CounterClockwise |> UpdateActivePiece |> Some
              | Keyboard.Codes.right_arrow -> OffsetPosition { X = 1; Y = 0 } |> UpdateActivePiece |> Some
              | Keyboard.Codes.down_arrow -> Drop |> UpdateActivePiece |> Some
              | Keyboard.Codes.left_arrow -> OffsetPosition { X = -1; Y = 0 } |> UpdateActivePiece |> Some
              | _ -> None
    match msg with
    | Some msg -> dispatch msg
    | None -> ()
    null)


let validatePiece (board: Board) { Tetromino=tetro; Rotation=rot; Position=pos } =
  let posVacant pos =
    pos
      |> board.TryFind
      |> Option.map (fun cellOpt -> cellOpt.IsNone)
      |> Option.defaultValue false
  let tetroStructure = structure rot tetro
  let invalidPos = tetroStructure |> Seq.map ((+) pos) |> Seq.map posVacant |> Seq.contains false
  not invalidPos 

let handleTick (model: Model): Model * Cmd<Msg> =
  let tickFrequency = 25<ms>

  // Optimization so we don't spam ticks
  let subs = [ (fun dispatch -> (fun () -> dispatch Tick) |> FPWindow.setTimeout (float tickFrequency * 0.1<ms>)) ]

  match model.Paused with
  | true -> model, subs
  | false ->
      let { Ticks=ticks; DropFrequency=dropFreq } = model.Clock
      let ticks' = ticks + 1L
      let model' = { model with Clock = { model.Clock with Ticks = ticks' } }

      let dropSubOpt =
        match ticks' % dropFreq with
        | 0L -> Some (fun dispatch -> dispatch (UpdateActivePiece Drop))
        | _ -> None
      model', match dropSubOpt with Some dropSub -> dropSub::subs | None -> subs

let rec droppedPlacement board piece =
            let pos' = let { X=x; Y=y } = piece.Position in { X=x; Y=y+1 }
            let piece' = { piece with Position = pos' }
            if piece' |> validatePiece board 
              then piece' |> droppedPlacement board
              else piece

let applyToBoard (board: Board) ({ Tetromino=tetromino; Rotation=rot; Position=pos }: ActivePiece) =
  let cell = Fragment (toMeta tetromino).Color
  structure rot tetromino
  |> Seq.map ((+) pos)
  |> Seq.fold (fun (board: Board) pos -> board.Add (pos, Some cell)) board

let clearLines (board: Board) =
  let boardRows =
    board
    |> Map.toSeq
    |> Seq.groupBy (fun ({ Y=y }, _) -> y)
    |> Seq.sortByDescending (fun (y, _) -> y)

  // Find full rows
  let fullRowsDesc =
    boardRows
    |> Seq.fold 
        (fun acc (y, row) ->
          let full =
            row
            |> Seq.map (fun (_, cell) -> cell)
            |> Seq.contains None
            |> not
          if full then y::acc else acc
          ) []
    |> List.sortDescending

  let (_, _, clearedBoard) =
    boardRows
    |> Seq.fold 
      (fun (downOffset, (fullRowsDesc: int list), board) (y, cells) -> 
        match y with
        | _ when y = fullRowsDesc.Head ->
            let downOffset' = downOffset + 1
            (downOffset', List.tail fullRowsDesc, board)
        | _ ->
            let board' = cells |> Seq.fold (fun acc ({ X=x; Y=y }, cell) -> acc |> Map.add { X=x; Y=y+downOffset} cell) board
            (downOffset, fullRowsDesc, board')
      )
      (0, fullRowsDesc, Map.empty<Position, Cell option>)

  // Pad the board at the beginning
  let board' = 
    seq { 0 .. List.length fullRowsDesc }
    |> Seq.fold
        (fun acc y ->
          seq { 0 .. Board.width - 1 } |> Seq.fold (fun acc x -> acc |> Map.add { X=x; Y=y } None) acc )
        clearedBoard  
  board'
  
let update msg model : Model * Cmd<Msg> =
  match msg with
  | Tick -> handleTick model
  | TogglePaused ->
      { model with Paused = not model.Paused }, []
  | TriggerRestart ->
      initGameState, []    
  | LambdaMode ->
      { model with EligiblePieces = model.EligiblePieces |> Set.add Λ }, []
  | UpdateActivePiece apMsg when not model.Paused ->
      match apMsg with
      | Drop ->
          if model.ActivePiece |> validatePiece model.PlacedBoard |> not then
            // game over
            model, []
          else

          let activePiece' = 
            let pos' = let { X=x; Y=y } = model.ActivePiece.Position in { X=x; Y=y+1 }
            { model.ActivePiece with Position = pos' }
          let isValid = activePiece' |> validatePiece model.PlacedBoard

          let model' =
            match isValid with
            | true -> { model with ActivePiece = activePiece' }
            | false ->
                let board' =
                  model.ActivePiece
                  |> applyToBoard model.PlacedBoard
                  |> clearLines
                let activePiece', queued' = nextPiece model.EligiblePieces model.QueuedPieces
                let holdPiece' =
                  match model.HoldPiece with
                  | Locked p -> Some p |> Unlocked
                  | Unlocked _ -> model.HoldPiece
                { model with PlacedBoard = board'; ActivePiece = activePiece'; QueuedPieces = queued'; HoldPiece = holdPiece' }

          model', []
      | HardDrop ->
          let model' =
            let droppedPiece = model.ActivePiece |> droppedPlacement model.PlacedBoard
            let board' = droppedPiece |> applyToBoard model.PlacedBoard |> clearLines
            let activePiece', queued' = nextPiece model.EligiblePieces model.QueuedPieces
            // TODO remove duplication
            let holdPiece' =
              match model.HoldPiece with
              | Locked p -> Some p |> Unlocked
              | Unlocked _ -> model.HoldPiece
            { model with PlacedBoard = board'; ActivePiece = activePiece'; QueuedPieces = queued'; HoldPiece = holdPiece' }

          model', []
      | Hold ->
          let model' = 
            match model.HoldPiece with
            | Locked _ -> model
            | Unlocked holdPiece ->
              match holdPiece with
              | Some tetro ->
                  let activePiece' = ActivePiece.init tetro
                  { model with
                      ActivePiece = activePiece'
                      HoldPiece = Locked model.ActivePiece.Tetromino }
              | None ->
                  let (activePiece', queued') = nextPiece model.EligiblePieces model.QueuedPieces
                  { model with
                      ActivePiece = activePiece'
                      QueuedPieces = queued'
                      HoldPiece = Locked model.ActivePiece.Tetromino }
          model', []
      | UpdatePosition pos ->
          let piece' = { model.ActivePiece with Position = pos }
          let isValid = piece' |> validatePiece model.PlacedBoard

          let model' = if isValid then { model with ActivePiece = piece' } else model
          model', []
      | OffsetPosition offset ->
          model, UpdatePosition (model.ActivePiece.Position + offset) |> UpdateActivePiece |> Cmd.ofMsg
      | UpdateRotation spin ->
          let nextRot = model.ActivePiece.Rotation |> Spin.nextRot spin
          let piece' = { model.ActivePiece with Rotation = nextRot }
          let isValid = piece' |> validatePiece model.PlacedBoard
          let model' = if isValid then { model with ActivePiece = piece' } else model
          model', []
    | _ -> model, []        

let init () : Model * Cmd<Msg> =
  initGameState,
  [ fun dispatch -> dispatch Tick
    fun dispatch -> bindKeys dispatch ]