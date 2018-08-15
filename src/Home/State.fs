module Home.State

open Elmish
open Types
open Fable.Core
open System
open Fable.PowerPack
open Fable
open Home.Types.Game
open Home.Types.Tetromino
open Home.Types.Board
open FSExtend

let initBoard () =
  seq {
    for i in 0 .. Board.height - 1 do
      for j in 0 .. Board.width - 1 do
        yield { X=j; Y=i }, None
  } |> Map.ofSeq

let randNullaryUnion<'t>() = 
  let cases = Reflection.FSharpType.GetUnionCases(typeof<'t>)
  let index = System.Random().Next(cases.Length)
  let case = cases.[index]
  Reflection.FSharpValue.MakeUnion(case, [||]) :?> 't

let nextPiece () =
  let nextTetromino = randNullaryUnion<Tetromino>()
  { Tetromino = nextTetromino; Position = { X = Board.width / 2 - 1; Y = 2 }; Rotation = Up; LastDrop = DateTime.Now.Ticks }

module FPWindow =
  [<Emit("window.setTimeout($1, $0)")>]
  let setTimeout (ms: float<ms>) (f: unit -> unit) = Exceptions.jsNative

let bindKeys (dispatch: Dispatch<Msg>) =
  Fable.Import.Browser.console.log("Binding keys")
  Fable.Import.Browser.document.addEventListener_keydown (fun evt ->
    Fable.Import.Browser.console.log(sprintf "Key pressed: %f (%f?)" evt.keyCode Keyboard.Codes.right_arrow)
    let msg = match evt.keyCode with
              | Keyboard.Codes.up_arrow -> UpdateRotation Clockwise |> Some
              | Keyboard.Codes.right_arrow -> OffsetPosition { X = 1; Y = 0 } |> Some
              | Keyboard.Codes.down_arrow -> Drop |> Some
              | Keyboard.Codes.left_arrow -> OffsetPosition { X = -1; Y = 0 } |> Some
              | _ -> None
    match msg with
    | Some msg -> UpdateActivePiece msg |> dispatch
    | None -> ()
    null)


let validateMove (board: Board) pos activePiece =
  let posVacant pos =
    pos
      |> board.TryFind
      |> Option.map (fun cellOpt -> cellOpt.IsNone)
      |> Option.defaultValue false
  let tetroStructure = Tetromino.structure Rotation.Up activePiece.Tetromino
  let invalidPos = tetroStructure |> Seq.map ((+) pos) |> Seq.map posVacant |> Seq.contains false
  if invalidPos then None else Some pos

let handleTick (model: Model): Model * Cmd<Msg> =
  // Optimization so we don't spam ticks
  let subscriptions = [ (fun dispatch -> (fun () -> dispatch Tick) |> FPWindow.setTimeout (float model.TickFrequency * 0.1<ms>)) ]

  let now = DateTime.Now.Ticks
  if now - model.ActivePiece.LastDrop >= TimeSpan.TicksPerMillisecond * int64 model.TickFrequency then
    { model with ActivePiece = { model.ActivePiece with LastDrop = now } }, (fun dispatch -> dispatch (UpdateActivePiece Drop))::subscriptions
  else
    model, subscriptions

let applyToBoard (board: Board) ({ Tetromino=tetromino; Position=pos }: ActivePiece) =
  let cell = { Color = (Tetromino.toMeta tetromino).Color }
  Tetromino.structure Rotation.Up tetromino
  |> Seq.map ((+) pos)
  |> Seq.fold (fun (board: Board) pos -> board.Add (pos, Some cell)) board
  
let update msg model : Model * Cmd<Msg> =
  match msg with
  | UpdateBoard board ->
      model, []
  | Tick -> handleTick model
  | UpdateActivePiece apMsg -> 
      match apMsg with
      | Drop ->
          let pos' = let { X=x; Y=y } = model.ActivePiece.Position in { X=x; Y=y+1 }
          let droppedPiece =
            model.ActivePiece
            |> validateMove model.PlacedBoard pos'
            |> Option.map (fun p -> { model.ActivePiece with Position = p })

          let model' =
            match droppedPiece with
            | Some piece -> { model with ActivePiece = piece }
            | None ->
                let board' = model.ActivePiece |> applyToBoard model.PlacedBoard
                let activePiece' = nextPiece ()
                { model with PlacedBoard = board'; ActivePiece = activePiece' }

          model', []            
      | UpdatePosition pos ->
          let activePiece = 
            model.ActivePiece
            |> validateMove model.PlacedBoard pos
            |> Option.map (fun p -> { model.ActivePiece with Position = p })
            |> Option.defaultValue model.ActivePiece

          { model with ActivePiece = activePiece }, []
      | OffsetPosition offset ->
          let activePiece =
            model.ActivePiece
            |> validateMove model.PlacedBoard (model.ActivePiece.Position + offset)
            |> Option.map (fun p -> { model.ActivePiece with Position = p })
            |> Option.defaultValue model.ActivePiece
          { model with ActivePiece = activePiece }, []
      | UpdateRotation spin ->
          match spin with
          | Clockwise -> 
              { model with ActivePiece = { model.ActivePiece with Rotation = Right } }, []
              
let init () : Model * Cmd<Msg> =
  let gameState = { PlacedBoard     = initBoard ()
                    ActivePiece     = nextPiece ()
                    QueuedPieces    = [ Tetromino.L; Tetromino.J ]
                    TickFrequency = 500.<ms> }
  gameState, [ fun dispatch -> dispatch Tick
               fun dispatch -> bindKeys dispatch ]