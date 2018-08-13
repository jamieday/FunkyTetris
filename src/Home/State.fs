module Home.State

open Elmish
open Types
open Fable.Core
open System
open Fable.PowerPack
open Fable
open Home.Types.Game
open Home.Types.Board
open FSExtend

let initBoard () =
  seq {
    for i in 0 .. Board.height - 1 do
      for j in 0 .. Board.width - 1 do
        yield { X=j; Y=i }, None
  } |> Map.ofSeq

let nextPiece () =
  { Tetromino = Tetromino.L; Position = { X = Board.width / 2 - 1; Y = 0 }; Rotation = Up }

module FPWindow =
  [<Emit("window.setTimeout($1, $0)")>]
  let setTimeout (ms: float<ms>) (f: unit -> unit) = Exceptions.jsNative

let bindKeys (dispatch: Dispatch<Msg>) =
  Fable.Import.Browser.console.log("Binding keys")
  Fable.Import.Browser.document.addEventListener_keydown (fun evt ->
    Fable.Import.Browser.console.log(sprintf "Key pressed: %f (%f?)" evt.keyCode Keyboard.Codes.right_arrow)
    let msg = match evt.keyCode with
              | Keyboard.Codes.up_arrow -> UpdatePosition { X = 0; Y = -1 } |> Some
              | Keyboard.Codes.right_arrow -> OffsetPosition { X = 1; Y = 0 } |> Some
              | Keyboard.Codes.down_arrow -> OffsetPosition { X = 0; Y = 1 } |> Some
              | Keyboard.Codes.left_arrow -> OffsetPosition { X = -1; Y = 0 } |> Some
              | _ -> None
    match msg with
    | Some msg -> UpdateActivePiece msg |> dispatch
    | None -> ()
    null)

let handleTick (model: Model): Model * Cmd<Msg> =
  // Optimization so we don't spam ticks
  let subscriptions = [ (fun dispatch -> (fun () -> dispatch Tick) |> FPWindow.setTimeout (float model.TickFrequency * 0.1<ms>)) ]

  let now = DateTime.Now.Ticks
  if now - model.LastDrop >= TimeSpan.TicksPerMillisecond * int64 model.TickFrequency then
    let newPos = { model.ActivePiece.Position with Y = model.ActivePiece.Position.Y + 1 }
    let updateMsg = ActivePieceMsg.UpdatePosition newPos |> UpdateActivePiece
    { model with LastDrop = now }, (fun dispatch -> dispatch updateMsg)::subscriptions
  else
    model, subscriptions



/// **Description**
/// Checks the provided tetromino placement
/// and returns whether or not it is vacant or occupied
/// **Parameters**
///   * `newPos` - parameter of type `'a`
///
/// **Output Type**
///   * `'b option`
///
/// **Exceptions**
///
let analyzePos board tetromino pos = None

let update msg model : Model * Cmd<Msg> =
  match msg with
  | UpdateBoard board ->
      model, []
  | Tick -> handleTick model
  | UpdateActivePiece apMsg -> 
      match apMsg with
      | UpdatePosition pos ->
          let model' =
            maybe {
              let posVacant pos =
                pos
                  |> model.PlacedBoard.TryFind
                  |> Option.map (fun cellOpt -> cellOpt.IsNone)
                  |> Option.defaultValue false
              let tetroStructure = Tetromino.structure Rotation.Up model.ActivePiece.Tetromino
              let invalidPos = tetroStructure |> Seq.map ((+) model.ActivePiece.Position) |> Seq.map posVacant |> Seq.contains false
              let model' = match invalidPos with
                            | false ->
                              let activePiece = { model.ActivePiece with Position = pos }
                              { model with ActivePiece = activePiece }
                            | true ->
                              model
              return model'                          
            }
          model' |> Option.defaultValue model, []
      | OffsetPosition offset ->
          let newActivePiece = { model.ActivePiece with Position = model.ActivePiece.Position + offset }
          { model with ActivePiece = newActivePiece }, []
      | UpdateRotation(_) -> failwith "Not Implemented"

let init () : Model * Cmd<Msg> =
  let gameState = { PlacedBoard     = initBoard ()
                    ActivePiece     = nextPiece ()
                    QueuedPieces    = [ Tetromino.L; Tetromino.J ]
                    LastDrop      = 0L
                    TickFrequency = 1000.<ms> }
  gameState, [ fun dispatch -> dispatch Tick
               fun dispatch -> bindKeys dispatch ]