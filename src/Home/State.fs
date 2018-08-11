module Home.State

open Elmish
open Types
open Fable.Core
open System

let initBoard () =
  seq {
    for i in 0 .. Board.height - 1 do
      for j in 0 .. Board.width - 1 do
        yield { X=j; Y=i }, None
  } |> Map.ofSeq

let nextPiece () =
  { Tetromino = L; Position = { X = Board.width / 2 - 1; Y = 0 }; Rotation = Up }

module Window =
  [<Emit("window.setTimeout($1, $0)")>]
  let setTimeout (ms: float<ms>) (f: unit -> unit) = Exceptions.jsNative

let handleTick (model: Model): Model * Cmd<Msg> =
  // Optimization so we don't spam ticks
  let tickLater = (fun dispatch -> (fun () -> dispatch Tick) |> Window.setTimeout (float model.TickFrequency * 0.1<ms>))
  let now = DateTime.Now.Ticks
  if now - model.LastTick >= int64 model.TickFrequency then
    let newPos = { model.ActivePiece.Position with Y = model.ActivePiece.Position.Y + 1 }
    let updateMsg = ActivePieceMsg.UpdatePosition newPos |> UpdateActivePiece
    let thing = fun (dispatch: Dispatch<Msg>) -> dispatch Tick
    { model with LastTick = now }, [ tickLater; (fun dispatch -> dispatch updateMsg) ]
  else
    model, [ tickLater ]

let update msg model : Model * Cmd<Msg> =
  match msg with
  | UpdateBoard board ->
      model, []
  | Tick ->
      handleTick model
  | UpdateActivePiece apMsg -> 
      match apMsg with
      | ActivePieceMsg.UpdatePosition newPos ->
          let newActivePiece = { model.ActivePiece with Position = newPos }
          { model with ActivePiece = newActivePiece }, []

let init () : Model * Cmd<Msg> =
  let gameState = { PlacedBoard     = initBoard ()
                    ActivePiece     = nextPiece ()
                    QueuedPieces    = [ L; J ]
                    LastTick      = 0L
                    TickFrequency = 1000.<ms> }
  gameState, [ fun dispatch -> dispatch Tick ]