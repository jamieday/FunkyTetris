module Home.State

open Elmish
open Types

let initBoard () =
  seq {
    for i in 0 .. Board.height - 1 do
      for j in 0 .. Board.width - 1 do
        yield { X=j; Y=i }, None
  } |> Map.ofSeq

let nextPiece () =
  { Tetrimino = L; Position = { X = Board.width / 2 - 1; Y = 0 }; Rotation = Up }

let init () : Model * Cmd<Msg> =
  let gameState = { PlacedBoard   = initBoard ()
                    ActivePiece   = nextPiece ()
                    QueuedPieces  = [ L; J ] }
  gameState, []

let update msg model : Model * Cmd<Msg> =
  match msg with
  | UpdateBoard board ->
      // TODO do something
      model, []
