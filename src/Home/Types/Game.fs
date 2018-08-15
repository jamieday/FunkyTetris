module Home.Types.Game

open Home.Types.Board
open Home.Types.Tetromino

type ActivePiece = 
  { Tetromino: Tetromino
    Position: Position
    Rotation: Rotation
    LastDrop: int64 }


[<Measure>] type ms

type GameState = { PlacedBoard: Board
                   ActivePiece: ActivePiece
                   QueuedPieces: Tetromino seq
                   TickFrequency: float<ms> }

type Spin = Clockwise | CounterClockwise

module Spin =
  let nextRot = function
    | Clockwise -> 
        function
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up
    | CounterClockwise -> 
        function
        | Up -> Left
        | Right -> Up
        | Down -> Right
        | Left -> Down

type ActivePieceMsg =
  | Drop
  | HardDrop
  | UpdatePosition of Position
  | OffsetPosition of Position
  | UpdateRotation of Spin