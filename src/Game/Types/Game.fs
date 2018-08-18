module Game.Types.Game

open Game.Types.Board
open Game.Types.Tetromino

type ActivePiece = 
  { Tetromino: Tetromino
    Position: Position
    Rotation: Rotation
    LastDrop: int64 }


[<Measure>] type ms

type GameState = { PlacedBoard: Board
                   ActivePiece: ActivePiece
                   QueuedPieces: Tetromino list
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