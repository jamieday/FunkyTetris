module Game.Types.Game

open Game.Types.Board
open Game.Types.Tetromino

type ActivePiece = 
  { Tetromino: Tetromino
    Position: Position
    Rotation: Rotation }

type HoldPiece =
  | Locked of Tetromino
  | Unlocked of Tetromino option

[<Measure>] type ms

type ClockState =
  { Ticks: int64
    DropFrequency: int64 }

type GameState = { Paused: bool
                   PlacedBoard: Board
                   ActivePiece: ActivePiece
                   HoldPiece: HoldPiece
                   QueuedPieces: Tetromino list
                   Clock: ClockState }

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
  | Hold
  | UpdatePosition of Position
  | OffsetPosition of Position
  | UpdateRotation of Spin