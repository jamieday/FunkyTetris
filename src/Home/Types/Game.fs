module Home.Types.Game

open Home.Types.Board
open Home.Types.Tetromino

type Rotation = Up | Right | Down | Left

type ActivePiece = 
  { Tetromino: Tetromino
    Position: Position
    Rotation: Rotation }

[<Measure>] type ms

type GameState = { PlacedBoard: Board
                   ActivePiece: ActivePiece
                   QueuedPieces: Tetromino seq
                   LastDrop: int64
                   TickFrequency: float<ms> }


type ActivePieceMsg =
  | UpdatePosition of Position
  | OffsetPosition of Position
  | UpdateRotation of Rotation