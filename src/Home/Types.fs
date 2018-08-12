module Home.Types

type Color =
  | Cyan
  | Blue
  | Orange
  | Yellow
  | Green
  | Purple
  | Red

type Cell =
  { Color: Color }

type Position = { X: int; Y: int }
type Position with
  static member (+) ({ X=x1; Y=y1 }, { X=x2; Y=y2 }) = { X=x1 + x2; Y=y1 + y2 }

type Board = Map<Position, Cell option>
module Board =
  let width = 10
  let height = 24

type Rotation = Up | Right | Down | Left

type ActivePiece = 
  { Tetromino: Tetromino.T
    Position: Position
    Rotation: Rotation }

[<Measure>] type ms

type GameState = { PlacedBoard: Board
                   ActivePiece: ActivePiece
                   QueuedPieces: Tetromino.T seq
                   LastDrop: int64
                   TickFrequency: float<ms> }

type Model = GameState

type ActivePieceMsg =
  | UpdatePosition of Position
  | OffsetPosition of Position
  | UpdateRotation of Rotation

type Msg =
  | Tick
  | UpdateBoard of Board
  | UpdateActivePiece of ActivePieceMsg