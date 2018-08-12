module Home.Types

type Color =
  | Red
  | Green
  | Blue

let toCssColor = function
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"

type Cell =
  { Color: Color }

type Position = { X: int; Y: int }
type Position with
  static member (+) ({ X=x1; Y=y1 }, { X=x2; Y=y2 }) = { X=x1 + x2; Y=y1 + y2 }

type Board = Map<Position, Cell option>
module Board =
  let width = 10
  let height = 24

type Tetromino =
  | L
  | Z
  | S
  | T
  | J
  | O

type Rotation = Up | Right | Down | Left

let structure _rot tetrimino =
  ( match tetrimino with
    | L -> [ (0, 0); (1, 0); (0, -1); (0, -2) ]
    | Z -> [ (-1, 0); (0, 0); (0, 1); (1, 1) ]
    | S -> [ (1, 0); (0, 0); (0, 1); (-1, 1) ]
    | T -> [ (0, 1); (0, 0); (-1, 0); (1, 0) ]
    | J -> [ (0, 0); (-1, 0); (0, -1); (0, -2) ]
    | O -> [ (0, 0); (0, 1); (1, 1); (1, 0) ] )
    |> List.map (fun (x, y) -> { X=x; Y=y })

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

type Model = GameState

type ActivePieceMsg =
  | UpdatePosition of Position
  | OffsetPosition of Position
  | UpdateRotation of Rotation

type Msg =
  | Tick
  | UpdateBoard of Board
  | UpdateActivePiece of ActivePieceMsg