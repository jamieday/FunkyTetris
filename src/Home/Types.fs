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
  static member (+) (p1, p2) = { X=p1.X + p2.X; Y=p1.Y + p2.Y }

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
                   LastTick: int64
                   TickFrequency: float<ms> }

type Model = GameState

type ActivePieceMsg =
  | UpdatePosition of Position
  | UpdateRotation of Rotation

type Msg =
  | Tick
  | UpdateBoard of Board
  | UpdateActivePiece of ActivePieceMsg