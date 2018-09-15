module Game.Types.Board

type Color =
  | Cyan
  | Blue
  | Orange
  | Yellow
  | Green
  | Purple
  | Red
  | Gray

type Cell =
  | Fragment of Color
  | Ghost

type Position = { X: int; Y: int }
type Position with
  static member (+) ({ X=x1; Y=y1 }, { X=x2; Y=y2 }) = { X=x1 + x2; Y=y1 + y2 }

type Board = Map<Position, Cell option>
module Board =
  let width = 10
  let height = 24