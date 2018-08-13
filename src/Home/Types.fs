module Home.Types

open Home.Types.Game

type Model = GameState

type Msg =
  | Tick
  | UpdateBoard of Home.Types.Board.Board
  | UpdateActivePiece of ActivePieceMsg