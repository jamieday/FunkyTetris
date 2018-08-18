module Game.Types

open Game.Types.Game

type Model = GameState

type Msg =
  | Tick
  | UpdateBoard of Game.Types.Board.Board
  | UpdateActivePiece of Game.Types.Game.ActivePieceMsg