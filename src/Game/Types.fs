module Game.Types

open Game.Types.Game

type Model = GameState

type Msg =
  | Tick
  | TogglePaused
  | UpdateActivePiece of Game.Types.Game.ActivePieceMsg