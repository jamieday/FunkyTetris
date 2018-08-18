module App.Types

open Page

type Msg =
  | GameMsg of Game.Types.Msg

type Model = {
    currentPage: Page
    game: Game.Types.Model }
