module App.Types

open Page

type Msg =
  | GameMsg of Game.Types.Model.Msg

type Model = {
    currentPage: Page
    game: Game.Types.Model.Model }
