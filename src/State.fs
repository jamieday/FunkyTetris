module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Page
open Types

let pageParser: Parser<Page->Page,Page> =
  oneOf [
    map Game (s "game")
  ]

let urlUpdate (result: Option<Page>) model =
  match result with
  | None ->
    console.error("Error parsing url")
    model,Navigation.modifyUrl <| (toMeta model.currentPage).Hash
  | Some page ->
      { model with currentPage = page }, []

let init result =
  let (game, gameCmd) = Game.State.init()
  let (model, cmd) =
    urlUpdate result
      { currentPage = Game
        game = game }
  model, Cmd.batch [ cmd
                     Cmd.map GameMsg gameCmd ]

let update msg model =
  match msg with
  | GameMsg msg ->
      let (game, gameCmd) = Game.State.update msg model.game
      { model with game = game }, Cmd.map GameMsg gameCmd
