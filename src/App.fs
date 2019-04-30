module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core.JsInterop
open Types
open App.State
open Page

importAll "./style.sass"

open Fable.Helpers.React
open Fable.Helpers.React.Props

let root model dispatch =

  let pageHtml =
    function
    | Page.Game -> Game.View.root model.game (GameMsg >> dispatch)


  div
    [ ClassName "root-content" ]
    [ div
        [ ClassName "header" ]
        [ h1 [ Style [ Display "inline-block" ] ] [ str "Funky Tetris" ]
          h2 [ ClassName "lead text-muted" ] [ str "Developed in F# with Fable, Elmish, model-view-update architecture"] ]
      pageHtml model.currentPage ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "funky-tetris"
|> Program.run
