module Navbar.View

open Fable.Helpers.React
open Fable.Helpers.React.Props

let navButton classy href faClass txt =
    p
        [ ClassName "control" ]
        [ a
            [ ClassName (sprintf "button %s" classy)
              Href href ]
            [ span
                [ ClassName "icon" ]
                [ i
                    [ ClassName (sprintf "fa %s" faClass) ]
                    [ ] ]
              span
                [ ]
                [ str txt ] ] ]

let navButtons =
    span
        [ ClassName "nav-item" ]
        [ div
            [ ClassName "field is-grouped" ]
              [ navButton "jamieday" "https://www.jamieday.app" "fa-fort-awesome" "Jamie's Sandbox" 
                navButton "github" "https://github.com/jamieday/FunkyTetris" "fa-github" "Github"] ]

let root =
    nav
        [ ClassName "nav" ]
        [ div
            [ ClassName "nav-left" ]
            [ h1
                [ ClassName "nav-item title is-4 is-brand" ]
                [ str "Funky Tetris" ] ]
          div
            [ ClassName "nav-center" ]
            [ span
                [ ClassName "nav-item" ]
                [ str "Created with 😐, made purely in F#" ] ]
          div
            [ ClassName "nav-right" ]
            [ navButtons ] ]
