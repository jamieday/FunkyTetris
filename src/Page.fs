module Page

type Page =
  | Game

type Meta =
  { Hash: string }

let toMeta =
  function
  | Game -> { Hash = "#game" }