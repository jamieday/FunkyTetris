module Page

type Page =
  | Home
  | Counter
  | About

type Meta =
  { Hash: string }

let toMeta =
  function
  | About -> { Hash = "#about" }
  | Counter -> { Hash = "#counter" }
  | Home -> { Hash = "#home" }