module Home.Types.Tetromino

open Home.Types.Board

type Tetromino =
  | L
  | Z
  | S
  | T
  | J
  | O

type Rotation = Up | Right | Down | Left

type MetaInfo = { Color: Color }

let toMeta = function
  | I -> { Color = Color.Cyan }
  | J -> { Color = Color.Blue }
  | L -> { Color = Color.Orange }
  | O -> { Color = Color.Yellow }
  | S -> { Color = Color.Green }
  | T -> { Color = Color.Purple }
  | Z -> { Color = Color.Red }

let structure (rot: Rotation) tetrimino =
  ( match tetrimino with
    | L -> [ (0, 0); (1, 0); (0, -1); (0, -2) ]
    | Z -> [ (-1, 0); (0, 0); (0, 1); (1, 1) ]
    | S -> [ (1, 0); (0, 0); (0, 1); (-1, 1) ]
    | T -> [ (0, 1); (0, 0); (-1, 0); (1, 0) ]
    | J -> [ (0, 0); (-1, 0); (0, -1); (0, -2) ]
    | O -> [ (0, 0); (0, 1); (1, 1); (1, 0) ] )
    |> Seq.ofList
    |> Seq.map ((fun (x, y) ->
                  match rot with
                  | Up -> (x, y)
                  | Right -> (-y, -x)
                  | Down -> (x, y)
                  | Left -> (x, y))
                >> fun (x, y) -> { X=x; Y=y })