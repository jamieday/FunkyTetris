module Home.Types.Tetromino

type T =
  | L
  | Z
  | S
  | T
  | J
  | O

type MetaInfo = { Color: Color }

let toMeta = function
  | I -> { Color = Color.Cyan }
  | J -> { Color = Color.Blue }
  | L -> { Color = Color.Orange }
  | O -> { Color = Color.Yellow }
  | S -> { Color = Color.Green }
  | T -> { Color = Color.Purple }
  | Z -> { Color = Color.Red }

let structure _rot tetrimino =
  ( match tetrimino with
    | L -> [ (0, 0); (1, 0); (0, -1); (0, -2) ]
    | Z -> [ (-1, 0); (0, 0); (0, 1); (1, 1) ]
    | S -> [ (1, 0); (0, 0); (0, 1); (-1, 1) ]
    | T -> [ (0, 1); (0, 0); (-1, 0); (1, 0) ]
    | J -> [ (0, 0); (-1, 0); (0, -1); (0, -2) ]
    | O -> [ (0, 0); (0, 1); (1, 1); (1, 0) ] )
    |> List.map (fun (x, y) -> { X=x; Y=y })