module FSExtend

type MaybeBuilder() =
  member __.Bind(m, f) = Option.bind f m
  member __.Return(x) = Some x
let maybe = new MaybeBuilder()

type Result<'a, 'b> = 
  | Success of 'a
  | Failure of 'b