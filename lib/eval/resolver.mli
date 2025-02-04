open Reader

type place =
  | Var of (Id.t * int option)
  | Field of (Ast.literal, place) Ast.expr * Id.t
  | Super of {
      super : Id.t * int option;
      this : Id.t * int option;
      attr : Id.t;
    }

type t = (Ast.literal, place) Ast.t
type input = (Ast.literal, Ast.place) Ast.t

val go : input list -> (t list, [> Error.t ]) result
