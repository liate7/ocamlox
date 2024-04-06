module type S = sig
  type input
  type t

  val go : input list -> (t list, [> Error.t ]) result
end
