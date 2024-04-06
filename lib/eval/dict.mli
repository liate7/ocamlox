type 'value t

val of_seq : (Reader.Id.t * 'a) Seq.t -> 'a t
val child_of : 'value t -> 'value t
val find_opt : depth:int option -> 'value t -> Reader.Id.t -> 'value option
val add : 'value t -> Reader.Id.t -> 'value -> unit

val replace :
  depth:int option -> 'value t -> Reader.Id.t -> 'value -> 'value option

val height : _ t -> int
