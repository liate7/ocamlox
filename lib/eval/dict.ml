open! ContainersLabels
open Reader
open Option.Infix
module M = Hashtbl.Make (Id)

type 'value t =
  | Toplevel of 'value M.t
  | Inner of { global : 'value M.t; locals : 'value M.t list }

let of_seq seq = Toplevel (M.of_seq seq)

let child_of = function
  | Toplevel global -> Inner { global; locals = [ M.create 2 ] }
  | Inner { global; locals } -> Inner { global; locals = M.create 2 :: locals }

let find_opt ~depth t key =
  match (t, depth) with
  | Toplevel global, None -> M.find_opt global key
  | Toplevel _, Some _ -> None
  | Inner { global; _ }, None -> M.find_opt global key
  | Inner { locals; _ }, Some depth ->
      let* hash = List.nth_opt locals depth in
      M.find_opt hash key

let add t key value =
  match t with
  | Toplevel global -> M.replace global key value
  | Inner { locals = closest :: _; _ } -> M.replace closest key value
  | Inner { locals = []; _ } -> failwith "invariant broken, empty locals list"

let replace ~depth t key value =
  let go hash =
    let+ prev = M.find_opt hash key in
    M.replace hash key value;
    prev
  in
  match (t, depth) with
  | Toplevel global, None -> go global
  | Toplevel _, Some _ -> None
  | Inner { global; _ }, None -> go global
  | Inner { locals; _ }, Some depth -> List.nth_opt locals depth >>= go

let height = function
  | Toplevel _ -> 0
  | Inner { locals; _ } -> 1 + List.length locals
