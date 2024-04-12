open! ContainersLabels
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

module Map = Map.Make (Id)
open Result.Infix

type function_type = Top | Function
type obj_type = Class | Subclass | Not

let declare scopes name =
  match scopes with [] -> [] | cur :: rest -> Map.add name false cur :: rest

let declaring scopes name =
  match scopes with
  | cur :: _ -> not @@ Map.get_or name cur ~default:true
  | [] -> false

let define scopes name =
  match scopes with [] -> [] | cur :: rest -> Map.add name true cur :: rest

let transform_var scopes var =
  let rec loop acc = function
    | [] -> (var, None)
    | cur :: _ when Map.mem var cur -> (var, Some acc)
    | _ :: rest -> loop (acc + 1) rest
  in
  loop 0 scopes

let rec decl ftype otype scopes :
    input -> (bool Map.t list * t, [> Error.t ]) result = function
  | Ast.Var (id, init) ->
      let scopes = declare scopes id in
      let+ init = expr otype scopes init in
      (define scopes id, Ast.Var (id, init))
  | Ast.Fun (name, f) ->
      let scopes = define (declare scopes name) name in
      let+ f = handle_func otype scopes f in
      (scopes, Ast.Fun (name, f))
  | Ast.Stmt s ->
      let+ s = stmt ftype otype scopes s in
      (scopes, Ast.Stmt s)
  | Ast.Class { name; superclass = Some (Variable super); _ }
    when Id.(name = super) ->
      Error.of_string "a class can't inherit from itself."
  | Ast.Class { name; superclass; methods } ->
      let superclass =
        Option.map
          (function
            | Ast.Variable var -> Var (transform_var scopes var)
            | _ -> assert false)
          superclass
      in
      let scopes = define (declare scopes name) name
      and names, funcs = List.split methods in
      let class_scope =
        match superclass with
        | Some _ -> Map.singleton (Id.of_string "super") true :: scopes
        | None -> scopes
      in
      let inner =
        List.fold_left names
          ~init:(Map.singleton (Id.of_string "this") true :: class_scope)
          ~f:(fun scopes name -> define (declare scopes name) name)
      in
      let+ funcs =
        Result.map_l
          (handle_func
             (match superclass with None -> Class | Some _ -> Subclass)
             inner)
          funcs
      in
      ( scopes,
        Ast.Class { name; superclass; methods = List.combine names funcs } )

and handle_func otype scopes { params; body } =
  let duplicate_params =
    params
    |> List.map ~f:(fun x -> (x, 1))
    |> Map.of_list_with ~f:(Fun.const ( + ))
    |> Map.filter (Fun.const (( < ) 1))
  in
  if Map.is_empty duplicate_params then
    let inner = List.fold_left ~init:(Map.empty :: scopes) ~f:define params in
    let+ body = stmt Function otype inner body in
    { Ast.params; body }
  else
    Error.of_string
      [%string
        "can't write a function with duplicate parameters (parameters: \
         %{params |> List.to_string Id.to_string})"]

and stmt ftype otype scopes = function
  | Ast.Expr e ->
      let+ e = expr otype scopes e in
      Ast.Expr e
  | Ast.Log e ->
      let+ e = e |> Result.map_l (expr otype scopes) in
      Ast.Log e
  | Ast.Block decls ->
      let+ _, body =
        Result.fold_l
          (fun (scopes, acc) d ->
            let+ scopes, decl = decl ftype otype scopes d in
            (scopes, decl :: acc))
          (Map.empty :: scopes, [])
          decls
      in
      Ast.Block (List.rev body)
  | Ast.If { condition; if_true; if_false } -> (
      let* condition = expr otype scopes condition
      and* if_true = stmt ftype otype scopes if_true in
      match if_false with
      | None -> Ok (Ast.If { condition; if_true; if_false = None })
      | Some if_false ->
          let+ if_false = stmt ftype otype scopes if_false in
          Ast.If { condition; if_true; if_false = Some if_false })
  | Ast.While { condition; body } ->
      let+ condition = expr otype scopes condition
      and+ body = stmt ftype otype scopes body in
      Ast.While { condition; body }
  | Ast.Return e -> (
      match (ftype, e) with
      | Top, _ -> Error.of_string "can't return outside function"
      | _, None -> Ok (Ast.Return None)
      | _, Some e ->
          let+ e = expr otype scopes e in
          Ast.Return (Some e))

and expr otype scopes = function
  | Ast.Binary (l, op, r) ->
      let+ l = expr otype scopes l and+ r = expr otype scopes r in
      Ast.Binary (l, op, r)
  | Ast.Unary (op, e) ->
      let+ e = expr otype scopes e in
      Ast.Unary (op, e)
  | Ast.Grouping e ->
      let+ e = expr otype scopes e in
      Ast.Grouping e
  | Ast.Assign (p, e) ->
      let+ p = place otype scopes p and+ e = expr otype scopes e in
      Ast.Assign (p, e)
  | Ast.Logic (l, op, r) ->
      let+ l = expr otype scopes l and+ r = expr otype scopes r in
      Ast.Logic (l, op, r)
  | Ast.Call (f, args) ->
      let+ f = expr otype scopes f
      and+ args = Result.map_l (expr otype scopes) args in
      Ast.Call (f, args)
  | Ast.Lambda f ->
      let+ f = handle_func otype scopes f in
      Ast.Lambda f
  | Ast.Literal lit -> Ok (Ast.Literal lit)
  | Ast.Get p ->
      let+ p = place otype scopes p in
      Ast.Get p

and place otype scopes = function
  | Variable var ->
      if declaring scopes var then
        Error.of_string
          [%string
            {|can't reference "%{Id.to_string var}" while shadowing it.|}]
      else Ok (Var (transform_var scopes var))
  | Ast.This -> (
      match otype with
      | Class | Subclass ->
          Ok (Var (transform_var scopes @@ Id.of_string "this"))
      | Not -> Error.of_string {|can't reference "this" outside a class|})
  | Ast.Field (obj, attr) ->
      let+ obj = expr otype scopes obj in
      Field (obj, attr)
  | Ast.Super attr -> (
      match otype with
      | Subclass ->
          Ok
            (Super
               {
                 super = transform_var scopes @@ Id.of_string "super";
                 this = transform_var scopes @@ Id.of_string "this";
                 attr;
               })
      | Class ->
          Error.of_string
            {|can't reference "super" in a class with no superclass|}
      | Not -> Error.of_string {|can't reference "super" outside a class|})

let go ast =
  let+ scopes, ast =
    Result.fold_l
      (fun (scopes, acc) d ->
        let+ scopes, d = decl Top Not scopes d in
        (scopes, d :: acc))
      ([], []) ast
  in
  assert (List.is_empty scopes);
  List.rev ast
