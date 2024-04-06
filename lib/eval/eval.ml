open! ContainersLabels
open! Eio.Std
open Reader

type error = Error.t

module Obj = Obj
module Env = Env

type t = Interpret.t =
  | Value of Obj.t
  | Binding of Ast.Id.t * Obj.t * Env.t
  | Void

let eval env ast =
  Result.(Returns_check.go ast >>= Resolver.go >>= Interpret.go env)
