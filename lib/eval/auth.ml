open Eio.Std

type t = { stdout : Eio.Flow.sink_ty r; clock : float Eio.Time.clock_ty r }
