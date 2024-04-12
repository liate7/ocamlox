open! ContainersLabels
open! Eio.Std

val eval_file :
  [> Eio.Flow.source_ty ] r ->
  < stdin : [> Eio.Flow.source_ty ] r
  ; stdout : [> Eio.Flow.sink_ty ] r
  ; clock : [> float Eio.Time.clock_ty ] r
  ; .. > ->
  (int, string) result

val repl :
  < stdin : [> Eio.Flow.source_ty ] r
  ; stdout : [> Eio.Flow.sink_ty ] r
  ; clock : [> float Eio.Time.clock_ty ] r
  ; .. > ->
  (int, string) result
