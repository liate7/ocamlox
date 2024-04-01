open! ContainersLabels
open! Eio.Std

val eval_file : string -> Eio_linux.stdenv -> (int, string) result
val repl : Eio_linux.stdenv -> (int, string) result
