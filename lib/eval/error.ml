open! Eio.Std

type t = [ `Error of string ]

let of_string msg = Error (`Error msg)
