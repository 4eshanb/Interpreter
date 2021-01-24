(* $Id: interp.mli,v 1.5 2019-01-24 17:08:37-08 - - $ *)

(*
* Interpreter for Silly Basic
*)

val want_dump : bool ref

val interpret_program : Absyn.program -> unit

