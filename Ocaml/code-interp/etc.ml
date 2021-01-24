(* $Id: etc.ml,v 1.1 2019-01-24 15:47:38-08 - - $ *)

let execname = Filename.basename Sys.argv.(0)

let exit_status_ref = ref 0

let quit () =
    if !Sys.interactive
    then Printf.printf "Quit: exit %d\n%!" !exit_status_ref
    else exit !exit_status_ref

let eprint_list message =
    (exit_status_ref := 1;
     flush_all ();
     List.iter prerr_string message;
     prerr_newline ();
     flush_all ())

let warn message = eprint_list (execname :: ": " :: message)

let die message = (warn message; quit ())

let syntax_error position message =
    warn (position.Lexing.pos_fname :: ": "
            :: string_of_int position.Lexing.pos_lnum :: ": "
            :: message)

let usage_exit message =
    (eprint_list ("Usage: " :: execname :: " " :: message); quit ())

let buffer : string list ref = ref []

let rec read_number () = match !buffer with
    | head::tail -> (buffer := tail;
                     try float_of_string head
                     with Failure _ -> nan)
    | [] -> let line = input_line stdin
            in (buffer := Str.split (Str.regexp "[ \\t]+") line;
                read_number ())

