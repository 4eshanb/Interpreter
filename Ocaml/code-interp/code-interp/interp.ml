(* $Id: interp.ml,v 1.6 2019-01-24 19:14:14-08 - - $ *)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> unimpl "eval_expr Memref"
    | Unary (oper, expr) -> unimpl "eval_expr Unary"
    | Binary (oper, expr1, expr2) -> unimpl "eval_expr Binary"

let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())

let interp_input (memref_list : Absyn.memref list) =
    let input_number memref =
        try  let number = Etc.read_number ()
             in (print_float number; print_newline ())
        with End_of_file -> 
             (print_string "End_of_file"; print_newline ())
    in List.iter input_number memref_list

let interp_stmt (stmt : Absyn.stmt) = match stmt with
    | Dim (ident, expr) -> unimpl "Dim (ident, expr)"
    | Let (memref, expr) -> unimpl "Let (memref, expr)"
    | Goto labsl -> unimpl "Goto labsl"
    | If (expr, label) -> unimpl "If (expr, label)"
    | Print print_list -> interp_print print_list
    | Input memref_list -> interp_input memref_list

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> (interp_stmt stmt; interpret otherlines)

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

