(* $Id: dumper.ml,v 1.10 2019-01-24 19:14:14-08 - - $ *)

open Absyn

let quote string =
    let regex = Str.regexp "\""
    and subst _ = "\\\""
    in  "\"" ^ Str.global_substitute regex subst string ^ "\""

let join start sep stop list =
    let rec join' list' = match list' with
        | [] -> stop
        | [unit] -> unit ^ stop
        | head::tail -> head ^ sep ^ " " ^ join' tail
     in match list with
        | [] -> start ^ stop
        | _::_ -> start ^ join' list

let str_opt str_fn item = match item with
    | None -> "None"
    | Some thing -> "Some (" ^ str_fn thing ^ ")"

let str_ctor ctor args = join (ctor ^ " (") "," ")" args

let str_list str_fn list = join "[" ";" "]" (List.map str_fn list)


let rec str_printable printable = match printable with
    | Printexpr expr -> str_ctor "Printexpr" [str_expr expr]
    | String string -> str_ctor "String" [quote string]

and str_memref memref = match memref with
    | Arrayref (ident, expr) ->
          str_ctor "Arrayref" [quote ident; str_expr expr]
    | Variable ident -> str_ctor "Variable" [quote ident]

and str_expr expr = match expr with
    | Number number -> str_ctor "Number" [string_of_float number]
    | Memref memref -> str_ctor "Memref" [str_memref memref]
    | Unary (oper, expr) -> str_ctor "Unary" [quote oper; str_expr expr]
    | Binary (oper, expr1, expr2) ->
          str_ctor "Binary" [quote oper; str_expr expr1; str_expr expr2]

let str_stmt (stmt: stmt) = match stmt with
    | Dim (ident, expr) ->
          str_ctor "Dim" [quote ident ^ ", " ^ str_expr expr]
    | Let (memref, expr) ->
          str_ctor "Let" [str_memref memref; str_expr expr]
    | Goto label -> str_ctor "Goto" [quote label]
    | If (expr, label) -> str_ctor "If" [str_expr expr; quote label]
    | Print printable'list ->
          str_ctor "Print" [str_list str_printable printable'list]
    | Input memref'list ->
          str_ctor "Input" [str_list str_memref memref'list]

let dump_progline (linenr, label'opt, stmt'opt) =
    Printf.fprintf stderr "%3d %s: %s\n%!" linenr
        (str_opt (fun x -> x) label'opt)
        (str_opt str_stmt stmt'opt)

let dump_program (program : program) =
    List.iter dump_progline program

