(* $Id: tables.ml,v 1.3 2019-01-24 17:08:37-08 - - $ *)

type variable_table_t = (string, float) Hashtbl.t
let variable_table : variable_table_t = Hashtbl.create 16
let _ = List.map (fun (label, value) ->
                  Hashtbl.add variable_table label value)
                 ["e"  , exp 1.0;
                  "eof", 0.0;
                  "pi" , acos ~-.1.0;
                  "nan", nan]

type array_table_t = (string, float array) Hashtbl.t
let array_table : array_table_t = Hashtbl.create 16

type unary_fn_table_t = (string, float -> float) Hashtbl.t
let unary_fn_table : unary_fn_table_t = Hashtbl.create 16
let _ = List.map (fun (label, value) ->
                  Hashtbl.add unary_fn_table label value)
                 ["+"    , (~+.);
                  "-"    , (~-.);
                  "abs"  , abs_float;
                  "acos" , acos;
                  "asin" , asin;
                  "atan" , atan;
                  "ceil" , ceil;
                  "cos"  , cos;
                  "exp"  , exp;
                  "floor", floor;
                  "log"  , log;
                  "log10", log10;
                  "log2" , (fun x -> log x /. log 2.0);
                  "round", (fun x -> floor (x +. 0.5));
                  "sin"  , sin;
                  "sqrt" , sqrt;
                  "tan"  , tan]

type binary_fn_table_t = (string, float -> float -> float) Hashtbl.t
let binary_fn_table : binary_fn_table_t = Hashtbl.create 16
let _ = List.map (fun (label, value) ->
                  Hashtbl.add binary_fn_table label value)
                 ["+", (+.);
                  "-", (-.);
                  "*", ( *.);
                  "/", (/.);
                  "%", mod_float;
                  "^", ( ** )]


type label_table_t = (string, Absyn.program) Hashtbl.t
let label_table : label_table_t = Hashtbl.create 16

let rec init_label_table program =
    let rec init program =  match program with
        | [] -> ()
        | (_, Some label, _)::rest ->
              (Hashtbl.add label_table label program; init rest)
        | _::rest -> init rest
    in (Hashtbl.reset label_table; init program)

let dump_label_table () =
    let dump key value = match value with
        | [] -> ()
        | (line, _, _)::_ ->
          Printf.fprintf stderr
              "label_table: \"%s\" -> line %d\n%!" key line
    in Hashtbl.iter dump label_table

