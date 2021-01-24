type token =
  | RELOP of (string)
  | EQUAL of (string)
  | ADDOP of (string)
  | MULOP of (string)
  | POWOP of (string)
  | IDENT of (string)
  | NUMBER of (string)
  | STRING of (string)
  | COLON
  | COMMA
  | LPAR
  | RPAR
  | LSUB
  | RSUB
  | EOL
  | EOF
  | DIM
  | LET
  | GOTO
  | IF
  | PRINT
  | INPUT

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"

let linenr () = (symbol_start_pos ()).Lexing.pos_lnum

let syntax () = Etc.syntax_error (symbol_start_pos ()) ["syntax error"]

# 34 "parser.ml"
let yytransl_const = [|
  265 (* COLON *);
  266 (* COMMA *);
  267 (* LPAR *);
  268 (* RPAR *);
  269 (* LSUB *);
  270 (* RSUB *);
  271 (* EOL *);
    0 (* EOF *);
  272 (* DIM *);
  273 (* LET *);
  274 (* GOTO *);
  275 (* IF *);
  276 (* PRINT *);
  277 (* INPUT *);
    0|]

let yytransl_block = [|
  257 (* RELOP *);
  258 (* EQUAL *);
  259 (* ADDOP *);
  260 (* MULOP *);
  261 (* POWOP *);
  262 (* IDENT *);
  263 (* NUMBER *);
  264 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\009\000\009\000\011\000\011\000\010\000\010\000\007\000\007\000\
\008\000\008\000\006\000\006\000\012\000\012\000\013\000\013\000\
\014\000\014\000\014\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\000\000\002\000\001\000\001\000\000\000\
\002\000\005\000\004\000\002\000\004\000\002\000\001\000\002\000\
\003\000\001\000\001\000\001\000\003\000\001\000\001\000\004\000\
\003\000\003\000\003\000\001\000\003\000\001\000\003\000\001\000\
\003\000\002\000\001\000\001\000\004\000\002\000"

let yydefred = "\000\000\
\004\000\000\000\038\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\003\000\009\000\000\000\000\000\000\000\012\000\000\000\000\000\
\035\000\000\000\000\000\036\000\000\000\000\000\030\000\000\000\
\020\000\000\000\014\000\000\000\000\000\016\000\002\000\005\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\000\000\000\000\000\000\013\000\029\000\
\031\000\017\000\021\000\010\000\024\000\037\000"

let yydgoto = "\002\000\
\003\000\004\000\014\000\015\000\016\000\034\000\028\000\029\000\
\035\000\038\000\036\000\030\000\031\000\032\000"

let yysindex = "\023\000\
\000\000\000\000\000\000\001\000\243\254\019\255\000\000\043\255\
\047\255\050\255\060\255\089\255\047\255\036\255\064\255\000\000\
\000\000\000\000\044\255\049\255\062\255\000\000\060\255\001\255\
\000\000\060\255\045\255\000\000\051\255\061\255\000\000\069\255\
\000\000\072\255\000\000\077\255\079\255\000\000\000\000\000\000\
\060\255\060\255\060\255\000\000\060\255\024\255\060\255\060\255\
\060\255\087\255\060\255\060\255\089\255\047\255\002\255\008\255\
\072\255\026\255\000\000\072\255\072\255\061\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\083\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\084\255\000\000\000\000\086\255\000\000\
\000\000\000\000\000\000\011\255\000\000\000\000\000\000\005\255\
\000\000\000\000\000\000\000\000\000\000\058\255\000\000\040\255\
\000\000\015\255\000\000\088\255\090\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\091\255\000\000\000\000\092\255\093\255\076\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\087\000\248\255\247\255\000\000\
\051\000\053\000\000\000\059\000\236\255\086\000"

let yytablesize = 278
let yytable = "\021\000\
\007\000\017\000\027\000\037\000\049\000\023\000\023\000\023\000\
\023\000\023\000\049\000\045\000\023\000\042\000\023\000\068\000\
\023\000\046\000\023\000\023\000\023\000\069\000\023\000\001\000\
\019\000\023\000\049\000\018\000\049\000\019\000\064\000\065\000\
\055\000\056\000\057\000\059\000\058\000\070\000\060\000\061\000\
\032\000\032\000\032\000\032\000\037\000\047\000\048\000\049\000\
\019\000\032\000\039\000\032\000\020\000\032\000\032\000\022\000\
\041\000\032\000\028\000\028\000\028\000\042\000\023\000\043\000\
\051\000\024\000\025\000\028\000\050\000\028\000\026\000\028\000\
\028\000\052\000\049\000\028\000\027\000\027\000\027\000\008\000\
\009\000\010\000\011\000\012\000\013\000\027\000\053\000\027\000\
\054\000\027\000\027\000\023\000\063\000\027\000\024\000\025\000\
\033\000\008\000\015\000\026\000\007\000\040\000\018\000\066\000\
\022\000\011\000\067\000\062\000\044\000\025\000\026\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\009\000\010\000\011\000\012\000\013\000"

let yycheck = "\009\000\
\000\000\015\001\011\000\013\000\003\001\001\001\002\001\003\001\
\004\001\005\001\003\001\011\001\002\001\013\001\010\001\014\001\
\012\001\026\000\014\001\015\001\010\001\014\001\018\001\001\000\
\010\001\015\001\003\001\009\001\003\001\015\001\051\000\052\000\
\041\000\042\000\043\000\012\001\045\000\012\001\047\000\048\000\
\001\001\002\001\003\001\004\001\054\000\001\001\002\001\003\001\
\006\001\010\001\015\001\012\001\006\001\014\001\015\001\006\001\
\013\001\018\001\001\001\002\001\003\001\013\001\003\001\002\001\
\004\001\006\001\007\001\010\001\018\001\012\001\011\001\014\001\
\015\001\005\001\003\001\018\001\001\001\002\001\003\001\016\001\
\017\001\018\001\019\001\020\001\021\001\010\001\010\001\012\001\
\010\001\014\001\015\001\003\001\006\001\018\001\006\001\007\001\
\008\001\015\001\015\001\011\001\015\001\015\000\015\001\053\000\
\015\001\015\001\054\000\049\000\023\000\018\001\018\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\255\255\255\255\255\255\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001"

let yynames_const = "\
  COLON\000\
  COMMA\000\
  LPAR\000\
  RPAR\000\
  LSUB\000\
  RSUB\000\
  EOL\000\
  EOF\000\
  DIM\000\
  LET\000\
  GOTO\000\
  IF\000\
  PRINT\000\
  INPUT\000\
  "

let yynames_block = "\
  RELOP\000\
  EQUAL\000\
  ADDOP\000\
  MULOP\000\
  POWOP\000\
  IDENT\000\
  NUMBER\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 22 "parser.mly"
                                      (List.rev _1)
# 228 "parser.ml"
               : Absyn.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 24 "parser.mly"
                                      (_2::_1)
# 236 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_list) in
    Obj.repr(
# 25 "parser.mly"
                                      (syntax (); _1)
# 243 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
                                      ([])
# 249 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action) in
    Obj.repr(
# 28 "parser.mly"
                                      ((linenr (), Some _1, Some _2))
# 257 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'action) in
    Obj.repr(
# 29 "parser.mly"
                                      ((linenr (), None, Some _1))
# 264 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 30 "parser.mly"
                                      ((linenr (), Some _1, None))
# 271 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "parser.mly"
                                      ((linenr (), None, None))
# 277 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 33 "parser.mly"
                                      (_1)
# 284 "parser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                                      (Absyn.Dim (_2, _4))
# 292 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'memref) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                                      (Absyn.Let (_2, _4))
# 301 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parser.mly"
                                      (Absyn.Goto _2)
# 308 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'relexpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
                                      (Absyn.If (_2, _4))
# 316 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'print_list) in
    Obj.repr(
# 39 "parser.mly"
                                      (Absyn.Print _2)
# 323 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                      (Absyn.Print ([]))
# 329 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'input_list) in
    Obj.repr(
# 41 "parser.mly"
                                      (Absyn.Input _2)
# 336 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'print) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'print_list) in
    Obj.repr(
# 43 "parser.mly"
                                      (_1::_3)
# 344 "parser.ml"
               : 'print_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'print) in
    Obj.repr(
# 44 "parser.mly"
                                      ([_1])
# 351 "parser.ml"
               : 'print_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                                      (Absyn.Printexpr _1)
# 358 "parser.ml"
               : 'print))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 47 "parser.mly"
                                      (Absyn.String _1)
# 365 "parser.ml"
               : 'print))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'memref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'input_list) in
    Obj.repr(
# 49 "parser.mly"
                                      (_1::_3)
# 373 "parser.ml"
               : 'input_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'memref) in
    Obj.repr(
# 50 "parser.mly"
                                      ([_1])
# 380 "parser.ml"
               : 'input_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                                      (Absyn.Variable _1)
# 387 "parser.ml"
               : 'memref))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                                      (Absyn.Arrayref (_1, _3))
# 395 "parser.ml"
               : 'memref))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                                      (Absyn.Binary (_2, _1, _3))
# 404 "parser.ml"
               : 'relexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                                      (Absyn.Binary (_2, _1, _3))
# 413 "parser.ml"
               : 'relexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 59 "parser.mly"
                                      (Absyn.Binary (_2, _1, _3))
# 422 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 60 "parser.mly"
                                      (_1)
# 429 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 62 "parser.mly"
                                      (Absyn.Binary (_2, _1, _3))
# 438 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 63 "parser.mly"
                                      (_1)
# 445 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'primary) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 65 "parser.mly"
                                      (Absyn.Binary (_2, _1, _3))
# 454 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary) in
    Obj.repr(
# 66 "parser.mly"
                                      (_1)
# 461 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                      (_2)
# 468 "parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary) in
    Obj.repr(
# 69 "parser.mly"
                                      (Absyn.Unary (_1, _2))
# 476 "parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 70 "parser.mly"
                                      (Absyn.Number (float_of_string _1))
# 483 "parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'memref) in
    Obj.repr(
# 71 "parser.mly"
                                      (Absyn.Memref _1)
# 490 "parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                                      (Absyn.Unary (_1, _3))
# 498 "parser.ml"
               : 'primary))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Absyn.program)
