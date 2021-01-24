(* $Id: scanner.mll,v 1.1 2019-01-24 15:47:38-08 - - $ *)

{

let lexerror lexbuf =
    Etc.syntax_error (Lexing.lexeme_start_p lexbuf)
            ["invalid character `" ^ (Lexing.lexeme lexbuf) ^ "'"]

let newline lexbuf =
    let incr pos =
        {pos with Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                  Lexing.pos_bol = pos.Lexing.pos_cnum}
    in  (lexbuf.Lexing.lex_start_p <- incr lexbuf.Lexing.lex_start_p;
         lexbuf.Lexing.lex_curr_p <- incr lexbuf.Lexing.lex_curr_p)

let lexeme = Lexing.lexeme

}

let letter          = ['a'-'z' 'A'-'Z' '_']
let digit           = ['0'-'9']
let fraction        = (digit+ '.'? digit* | '.' digit+)
let exponent        = (['E' 'e'] ['+' '-']? digit+)

let comment         = ('#' [^'\n']*)
let ident           = (letter (letter | digit)*)
let number          = (fraction exponent?)
let string          = '"' [^'\n' '"']* '"'


rule token     = parse
    | eof           { Parser.EOF }
    | [' ' '\t']    { token lexbuf }
    | comment       { token lexbuf }
    | "\n"          { newline lexbuf; Parser.EOL }
    | ":"           { Parser.COLON }
    | ","           { Parser.COMMA }
    | "("           { Parser.LPAR }
    | ")"           { Parser.RPAR }
    | "["           { Parser.LSUB }
    | "]"           { Parser.RSUB }
    | "="           { Parser.EQUAL (lexeme lexbuf) }
    | "<>"          { Parser.RELOP (lexeme lexbuf) }
    | "<"           { Parser.RELOP (lexeme lexbuf) }
    | "<="          { Parser.RELOP (lexeme lexbuf) }
    | ">"           { Parser.RELOP (lexeme lexbuf) }
    | ">="          { Parser.RELOP (lexeme lexbuf) }
    | "+"           { Parser.ADDOP (lexeme lexbuf) }
    | "-"           { Parser.ADDOP (lexeme lexbuf) }
    | "*"           { Parser.MULOP (lexeme lexbuf) }
    | "/"           { Parser.MULOP (lexeme lexbuf) }
    | "%"           { Parser.MULOP (lexeme lexbuf) }
    | "^"           { Parser.POWOP (lexeme lexbuf) }
    | "dim"         { Parser.DIM }
    | "goto"        { Parser.GOTO }
    | "if"          { Parser.IF }
    | "input"       { Parser.INPUT }
    | "let"         { Parser.LET }
    | "print"       { Parser.PRINT }
    | number        { Parser.NUMBER (lexeme lexbuf) }
    | string        { Parser.STRING (lexeme lexbuf) }
    | ident         { Parser.IDENT (lexeme lexbuf) }
    | _             { lexerror lexbuf; token lexbuf }

