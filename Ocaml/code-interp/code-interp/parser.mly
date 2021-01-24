/* $Id: parser.mly,v 1.1 2019-01-24 15:47:38-08 - - $ */

%{

let linenr () = (symbol_start_pos ()).Lexing.pos_lnum

let syntax () = Etc.syntax_error (symbol_start_pos ()) ["syntax error"]

%}

%token <string> RELOP EQUAL ADDOP MULOP POWOP
%token <string> IDENT NUMBER STRING
%token COLON COMMA LPAR RPAR LSUB RSUB EOL EOF
%token DIM LET GOTO IF PRINT INPUT

%type <Absyn.program> program

%start program

%%

program    : stmt_list EOF            {List.rev $1}

stmt_list  : stmt_list stmt EOL       {$2::$1}
           | stmt_list error EOL      {syntax (); $1}
           |                          {[]}

stmt       : label action             {(linenr (), Some $1, Some $2)}
           | action                   {(linenr (), None, Some $1)}
           | label                    {(linenr (), Some $1, None)}
           |                          {(linenr (), None, None)}

label      : IDENT COLON              {$1}

action     : DIM IDENT LSUB expr RSUB {Absyn.Dim ($2, $4)}
           | LET memref EQUAL expr    {Absyn.Let ($2, $4)}
           | GOTO IDENT               {Absyn.Goto $2}
           | IF relexpr GOTO IDENT    {Absyn.If ($2, $4)}
           | PRINT print_list         {Absyn.Print $2}
           | PRINT                    {Absyn.Print ([])}
           | INPUT input_list         {Absyn.Input $2}

print_list : print COMMA print_list   {$1::$3}
           | print                    {[$1]}

print      : expr                     {Absyn.Printexpr $1}
           | STRING                   {Absyn.String $1}

input_list : memref COMMA input_list  {$1::$3}
           | memref                   {[$1]}


memref     : IDENT                    {Absyn.Variable $1}
           | IDENT LSUB expr RSUB     {Absyn.Arrayref ($1, $3)}

relexpr    : expr RELOP expr          {Absyn.Binary ($2, $1, $3)}
           | expr EQUAL expr          {Absyn.Binary ($2, $1, $3)}

expr       : expr ADDOP term          {Absyn.Binary ($2, $1, $3)}
           | term                     {$1}

term       : term MULOP factor        {Absyn.Binary ($2, $1, $3)}
           | factor                   {$1}

factor     : primary POWOP factor     {Absyn.Binary ($2, $1, $3)}
           | primary                  {$1}

primary    : LPAR expr RPAR           {$2}
           | ADDOP primary            {Absyn.Unary ($1, $2)}
           | NUMBER                   {Absyn.Number (float_of_string $1)}
           | memref                   {Absyn.Memref $1}
           | IDENT LPAR expr RPAR     {Absyn.Unary ($1, $3)}

