type token =
  | VAR of (string)
  | EXPRESSION of (Tree.tree_t)
  | IMPL
  | AND
  | OR
  | NOT
  | OPEN
  | CLOSE
  | EOF
  | COMMA
  | PROOF
  | GENERAL

open Parsing;;
let _ = parse_error;;
# 2 "src/parser.mly"
  open Tree;;
# 20 "src/parser.ml"
let yytransl_const = [|
  259 (* IMPL *);
  260 (* AND *);
  261 (* OR *);
  262 (* NOT *);
  263 (* OPEN *);
  264 (* CLOSE *);
    0 (* EOF *);
  265 (* COMMA *);
  266 (* PROOF *);
  267 (* GENERAL *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* EXPRESSION *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\004\000\004\000\000\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\002\000\003\000\003\000\003\000\
\004\000\003\000\001\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\002\000\003\000\000\000\000\000\013\000\
\000\000\000\000\014\000\000\000\000\000\005\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\004\000\000\000\
\007\000\000\000\010\000\012\000\000\000\009\000"

let yydgoto = "\003\000\
\008\000\011\000\012\000\013\000"

let yysindex = "\009\000\
\002\255\024\255\000\000\000\000\000\000\002\255\002\255\000\000\
\014\000\002\255\000\000\029\255\245\254\000\000\036\255\002\255\
\002\255\002\255\000\000\017\000\002\255\002\255\000\000\042\255\
\000\000\254\254\000\000\000\000\020\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\251\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\001\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\006\000\250\255"

let yytablesize = 281
let yytable = "\022\000\
\008\000\017\000\004\000\005\000\006\000\011\000\009\000\006\000\
\007\000\001\000\002\000\014\000\015\000\019\000\028\000\020\000\
\027\000\000\000\000\000\030\000\000\000\024\000\025\000\026\000\
\004\000\005\000\000\000\029\000\000\000\006\000\007\000\016\000\
\017\000\018\000\010\000\000\000\000\000\021\000\016\000\017\000\
\018\000\000\000\000\000\023\000\016\000\017\000\018\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\008\000\000\000\000\000\
\008\000\008\000\000\000\008\000\006\000\006\000\000\000\006\000\
\016\000\017\000\018\000\016\000\017\000\018\000\016\000\017\000\
\018\000"

let yycheck = "\011\001\
\000\000\004\001\001\001\002\001\000\000\011\001\001\000\006\001\
\007\001\001\000\002\000\006\000\007\000\000\000\021\000\010\000\
\000\000\255\255\255\255\000\000\255\255\016\000\017\000\018\000\
\001\001\002\001\255\255\022\000\255\255\006\001\007\001\003\001\
\004\001\005\001\011\001\255\255\255\255\009\001\003\001\004\001\
\005\001\255\255\255\255\008\001\003\001\004\001\005\001\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\005\001\255\255\255\255\
\008\001\009\001\255\255\011\001\008\001\009\001\255\255\011\001\
\003\001\004\001\005\001\003\001\004\001\005\001\003\001\004\001\
\005\001"

let yynames_const = "\
  IMPL\000\
  AND\000\
  OR\000\
  NOT\000\
  OPEN\000\
  CLOSE\000\
  EOF\000\
  COMMA\000\
  PROOF\000\
  GENERAL\000\
  "

let yynames_block = "\
  VAR\000\
  EXPRESSION\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 24 "src/parser.mly"
                           ( _1 )
# 173 "src/parser.ml"
               : Tree.tree_t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 26 "src/parser.mly"
                           ( Var (_1) )
# 180 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Tree.tree_t) in
    Obj.repr(
# 27 "src/parser.mly"
                           ( _1 )
# 187 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 28 "src/parser.mly"
                           ( _2 )
# 194 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 29 "src/parser.mly"
                           ( Neg (_2) )
# 201 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 30 "src/parser.mly"
                           ( Binop (Impl, _1, _3) )
# 209 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "src/parser.mly"
                           ( Binop (Conj, _1, _3) )
# 217 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "src/parser.mly"
                           ( Binop (Disj, _1, _3) )
# 225 "src/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expressions) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 36 "src/parser.mly"
                               ( (_1, _3) )
# 233 "src/parser.ml"
               : Tree.tree_t list * Tree.tree_t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 37 "src/parser.mly"
                               ( ([], _2) )
# 240 "src/parser.ml"
               : Tree.tree_t list * Tree.tree_t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "src/parser.mly"
                          ( [_1] )
# 247 "src/parser.ml"
               : 'expressions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expressions) in
    Obj.repr(
# 41 "src/parser.mly"
                          ( _1::_3 )
# 255 "src/parser.ml"
               : 'expressions))
(* Entry body *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry header *)
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
let body (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tree.tree_t)
let header (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Tree.tree_t list * Tree.tree_t)