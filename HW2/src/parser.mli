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

val body :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.tree_t
val header :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.tree_t list * Tree.tree_t
