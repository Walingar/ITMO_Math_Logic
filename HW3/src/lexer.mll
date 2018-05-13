{
open Parser
}

let variable = ['A' - 'Z']+ ['A' - 'Z' '0' - '9']*
let white_space = [' ' '\t' '\r' '\n']

rule read = parse
	| white_space   { read lexbuf }
        | variable as v { VAR(v) }
        | "|-"          { PROOF }
        | "|="          { GENERAL }
        | ","           { COMMA }
        | "->"          { IMPL }
        | "&"           { AND }
        | "|"           { OR }
        | "!"           { NOT }
        | "("           { OPEN }
        | ")"           { CLOSE }
        | eof           { EOF }

and write a b c = parse
        | white_space   { write a b c lexbuf }
        | "A"           { EXPRESSION (a) }
        | "B"           { EXPRESSION (b) }
        | "C"           { EXPRESSION (c) }
        | "|-"          { PROOF }
        | "|="          { GENERAL }
        | ","           { COMMA }
        | "->"          { IMPL }
        | "&"           { AND }
        | "|"           { OR }
        | "!"           { NOT }
        | "("           { OPEN }
        | ")"           { CLOSE }
        | eof           { EOF }