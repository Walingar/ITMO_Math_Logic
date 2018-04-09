open Tree;;

let (>>) x f = f x;;

let parse_expression_to_tree expression = expression >> Lexing.from_string >> Parser.body Lexer.read;;