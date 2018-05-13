%{
  open Tree;;
%}
%token <string> VAR
%token <Tree.tree_t> EXPRESSION
%token IMPL AND OR NOT
%token OPEN CLOSE
%token EOF
%token COMMA PROOF GENERAL
%right IMPL
%left OR
%left AND
%left PROOF
%left GENERAL
%left COMMA
%nonassoc NOT
%start body
%start header
%type <Tree.tree_t> body
%type <Tree.tree_t list * Tree.tree_t> header
%%

body:
        expr EOF           { $1 }
expr:
        VAR                { Var ($1) }
        |EXPRESSION        { $1 }
        |OPEN expr CLOSE   { $2 }     
        |NOT expr          { Neg ($2) }  
        |expr IMPL expr    { Binop (Impl, $1, $3) }
        |expr AND expr     { Binop (Conj, $1, $3) }
        |expr OR expr      { Binop (Disj, $1, $3) }


header:
		expressions GENERAL expr EOF { ($1, $3) }
		|GENERAL expr EOF            { ([], $2) }

expressions:
		expr                    { [$1] }
		|expr COMMA expressions { $1::$3 }