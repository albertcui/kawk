%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ RETURN IF ELSE FOR WHILE INT EOF OR AND
%token ACCESS STRUCT ASSERT
%token <int> LITERAL
%token <string> ID
%token <string> STRING

%nonassoc NOELSE /* Precedence and associativity of each operator */
%nonassoc ELSE
%right ASSIGN
%left OR AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program /* Start symbol */
%type <Ast.program> program /* Type returned by a program */

%%

expr: LITERAL 					{ Literal($1) }
| ID							{ Id($1) }
| expr PLUS expr				{ Binop($1, Add, $3) }
| expr MINUS expr 				{ Binop($1, Sub, $3) }
| expr TIMES expr 				{ Binop($1, Mult, $3) }
| expr DIVIDE expr				{ Binop($1, Div, $3) }
| expr EQ						{ Binop($1, Equal, $3) }
| expr NEQ						{ Binop($1, Neq, $3) }
| expr LT 						{ Binop($1, Less, $3) }
| expr LEQ 						{ Binop($1, Leq, $3) }
| expr GT						{ Binop($1, Greater, $3) }
| expr GEQ						{ Binop($1, Geq, $3) }
| ID ASSIGN expr 				{ Assign($1, $3) }
| ID LPAREN actuals_opt RPAREN 	{ Call($1, $3) } 
| LPAREN expr RPAREN 			{ $2 }
