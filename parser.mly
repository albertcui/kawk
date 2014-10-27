%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA PLUS MINUS TIMES DIVIDE MOD
%token ASSIGN EQ NEQ LT LEQ GT GEQ RETURN IF ELSE FOR WHILE BOOL STRING INT EOF OR AND NOT
%token ACCESS STRUCT ASSERT THIS NULL
%token <string> ID
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL

%nonassoc ID
%nonassoc NOELSE /* Precedence and associativity of each operator */
%nonassoc ELSE
%nonassoc LBRACE RBRACE
%nonassoc LBRACK RBRACK
%nonassoc LPAREN RPAREN
%left ASSERT
%left ACCESS
%right ASSIGN
%left OR AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT
%nonassoc STRUCT

%start program /* Start symbol */
%type <Ast.program> program /* Type returned by a program */

%%

program:
	/* nothing */ 	{ [], [], [] }
	| program vdecl { let (str, var, func) = $1 in str, var, $2::func } /* int world = 4; */
	| program fdecl { let (str, var, func) = $1 in str, $2::var, func }
	| program sdecl { let (str, var, func) = $1 in $2::str, var, func }

fdecl:
	the_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
	{ { fname   = $2;
		formals = $4;
		locals  = List.rev $7;
		body    = List.rev $8 } }

formals_opt:
	/* nothing */		{ [] }
	| formal_list		{ List.rev $1 }

formal_list: 
	ID 						{ [$1] }
	| formal_list COMMA ID 	{ $3 :: $1 }

vdecl_list:
	/* nothing */		{ [] }
	| vdecl_list vdecl 	{ $2 :: $1 }

vdecl:
	the_type ID SEMI { Variable($1, $2) }
	| the_type ID expr SEMI { Variable_Initiation($1, $2, $3) }

sdecl:
	STRUCT ID LBRACK struct_body RBRACK
	{ { sname = $2;
		sbody = List.rev $4 } }

struct_body:
	/* nothing */ { [] }
	| struct_body vdecl { S_Varialbe_Decl($2) :: $1 }
	| struct_body ASSERT LPAREN expr RPAREN stmt { Assert($4, $6) :: $1 }

the_type:
	INT { Int }
	| STRING { String }
	| BOOL { Boolean }
	| STRUCT ID { Struct($2) }
	| the_type LBRACK expr RBRACK { Array($1, $3) }

stmt_list:
	/* nothing */		{ [] }
	| stmt_list stmt 	{ $2 :: $1 }
	| stmt_list init 	{ $2 :: $1 }

init:
	STRUCT ID ID ASSIGN block SEMI { Struct_initialization($2, $3, $5) }
	| ID LBRACK RBRACK ASSIGN block SEMI { Array_initialiation($1, $5) }

stmt:
	expr SEMI														{ Expr($1) }
	| RETURN expr SEMI												{ Return($2) }							
	| block															{ $1 }
	| IF LPAREN expr RPAREN stmt %prec NOELSE 						{ If($3, $5, Block([])) } 
	| IF LPAREN expr RPAREN stmt ELSE stmt 							{ If($3, $5, $7) }
	| FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt 	{ For($3, $5, $7, $9) } 
	| WHILE LPAREN expr RPAREN stmt 								{ While($3, $5) }

block:
	LBRACE stmt_list RBRACE { Block(List.rev $2) }

expr_opt:
	/* nothing */	{ Noexpr }
	| expr 			{ $1 }


expr: 
	ID								{ Id($1) }
	| INT_LITERAL 					{ Int_literal($1) }
	| STRING_LITERAL				{ String_literal($1) }
	| BOOL_LITERAL					{ Bool_literal($1) } 
	| THIS 							{ This }
	| NULL							{ Null }
	| NOT expr  					{ Uniop(Not, $2) }
	| expr PLUS expr				{ Binop($1, Add, $3) }
	| expr MINUS expr 				{ Binop($1, Sub, $3) }
	| expr TIMES expr 				{ Binop($1, Mult, $3) }
	| expr DIVIDE expr				{ Binop($1, Div, $3) }
	| expr MOD expr 				{ Binop($1, Mod, $3) }
	| expr EQ expr					{ Binop($1, Equal, $3) }
	| expr NEQ expr					{ Binop($1, Neq, $3) }
	| expr LT expr					{ Binop($1, Less, $3) }
	| expr LEQ expr					{ Binop($1, Leq, $3) }
	| expr GT expr					{ Binop($1, Greater, $3) }
	| expr GEQ expr					{ Binop($1, Geq, $3) }
	| expr OR expr					{ Binop ($1, Or, $3) }
	| expr AND expr					{ Binop ($1, And, $3) }
	| expr ACCESS expr				{ Access ($1, $3) }
	| expr ASSERT expr 				{ Assert ($1, $3) }
	| ID ASSIGN expr 				{ Assign ($1, $3) }
	| ID LPAREN actuals_opt RPAREN 	{ Call ($1, $3) }
	| LPAREN expr RPAREN 			{ $2 }
	| ID LBRACK expr RBRACK         { Array_access($1, $3) }

actuals_opt:
	/* nothing */ 	{ [] }
	| actuals_list 	{ List.rev $1 }

actuals_list:
	expr 						{ [$1] }
	| actuals_list COMMA expr 	{ $3 :: $1 }