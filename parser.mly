%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA PLUS MINUS TIMES DIVIDE
%token ASSIGN EQ NEQ LT LEQ GT GEQ RETURN IF ELSE FOR WHILE BOOL STRING INT EOF OR AND 
%token ACCESS STRUCT ASSERT THIS
%token <string> ID
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <bool> BOOL_LITERAL

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

program:
	/* nothing */ 	{ [], [] }
	| program vdecl { ($2 :: fst $1), snd $1 } /* int world = 4; */
	| program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
	ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
	{ { fname   = $1;
		formals = $3;
		locals  = List.rev $6;
		body    = List.rev $7 } }

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
	var_types ID SEMI { ($1, $2) }

stmt_list:
	/* nothing */		{ [] }
	| stmt_list stmt 	{ $2 :: $1 }

stmt:
	expr SEMI
	| RETURN expr SEMI
	| LBRACE stmt_list RBRACE
	| IF LPAREN expr RPAREN stmt %prec NOELSE 						{ If($3, $5, Block([])) } 
	| IF LPAREN expr RPAREN stmt ELSE stmt 							{ If($3, $5, $7) }
	| FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt 	{ For($3, $5, $7, $9) } 
	| WHILE LPAREN expr RPAREN stmt 								{ While($3, $5) }

expr_opt:
	/* nothing */	{ Noexpr }
	| expr 			{ $1 }


expr: 
	ID								{ Id($1) }
	| INT_LITERAL 					{ Int_literal($1) }
	| STRING_LITERAL				{ String_literal($1) }
	| BOOL_LITERAL					{ Bool_literal($1) } 
	| THIS 							{ This ($1) } 
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
	| expr OR expr					{ Binop ($1, Or, $3) }
	| expr AND expr					{ Binop ($1, And, $3) }
	| expr ACCESS expr				{ Access ($1, $3) }
	| expr ASSERT expr 				{ Assert ($1, $3) }
	| ID ASSIGN expr 				{ Assign ($1, $3) }
	| ID LPAREN actuals_opt RPAREN 	{ Call ($1, $3) } 
	| LPAREN expr RPAREN 			{ $2 }

actuals_opt:
	/* nothing */ 	{ [] }
	| actuals_list 	{ List.rev $1 }

actuals_list:
	expr 						{ [$1] }
	| actuals_list COMMA expr 	{ $3 :: $1 }