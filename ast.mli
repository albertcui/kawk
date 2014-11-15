type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | Or | And | Not

type expr = (* Expressions *)
	Noexpr (* for (;;) *)
	| This
	| Null
	| Id of string (* foo *)
	| Integer_literal of int (* 42 *)
	| String_literal of string (* "foo" *)
	| Boolean_literal of bool 
	| Array_access of string * expr (* foo[10] *)
	| Assign of string * expr (* foo = 42 *)
	| Uniop of op * expr
	| Binop of expr * op * expr (* a + b *)
	| Call of string * expr list (* foo(1, 25) *)
	| Access of string * string (* foo.bar *)

type stmt = (* Statements *)
	Block of stmt list (* { ... } *)
	| Expr of expr (* foo = bar + 3; *)
	| Return of expr (* return 42; *)
	| If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
	| For of expr * expr * expr * stmt (* for (i=0;i<10;i=i+1) { ... } *)
	| While of expr * stmt (* while (i<10) { i = i + 1 } *)
	
type var_types =
	Void
	| Int
	| String
	| Boolean
	| Struct of string
	| Array of var_types * expr

type var_decl =
	Variable of var_types * string
	| Variable_Initialization of var_types * string * expr
	| Array_Initialization of var_types * string * stmt
	| Struct_Initialization of var_types * string * expr list

type struct_body = 
	| S_Variable_Decl of var_decl (* int foo *)
	| Assert of expr * stmt list (* @ (bar > 1) { ... } *)

type struct_decl = {
	sname: string; (* Name of the struct *)
	sbody : struct_body list;
}

type func_decl = {
	ftype: var_types;
	fname : string; (* Name of the function *)
	formals : var_decl list; (* Formal argument names *)
	locals : var_decl list; (* Locally defined variables *)
	body : stmt list;
}

type program = struct_decl list * var_decl list * func_decl list (* global vars, funcs *)
