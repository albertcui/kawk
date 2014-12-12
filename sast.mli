open Ast

(*
TODO

Add definition of struct to SAST
*)


type variable_decl = var_decl * var_types

type function_decl = {
	ftype: var_types;
	fname : string; (* Name of the function *)
	checked_formals : variable_decl list; (* Formal argument names *)
	checked_locals : variable_decl list; (* Locally defined variables *)
	checked_body : stmt list;
}
and struct_decl = {
	sname: string; (* Name of the struct *)
	variable_decls: variable_decl list; (* int foo *)
	asserts: (expression * stmt list) list; (* @ (bar > 1) { ... } *)
}
and expr_detail =
	Noexpr
	| This
	| Null
	| IntConst of int
	| StrConst of string
	| BoolConst of bool
	| ArrayAccess of var_decl * expression
	| Id of var_decl
	| Call of function_decl * expression list
	| Access of struct_decl * var_decl
	| Uniop of op * expression
	| Binop of expression * op * expression
	| Assign of var_decl * expression
and expression = expr_detail * var_types
and stmt = 
	Block of stmt list (* { ... } *)
	| Expr of expression (* foo = bar + 3; *)
	| Return of expression (* return 42; *)
	| If of expression * stmt * stmt (* if (foo == 42) {} else {} *)
	| For of expression * expression * expression * stmt (* for (i=0;i<10;i=i+1) { ... } *)
	| While of expression * stmt

type program = struct_decl list * variable_decl list * function_decl list