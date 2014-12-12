open Ast

(*
TODO

Add definition of struct to SAST
*)


type checked_var_decl =
	Variable of var_types * string
	| Variable_Initialization of var_types * string * expression
	| Array_Initialization of var_types * string * expression list
	| Struct_Initialization of var_types * string * expression list
and variable_decl = checked_var_decl * var_types
and function_decl = {
	ftype: var_types;
	fname : string; (* Name of the function *)
	checked_formals : variable_decl list; (* Formal argument names *)
	checked_locals : variable_decl list; (* Locally defined variables *)
	checked_body : stmt list;
}
and unit_decl = 
	Local_udecl of expr list * expr * bool
	| Outer_udecl of string * expr list * expr * bool
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
	| ArrayAccess of checked_var_decl * expression
	| Id of checked_var_decl
	| Call of function_decl * expression list
	| Access of struct_decl * checked_var_decl
	| Uniop of op * expression
	| Binop of expression * op * expression
	| Assign of checked_var_decl * expression
and expression = expr_detail * var_types
and stmt = 
	Block of stmt list (* { ... } *)
	| Expr of expression (* foo = bar + 3; *)
	| Return of expression (* return 42; *)
	| If of expression * stmt * stmt (* if (foo == 42) {} else {} *)
	| For of expression * expression * expression * stmt (* for (i=0;i<10;i=i+1) { ... } *)
	| While of expression * stmt

type program = struct_decl list * variable_decl list * function_decl list * unit_decl list