open Ast

(* type var_types =
	Void
	| Int
	| String
	| Boolean
	| Struct of string
	| Array of var_types * expr

type var_decl =
	Variable of var_types * string
	| Variable_Initialization of var_types * string * expr
	| Array_Initialization of var_types * string * expr list
	| Struct_Initialization of var_types * string * expr list *)

type variable_decl = var_decl * var_types

type function_decl = {
	ftype: var_types;
	fname : string; (* Name of the function *)
	formals : variable_decl list; (* Formal argument names *)
	locals : variable_decl list; (* Locally defined variables *)
	body : stmt list;
}

type expr_detail =
	Noexpr
	| This
	| Null
	| IntConst of int
	| StrConst of string
	| BoolConst of bool
	| ArrayAccess of variable_decl * expression
	| Id of variable_decl
	| Call of function_decl * expression list
	| Access of struct_decl * string
	| Uniop of op * expression
	| Binop of expr_detail * op * expression
	| Assign of variable_decl * expression
and expression = expr_detail * var_types

type stmt = 
	Block of stmt list (* { ... } *)
	| Expr of expression (* foo = bar + 3; *)
	| Return of expression (* return 42; *)
	| If of expression * stmt * stmt (* if (foo == 42) {} else {} *)
	| For of expression * expression * expression * stmt (* for (i=0;i<10;i=i+1) { ... } *)
	| While of expression * stmt

type program = struct_decl list * variable_decl list * function_decl list