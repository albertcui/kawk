open Ast

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

type expression = expr_detail * var_types

type stmt = 
	Block of stmt list (* { ... } *)
	| Expr of expression (* foo = bar + 3; *)
	| Return of expression (* return 42; *)
	| If of expression * stmt * stmt (* if (foo == 42) {} else {} *)
	| For of expression * expression * expression * stmt (* for (i=0;i<10;i=i+1) { ... } *)
	| While of expression * stmt

type variable_decl = var_decl * var_types

type func_decl = {
	ftype: var_types;
	fname : string; (* Name of the function *)
	formals : var_decl list; (* Formal argument names *)
	locals : var_decl list; (* Locally defined variables *)
	body : stmt list;
}

type program = struct_decl list * variable_decl list * func_decl list