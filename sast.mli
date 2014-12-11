open Ast

type expr_detail =
	Noexpr
	| This
	| Null
	| IntConst of int
	| StrConst of string
	| BoolConst of bool
	| ArrayAccess of variable_decl * int
	| Id of variable_decl
	| Call of function_decl * expr_detail list
	| Access of struct_decl * string
	| Binop of expr_detail * op * expr_detail

type expression = expr_detail * var_types

type stmt = 
	Block of stmt list (* { ... } *)
	| Expr of expr_detail (* foo = bar + 3; *)
	| Return of expr_detail (* return 42; *)
	| If of expr_detail * stmt * stmt (* if (foo == 42) {} else {} *)
	| For of expr_detail * expr_detail * expr_detail * stmt (* for (i=0;i<10;i=i+1) { ... } *)
	| While of expr_detail * stmt
