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
	| Array_Initialization of var_types * string * expr list
	| Struct_Initialization of var_types * string * expr list

type struct_decl = {
	sname: string; (* Name of the struct *)
	variable_decls: var_decl list; (* int foo *)
	asserts: (expr * stmt list) list; (* @ (bar > 1) { ... } *)
}

type unit_decl =
	Unit_Initialization of expr list * expr;

type func_decl = {
	ftype: var_types;
	fname : string; (* Name of the function *)
	formals : var_decl list; (* Formal argument names *)
	locals : var_decl list; (* Locally defined variables *)
	body : stmt list;
	udecl_list : unit_decl list; (* Series of unit tests *)
}

type program = struct_decl list * var_decl list * func_decl list (* global vars, funcs *)
