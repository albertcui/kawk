type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | Or | And

type expr = (* Expressions *)
	Int_literal of int (* 42 *)
	| Noexpr (* for (;;) *)
	| Id of string (* foo *)
	| String_literal of string (* "foo bar" *)
	| Assign of string * expr (* foo = 42 *)
	| Binop of expr * op * expr (* a + b *)
	| Call of string * expr list (* foo(1, 25 *)
	| Access of id * id (* foo.bar *)

type stmt = (* Statements *)
	Block of stmt list (* { ... } *)
	| Expr of expr (* foo = bar + 3; *)
	| Return of expr (* return 42; *)
	| If of expr * stmt * stmt (* if (foo == 42) {} else {} *)
	| For of expr * expr * expr * stmt (* for (i=0;i<10;i=i+1) { ... } *)
	| While of expr * stmt (* while (i<10) { i = i + 1 } *)
	| Struct of id * stmt (* struct foo { ... }  *)
	| Assert of expr * expr * stmt(* foo @ bar > 1 { ... } *) !!!!

type func_decl = {
	fname : string; (* Name of the function *)
	formals : string list; (* Formal argument names *)
	locals : string list; (* Locally defined variables *)
	body : stmt list;
}

type var-types =
	Void
	| Int
	| String
	| Boolean
	| Struct of string * ((string * t) array)
	| Array of t * int 
	| Exception of string

type program = string list * func_decl list (* global vars, funcs *)
