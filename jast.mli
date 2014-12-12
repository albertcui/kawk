open Sast




type j_var_struct_decl = {
	the_variable: variable_decl;
	asserts: (expression * stmt list) list; (* @ (bar > 1) { ... } *)
}

type struct_decl = {
	sname: string; (* Name of the struct *)
	variable_decls: j_var_struct_decl list; (* int foo *)
}


type program = struct_decl list * variable_decl list * function_decl list