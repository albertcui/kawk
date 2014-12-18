open Sast


type j_var_struct_decl = {
	name: string;
	the_variable: variable_decl; (* int a *)
	mutable asserts: (expression * stmt list) list; (* @ (bar > 1) { ... } *)
}

type j_struct_decl = {
	sname: string; (* Name of the struct *)
	variable_decls: j_var_struct_decl list; (* list of asserts/shared variables *)
	original_struct: Sast.struct_decl;
	mutable j_name: string;
}

type program = j_struct_decl list * variable_decl list * function_decl list * unit_decl list