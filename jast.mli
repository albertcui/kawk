open Sast


type j_variable_decl = variable_decl * bool

type struct_decl = {
	sname: string; (* Name of the struct *)
	variable_decls: j_variable_decls list; (* int foo *)
	asserts: (expression * stmt list) list; (* @ (bar > 1) { ... } *)
}

type program = struct_decl list * variable_decl list * function_decl list