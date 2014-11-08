open Ast

let print_expr = function
	Noexpr -> print_string ""
	| Id(id) -> Printf.printf " %s" id
	| Integer_literal(i) -> Printf.printf " %d" i 
	| String_literal(str) -> Printf.printf " %S" str
	| Boolean_literal(b) -> Printf.printf " %B" b
	| Array_access(str, i) -> Printf.printf " %s [%d]" str i 

(*
let print_var_decl = 
	Variable -> print_var_types fst; print_string " " ^ snd
	| Variable_Initiation(type, name, expr) -> print_var_types type; print_string " " ^ name; print_expr expr;


let print ((structs, vars, funcs) : program) : unit =
	let rec print_structs : (struct_decl) =
		| [] -> []
		| hd::tl -> print_string "struct " ^ hd.sname ^ "{\n";
			let rec print_s_body (sb) : unit =
				List.iter (fun line -> 
					S_Variable_Decl ->   
					| Assert -> ) sb;
				print_structs tl;
*)