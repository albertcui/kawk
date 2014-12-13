open Sast
open Semantic_checker
open Lexing
open Map

let process_struct_decl s = 
let process_var_decl v =
let process_func_decl f = 
let process_unit_decl u =

let sast_to_jast p = 
	let (structs, vars, funcs, units) = p in 	
		List.fold process_struct_decl structs;
		List.fold process_var_decl vars;
		List.fold process_func_decl (List.rev funcs)
		List.fold process_unit_decl;
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = try
	Parser.program Scanner.token lexbuf 
	with _ -> Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1) in
	let sast = check_program ast in
	sast_to_jast sast 