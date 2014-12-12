open Sast
open Lexing
open Map

let process_struct_decl s = 
let process_var_decl v =
let process_func_decl f = 


let sast_to_jast p = 
	let (structs, vars, funcs) = p in 	
		List.iter process_struct_decl structs;
		List.iter process_var_decl vars;
		List.iter process_func_decl (List.rev funcs)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = try
	Parser.program Scanner.token lexbuf 
	with _ -> Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1) in
	sast_to_jast program