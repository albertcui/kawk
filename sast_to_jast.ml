open Sast
open Jast
open Semantic_checker
open Lexing
(* open Map
 *)
let find_decl (var_decl : Sast.checked_var_decl) (var_list : Jast.j_var_struct_decl list) =
	List.find (fun v -> let (v, _) = v.the_variable in v = var_decl) var_list

let rec check_j_in_a (j : Jast.j_var_struct_decl) (e : Sast.expression) =
	let (the_variable, _) = j.the_variable in
	let (expr_detail, _) = e in match expr_detail with
		ArrayAccess(var, expr) -> if var == the_variable then true else check_j_in_a j expr
		| Id(var) -> if var == the_variable then true else false
		| Call(_, expr_list) -> let rec check_list = function
			| [] -> false
			| hd::tl -> if check_j_in_a j e then true else check_list tl in
			check_list expr_list
		| Access(_, var) -> if var == the_variable then true else false
		| Uniop (_, expr) -> check_j_in_a j expr
		| Binop (expr1, _, expr2) -> check_j_in_a j expr1 && check_j_in_a j expr2 
		| Assign (var, expr) -> if var == the_variable then true else check_j_in_a j expr
		| _ -> false

let rec check_assert_expr (var_decl : Jast.j_var_struct_decl) a (e : Sast.expression) (var_list : Jast.j_var_struct_decl list) =
	try
		let _ = List.find(fun other_assert -> other_assert = a) var_decl.asserts in []
	with Not_found -> if check_j_in_a var_decl e then [a] else []

(* iterate over s.variable_decls to make 
	corresponding j_var_struct_decls intially with empty asserts*)
let process_struct_decl (s : Sast.struct_decl) = 
	let j_var_decls = List.fold_left (
		fun a v -> let (decl, _) = v in
		let id = match decl with
			Variable(_, id) -> id
			| Variable_Initialization(_, id, _) -> id
			| Array_Initialization(_, id, _) -> id
			| Struct_Initialization(_, id, _) -> id in
		{the_variable = v; asserts = []; name = id} :: a
	) [] s.variable_decls in
	List.iter (
		fun (var_decl : Jast.j_var_struct_decl) ->
			let asserts = List.fold_left (
				fun a the_assert -> 
				let (e, _)  = the_assert in
				check_assert_expr var_decl the_assert e j_var_decls @ a
			) var_decl.asserts s.asserts in var_decl.asserts <- asserts
	) j_var_decls; { sname = s.sname; variable_decls = j_var_decls; original_struct = s; j_name = "" }

let sast_to_jast p = 
	let (structs, vars, funcs, units) = p in 	
	let structs = List.fold_left (fun a s -> process_struct_decl s :: a) [] structs in
	(structs, vars, funcs, units)
(* 
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = try
	Parser.program Scanner.token lexbuf 
	with _ -> Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1) in
	let sast = check_program ast in
	sast_to_jast sast  *)