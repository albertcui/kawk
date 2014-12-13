open Sast
open Jast
open Semantic_checker
open Lexing
open Map

let find_decl (var_decl : Sast.checked_var_decl) (var_list : Jast.j_var_struct_decl list) =
	List.find (fun v -> let (v, _) = v.the_variable in v = var_decl) var_list

let rec check_assert_expr a (e : Sast.expression) (var_list : Jast.j_var_struct_decl list) =
	let (expr_detail, _) = e in 
	let Some(var) = match expr_detail with
		ArrayAccess(var, expr) ->  check_assert_expr a expr var_list; Some(var) 
		| Id(var) -> Some(var)
		| Call(_, expr_list) -> List.iter (fun e -> check_assert_expr a e var_list) expr_list; ()
		| Access(_, var) -> Some(var)
		| Uniop (_, expr) -> check_assert a expr var_list; ()
		| Binop (expr1, _, expr2) -> check_assert a expr1 var_list; check_assert a expr2 var_list; ()
		| Assign (var, expr) -> check_assert_expr a expr var_list; var
		| _ -> () in
	if var <> () then let j_var = find_decl var var_list in 
		(if List.find(fun a -> other_assert = a) then 
		j_var.asserts <- a :: j_var.asserts else ())
	else ()


let rec check_assert a (var_list : Jast.j_var_struct_decl list) =
	let (e, _) = a in
	let (expr_detail, _) = e in 
	let var = match expr_detail with
		ArrayAccess(var, expr) ->  check_assert_expr a expr var_list; var 
		| Id(var) -> var
		| Call(_, expr_list) -> List.iter (fun e -> check_assert_expr a expr var_list) expr_list; ()
		| Access(_, var) -> var
		| Uniop (_, expr) -> check_assert a expr var_list; ()
		| Binop (expr1, _, expr2) -> check_assert a expr1 var_list; check_assert a expr2 var_list; ()
		| Assign (var, expr) -> check_assert_expr a expr var_list; var
		| _ -> () in
	if var <> () then let j_var = find_decl var var_list in 
		(if List.find(fun a -> other_assert = a) then 
		j_var.asserts <- a :: j_var.asserts else ())
	else ()
	

(* iterate over s.variable_decls to make 
	corresponding j_var_struct_decls intially with empty asserts*)
let process_struct_decl (s : Sast.struct_decl) = 
	let j_var_decl = List.fold_left (fun a v -> {the_variable = v; asserts = []} :: a) [] s.variable_decls in
	List.iter (fun a -> check_assert a j_var_decl) s.asserts; j_var_decl

let sast_to_jast p = 
	let (structs, vars, funcs, units) = p in 	
	let structs = List.fold_left (fun a s -> process_struct_decl s) [] structs in
	(structs, vars, funcs, units)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = try
	Parser.program Scanner.token lexbuf 
	with _ -> Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1) in
	let sast = check_program ast in
	sast_to_jast sast 