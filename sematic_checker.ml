open Ast
open Lexing
open Map

type function_table = {
	funcs : func_decl list
}

type translation_environment = {
	scope : symbol_table; (* symbol table for vars *)
	return_type : var_types (* Function’s return type *)
}

type symbol_table = {
	parent : symbol_table option;
	variables : string * var_types list;
	functions : func_decl list
}

(* 
let rec find_variable (scope : symbol_table) name =
	try
		List.find (fun (s, _, _, _) -> s = name) scope.variables
	with Not_found ->
		match scope.parent with
		Some(parent) -> find_variable parent name
		| _ -> raise Not_found

let rec find_func (funcs : function_table) name = 
	try
		List.find  *)
(*
let rec expr env = 
	(* An integer constant: convert and return Int type *)
	Ast.IntConst(v) -> Sast.IntConst(v), Types.Ints
	(* An identifier: verify it is in scope and return its type *)
	| Ast.Id(vname) ->
		let vdecl = try
		find_variable env.scope vname (* locate a variable by name *)
		with Not_found ->
		raise (Error("undeclared identifier " ^ vname))
		in
		let (_, typ) = vdecl in (* get the variable’s type *)
		Sast.Id(vdecl), typ
	| Ast.Binop(e1, op, e2) ->
		let e1 = expr env e1 and e2 = expr env e2 in (* Check left and right children *)
		let _, t1 = e1 (* Get the type of each child *)
		and _, t2 = e2 in
		if op <> Ast.Equal && op <> Ast.NotEqual then
		(* Most operators require both left and right to be integer *)
		(require_integer e1 "Left operand must be integer";
		require_integer e2 "Right operand must be integer")
		else
		if not (weak_eq_type t1 t2) then
		(* Equality operators just require types to be "close" *)
		error ("Type mismatch in comparison: left is " ^
		Printer.string_of_sast_type t1 ^ "\" right is \"" ^
		Printer.string_of_sast_type t2 ^ "\""
		) loc;
		Sast.BinOp(e1, op, e2), Types.Int (* Success: result is int *)

let rec stmt env = 
	(* Expression statement: just check the expression *)
	Ast.Expression(e) -> Sast.Expression(expr env e)
	(* If statement: verify the predicate is integer *)
	| Ast.If(e, s1, s2) ->
		let e = check_expr env e in (* Check the predicate *)
		require_integer e "Predicate of if must be integer";
		Sast.If(e, stmt env s1, stmt env s2) (* Check then, else *)
	(* let rec stmt env = function *)
	| Ast.Local(vdecl) ->
		let decl, (init, _) = check_local vdecl (* already declared? *)
		in
		(* side-effect: add variable to the environment *)
		env.scope.variables <- decl :: env.scope.variables;
		init (* initialization statements, if any *)
	(* let rec stmt env = function *)
	| Ast.Block(sl) ->
		(* New scopes: parent is the existing scope, start out empty *)
		let scope' = { parent = Some(env.scope); variables = [] } in
		(* New environment: same, but with new symbol tables *)
		let env' = { env with scope = scope' } in
		(* Check all the statements in the block *)
		let sl = List.map (fun s -> stmt env' s) sl in
		scope'.variables <-
		List.rev scope'.variables; (* side-effect *)
		Sast.Block(scope', sl) (* Success: return block with symbols *)
*)

let print_op = function
	Add -> print_string "+ "
	| Sub -> print_string "- "
	| Mult -> print_string "* "
	| Div -> print_string "/ "
	| Mod -> print_string "% "
	| Equal -> print_string "= "
	| Neq -> print_string "!= "
	| Less -> print_string "< " 
	| Leq -> print_string "<= "
	| Greater -> print_string "> "
	| Geq -> print_string ">= "
	| Or -> print_string "| "
	| And -> print_string "& "
	| Not -> print_string "! " 

let rec print_expr = function
	Noexpr -> print_string ""
	| This -> print_string "this "
	| Null -> print_string "null "
	| Id(id) -> Printf.printf "%s " id
	| Integer_literal(i) -> Printf.printf "%d " i 
	| String_literal(str) -> Printf.printf "%S " str
	| Boolean_literal(b) -> Printf.printf "%B " b
	| Array_access(str, expr) -> Printf.printf "%s[" str; print_expr expr; print_string "]"
	| Assign(str, expr) -> Printf.printf "%s = " str; print_expr expr
	| Uniop(op, expr) -> print_op op; print_expr expr
	| Binop(expr1, op, expr2) -> print_expr expr1; print_op op; print_expr expr2
	| Call(str, expr_list) -> Printf.printf "%s(" str; List.iter print_expr expr_list; print_string ") "
	| Access(str1, str2) -> Printf.printf "%s.%s " str1 str2 

let print_expr_semi e = 
	print_expr e; print_string ";\n"

let rec print_expr_list = function
	[] -> print_string ""
	| hd::[] -> print_expr hd
	| hd::tl -> print_expr hd; print_string "; "; print_expr_list tl 


let rec print_stmt = function
	Block(stmt_list) -> print_string "{"; List.iter print_stmt stmt_list; print_string "}\n"
	| Expr(expr) -> print_expr_semi expr
	| Return(expr) -> print_string "return "; print_expr_semi expr
	| If(expr, stmt1, stmt2) -> print_string "if ("; print_expr_semi expr; print_string ")"; print_stmt stmt1; print_stmt stmt2
	| For(expr1, expr2, expr3, stmt) -> print_string "for ("; print_expr_semi expr1; print_string ";"; print_expr_semi expr2; print_string ";"; print_expr expr3; print_stmt stmt 
	| While(expr, stmt) -> print_string "while ("; print_expr_semi expr; print_string ")"; print_stmt stmt

let rec print_var_types = function
	Void -> print_string "void "
	| Int -> print_string "int "
	| String -> print_string "str " 
	| Boolean -> print_string "bool "
	| Struct(str) -> Printf.printf "struct %s " str 
	| Array(var_types, expr) -> print_var_types var_types; print_string "["; print_expr_semi expr; print_string "] "

let rec print_var_decl = function
	Variable(var_types, str) -> print_var_types var_types; print_string (str ^ ";\n")
	| Variable_Initialization(var_types, str, expr) -> print_var_types var_types; Printf.printf "%s = " str; print_expr_semi expr
	| Array_Initialization(var_types, str, expr_list) -> print_var_types var_types; Printf.printf "%s[] = { " str; print_expr_list expr_list; print_string "};\n"
	| Struct_Initialization(var_types, str, expr_list) -> print_var_types var_types; Printf.printf "%s = { " str; List.iter print_expr expr_list; print_string "};\n"


let print_struct_body = function
	S_Variable_Decl(var_decl) -> print_var_decl var_decl
	| Assert(expr, stmt_list) -> print_string "@("; print_expr expr; print_string ") "; List.iter print_stmt stmt_list

let print_struct_decl s =
	print_string "struct ";
	print_string s.sname; 
	print_string " {\n";
	List.iter print_struct_body s.sbody;
	print_string "}"

let find_func (l : func_decl list) f =
	List.find(fun c -> c.fname = f.fname) l

let proc_func_decl (env : translation_environment) f =
	try
		let _ = find_func env.scope.functions f in
			raise Failure ("Function already declared with name " ^ f.fname)
	with Not_found ->
		let scope' = { env.scope with parent = Some(env.scope); variables = f.locals::f.formals } in
		let scope' = List.fold_left check_statement scope' f.body in
		let scope' = { env.scope with functions = env.scope.functions :: f } in
		{ env with scope = scope' }

let rec check_stmt (scope : symbol_table) stmt = match stmt with
	Block(sl) -> List.fold_left check_statement scope sl
	| Expr(e) -> check_expr scope e
	| Return(e) -> check_expr scope e
	| If(expr, stmt1, stmt2) -> 
		let scope' = check_expr scope expr in
		let scope' = check_stmt scope' stmt1 in
		check_stmt scope' stmt2
	| For(expr1, expr2, expr3, stmt) ->
		let scope' = check_expr scope expr1 in
		let scope' = check_expr scope' expr2 in
		let scope' = check_expr scope' expr3 in
		check_stmt scope' stmt 
	| While(expr, stmt) ->
		let scope' = check_expr scope expr in
		check_stmt scope' stmt

let rec check_expr (scope : symbol_table) expr = match expr with
	Noexpr
	| This -> void
	| Null -> void
	| Id(str) -> check_id scope str
	| Integer_literal(i) -> Int
	| String_literal(str) -> String
	| Boolean_literal(b) -> Boolean
	| Array_access(str, expr) -> 
		let scope' = check_id scope str in
		check_expr scope' expr
	| Assign(str, expr) ->
		let scope' = check_id scope str in
		check_expr scope' expr
	| Uniop(op, expr) ->
		let scope' = check_op scope op in 
		check_expr scope' expr
	| Binop(expr1, op, expr2) ->
		let scope' = check_expr scope expr1 in
		let scope' = check_op scope' op in
		check_expr scope' expr2
	| Call(str, el) ->
		let scope' = check_id scope str in
		List.fold_left check_expr scope' el
	| Access(str1, str2) ->
		let scope' = check_id scope str1 in
		check_id scope' str2

let rec check_id (scope : symbol_table) id =
	try
		List.find(fun (name, _ ) -> name = id) scope.variables
	with Not_found -> match scope.parent with
		Some(parent) -> check_id scope.parent id
		| _ -> raise Not_found 


let process_func_formals (env : translation_environment) f =
	let scope' = { env.scope with parent = Some(env.scope); variables = [] } in
	let scope' = List.iter (fun var -> scope.variables:: head)

let print_program p = 
	let (structs, vars, funcs) = p in 	
		List.iter print_struct_decl structs;
		List.iter print_var_decl vars;
		List.iter print_func_decl (List.rev funcs)

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = try
	Parser.program Scanner.token lexbuf 
	with _ -> Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1) in
	print_program program
