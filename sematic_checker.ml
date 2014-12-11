open Sast
open Lexing
open Map

type function_table = {
	funcs : func_decl list
}

type translation_environment = {
	scope : symbol_table (* symbol table for vars *)
	(* return_type : var_types Function’s return type *)
}

type symbol_table = {
	parent : symbol_table option;
	variables : string * variable_decl * var_types list;
	functions : func_decl list;
	structs : struct_decl list
}

let find_struct (s : struct_decl list) stru =
	List.find(fun c -> c.sname = stru.sname) s


let find_func (l : func_decl list) f =
	List.find(fun c -> c.fname = f.fname) l
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
	| Integer_literal(i) -> Sast.IntConst(i), Int
	| String_literal(str) -> Sast.StrConst(str), String
	| Boolean_literal(b) -> Sast.BoolConst(b), Boolean
	| Array_access as a ->
		let t = check_array_access scope a in
		Sast.ArrayAccess(a, )
	| Assign as a ->
		check_assign scope a
	| Uniop(op, expr) ->
		check_uni_op scope op
	| Binop as b ->
		check_op scope b
	| Call as c ->
		check_call scope c
	| Access as a ->
		check_access scope a

let rec check_id (scope : symbol_table) id =
	try
		let (_, t) = List.find(fun (name, _ ) -> name = id) scope.variables in t
	with Not_found -> match scope.parent with
		Some(parent) -> check_id scope.parent id
		| _ -> raise Not_found
 
let check_op (scope : symbol_table) binop = 
	let (xp1, op, xp1) = binop in
	let e1 = check_expr scope xp1 and e2 = chekc_expr scope xp2 in
	match op with
	Add ->
		if (e1 <> Int || e2 <> Int) then
			if (e1 <> String || e2 <> String) then raise Failure "Incorrect types for + "
			else String
		else Int
	| Sub -> if (e1 <> Int || e2 <> Int) then raise Failure "Incorrect types for - " else Int
	| Mult -> if (e1 <> Int || e2 <> Int) then raise Failure "Incorrect types for * " else Int
	| Div -> if (e1 <> Int || e2 <> Int) then raise Failure "Incorrect types for / " else Int
	| Mod -> if (e1 <> Int || e2 <> Int) then raise Failure "Incorrect types for % " else Int
	| Equal -> if (e1 <> e2) then raise Failure "Incorrect types for = " else Boolean
	| Neq -> if (e1 <> e2) then raise Failure "Incorrect types for != " else Boolean
	| Less -> if (e1 <> Int || e2 <> Int) then raise Failure "Incorrect types for < " else Int
	| Leq -> if (e1 <> Int || e2 <> Int) then raise Failure "Incorrect types for <= " else Int
	| Greater -> if (e1 <> Int || e2 <> Int) then raise Failure "Incorrect types for > " else Int
	| Geq -> if (e1 <> Int || e2 <> Int) then raise Failure "Incorrect types for >= " else Int
	| Or -> if (e1 <> Boolean || e2 <> Boolean) then raise Failure "Incorrect types for | " else Boolean
	| And -> if (e1 <> Boolean || e2 <> Boolean) then raise Failure "Incorrect types for & " else Boolean
	| Not -> raise Failure "! is a unary operator."

let check_array_access (scope : symbol_table) a =
	let (id, expr) = a in
	let e1 = check_expr scope expr in
	let t = check_id scope id in
	if e1 <> Int then raise Failure "Array access must be integer." else t

let check_assign (scope : symbol_table) a =
	let (id, expr) = a in
	let e1 = check_expr scope expr in
	let t = check_id scope id in
	if e1 <> t then raise Failure "Incorrect type assignment." else t

let check_call (scope : symbol_table) c =
	let (id, el) = c in
	try
		let f = find_func env.scope.functions id in
			List.iter2 (
				fun a b -> match a with
				(t, _ ) -> if t <> check_expr b then raise Failure "wrong type" else t
				| (t, _, _)  -> if t <> check_expr b then raise Failure "wrong type" else t
			) f.formals el; f.ftype 	
	with Not_found -> raise Failure ("Function already declared with name " ^ id)
	
let check_access (scope : symbol_table) a =
	let (id, id2) = a in 
	let t = check_id scope id in
	match t with
		Struct(id) -> 
			try
				let s = find_struct env.scope.structs id in
				List.find (fun )

		| _ -> raise Failure id ^ " is not a struct." 
	
let check_uni_op (scope : symbol_table) uniop =
	let (op, exp) = uniop in
	let e = check_expr scope e in
	match op with
	Not -> if (e <> Boolean) then raise Failure "Incorrect types for ! " else Boolean
	| _ -> raise Failure (e ^ " is not a unary operator")

(* 
let process_func_formals (env : translation_environment) f =
	let scope' = { env.scope with parent = Some(env.scope); variables = [] } in
	let scope' = List.iter (fun var -> scope.variables:: head) *)

let process_var_decl (scope : symbol_table) v =
	let triple = match v with
		Variable(t, name) -> (name, v, t)
		| Variable_Initialization(t, name, expr) -> if t <> check_expr expr then raise Failure "wrong type" else (name, v, t) 
		| Array_Initialization(t, name, el) ->
			try 
				_ = List.find( fun elem -> t <> check_expr elem) el in raise Failure "wrong type"
			with Not_found ->
				(name, v, t) (* FLAG TO SEE IF ARRAY PLEASE *)
		| Struct_Initialization(t, name, el) ->
			Struct(id) -> 
				try
					let s = find_struct scope id in
					try
						_ = List.iter2 (
							fun a b -> match a with
							(t, _ ) -> if t <> check_expr b then raise Failure "wrong type" else t
							| (t, _, _)  -> if t <> check_expr b then raise Failure "wrong type" else t
						) s.variable_decls el in
						(name, v, t)
					with _ -> raise Failure "Not enough arguments or wrong type."
				with Not_found -> raise Failure ("struct " ^ id ^ " not found")
			| _ -> raise Failure "Not a struct"
	in scope.variables <- scope.variables :: triple; Sast.variable_decl(triple)

let process_func_decl (env : translation_environment) f =
	try
		let _ = find_func env.scope.functions f in
			raise Failure ("Function already declared with name " ^ f.fname)
	with Not_found ->
		let scope' = { env.scope with parent = Some(env.scope); variables = [] } in
		let scope' = List.fold_left process_var_decl scope' f.locals::f.formals in
		let scope' = List.fold_left check_statement scope' f.body in
		let scope' = { env.scope with functions = env.scope.functions :: f } in
		{ env with scope = scope' }

let process_assert (scope: symbol_table) a =
	let (expr, stml) = a in
	if check_expr expr <> Boolean then raise Failure "assert expr must be boolean " else
	List.fold_left check_stmt stml

let check_struct (scope : symbol_table) s =
	let scope' = List.fold_left process_var_decl scope s.variable_decls in
	List.fold_left process_assert scope' s.asserts

let process_struct_decl (env : translation_environment) s =
	try
		let _ = find_struct env.scope.structs s in
			raise Failure ("struct already declared with name " ^ s.sname)
	with Not_found ->
		List.fold_left check_struct scope s in (* Throw away scope of the struct *)
		let scope' = { env.scope with structs = env.scope.structs :: s } in
		{ env with scope = scope' }

let process_global_decl (env : translation_environment) g =
	try
		let _ = check_id env.scope.variables g in
			let name = match g with
				Variable(_, id) -> id
				| Variable_Initialization(_, id, _) -> id
				| Array_Initialization(_, id, _) -> id
				| Struct_Initialization(_, id, _) -> id
			in raise Failure ("Variable already declared with name " ^ name)
	with Not_found -> process_var_decl env.scope g
(* 		let scope' = { env.scope with parent = Some(env.scope); variables = env.scope.variables::g } in
		let scope' = { env.scope with functions = env.scope.functions :: f } in
		{ env with scope = scope' }
 *)

let check_program p =
	let s = { parent = None; variables = []; functions = []; structs = [] } in
	let env = { scope = s } in
	let (structs, vars, funcs) = p in 	
	let structs = 
		List.fold_left (
			fun a global -> a :: process_struct_decl env structs
		) [] structs in
	let globals =
		List.fold_left (
			fun a global -> a :: process_global_decl env global
		) [] vars in
	let List.fold_left (
			fun a global -> a :: process_func_decl env global
		) [] funcs in
	try
		List.find( fun (_, f, _, _) -> f == "main" ) env.scope.functions
	with Not_found -> raise Failure "No main function defined." in
	structs, globals, funcs

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = try
	Parser.program Scanner.token lexbuf 
	with _ -> Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1) in
	check_program program
