open Ast
open Sast
open Lexing
open Map

type function_table = {
	funcs : func_decl list
}

type symbol_table = {
	parent : symbol_table option;
	variables : (string * var_decl * var_types) list;
	functions : function_decl list;
	structs : struct_decl list;
}

type translation_environment = {
	scope : symbol_table (* symbol table for vars *)
	(* return_type : var_types Function’s return type *)
}

let find_struct (s : struct_decl list) stru =
	List.find(fun c -> c.sname = stru) s

let find_func (l : function_decl list) f =
	List.find(fun c -> c.fname = f) l

let rec check_id (scope : symbol_table) id =
	try
		let (_, decl, t) = List.find(fun (n, _, _) -> n = id ) scope.variables in
		decl, t
	with Not_found -> match scope.parent with
		Some(parent) -> check_id parent id
		| _ -> raise Not_found
 
 let rec check_expr (scope : symbol_table) (expr : Ast.expr) = match expr with
	Noexpr -> Sast.Noexpr, Void
	| This -> Sast.This, Void
	| Null -> Sast.Null, Void
	| Id(str) -> let (decl, t) = check_id scope str in Sast.Id(decl), t 
	| Integer_literal(i) -> Sast.IntConst(i), Int
	| String_literal(str) -> Sast.StrConst(str), String
	| Boolean_literal(b) -> Sast.BoolConst(b), Boolean
	| Array_access(_, _) as a -> check_array_access scope a
	| Assign(_, _) as a -> check_assign scope a
	| Uniop(op, expr) as u -> check_uni_op scope u
	| Binop(_, _, _) as b -> check_op scope b
	| Call(_, _) as c -> check_call scope c
	| Access(_, _) as a -> check_access scope a

and check_op (scope : symbol_table) binop = match binop with
	Ast.Binop(xp1, op, xp2) ->
		let e1 = check_expr scope xp1 and e2 = check_expr scope xp2 in
		let (_, t1) = e1 and (_, t2) = e2 in
		let t = match op with
			Add ->
				if (t1 <> Int || t2 <> Int) then
					if (t1 <> String || t2 <> String) then raise (Failure "Incorrect types for +")
					else String
				else Int
			| Sub -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for - ") else Int
			| Mult -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for * ") else Int
			| Div -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for / ") else Int
			| Mod -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for % ") else Int
			| Equal -> if (t1 <> t2) then raise (Failure "Incorrect types for = ") else Boolean
			| Neq -> if (t1 <> t2) then raise (Failure "Incorrect types for != ") else Boolean
			| Less -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for < ") else Int
			| Leq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for <= ") else Int
			| Greater -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for > ") else Int
			| Geq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for >= ") else Int
			| Or -> if (t1 <> Boolean || t2 <> Boolean) then raise (Failure "Incorrect types for | ") else Boolean
			| And -> if (t1 <> Boolean || t2 <> Boolean) then raise (Failure "Incorrect types for & ") else Boolean
			| Not -> raise (Failure "! is a unary operator.")
		in Sast.Binop(e1, op, e2), t
	| _ -> raise (Failure "Not an op")

and check_array_access (scope : symbol_table) a = match a with
	Ast.Array_access(id, expr) ->
		let (decl, t) = check_id scope id in
		let e1 = check_expr scope expr in
		let (_, t2) = e1 in
		if t2 <> Int then raise (Failure "Array access must be integer.") else
		Sast.ArrayAccess(decl, e1), t
	| _ -> raise (Failure "Not an array access")

and check_assign (scope : symbol_table) a = match a with
	Ast.Assign(id, expr) ->
		let (decl, t) = check_id scope id in
		let e = check_expr scope expr in
		let (_, t2) = e in
		if t <> t2 then raise (Failure "Incorrect type assignment.") else Sast.Assign(decl, e), t
	| _ -> raise (Failure "Not an assignment")

and check_call (scope : symbol_table) c = match c with
	Ast.Call(id, el) ->
		(try
			let f = find_func scope.functions id in
			let exprs = List.fold_left2 (
					fun a b c -> 
						let (_, t) = b in
						let expr = check_expr scope c in
						let (_, t2) = expr in
						if t <> t2
						then raise (Failure "wrong type")
						else expr :: a
				) [] f.formals el in
			Sast.Call(f, exprs), f.ftype
		with Not_found -> raise (Failure ("Function already declared with name " ^ id)))
	| _ -> raise (Failure "Not a call")	

and check_access (scope : symbol_table) a = match a with
	Ast.Access(id, id2) ->
		(let (_, t) = check_id scope id in match t with
			Struct(id) -> 
				(try
					let s = find_struct scope.structs id in
					let var = List.find (
						fun v -> match v with
						Variable(_, n) -> n = id
						| Variable_Initialization(_, n, _) -> n = id
						| Array_Initialization(_, n, _) -> n = id
						| Struct_Initialization(_, n, _) -> n = id
					) s.variable_decls in 
					let t = match var with
						Variable(t, _) -> t
						| Variable_Initialization(t, _, _) -> t
						| Array_Initialization(t, _, _) -> t
						| Struct_Initialization(t, _, _) -> t
					in Sast.Access(s, var), t
				with Not_found -> raise (Failure "Struct or access not found."))
			| _ -> raise (Failure (id ^ " is not a struct."))
		)
	| _ -> raise (Failure "Not an access")	

and check_uni_op (scope : symbol_table) uniop = match uniop with
	Ast.Uniop(op, expr) -> match op with
		Not ->
			let e = check_expr scope expr in
			let (_, t) = e in 
			if (t <> Boolean) then raise Failure "Incorrect type for ! " else Sast.Uniop(op, e), Boolean
	| _ -> raise Failure (e ^ " is not a unary operator")

(* 
let process_func_formals (env : translation_environment) f =
	let scope' = { env.scope with parent = Some(env.scope); variables = [] } in
	let scope' = List.iter (fun var -> scope.variables:: head) *)

let rec check_stmt (scope : symbol_table) stmt = match stmt with
	Block(sl) -> List.fold_left ( fun a s -> a :: check_statement scope s ) [] sl
	| Expr(e) -> check_expr scope e
	| Return(e) -> check_expr scope e
	| If(expr, stmt1, stmt2) -> 
		let expr = check_expr scope expr in
		let stmt1 = check_stmt scope stmt1 in
		let stmt2 = check_stmt scope stmt2 in
		Sast.If(expr, stmt1, stmt2)
	| For(expr1, expr2, expr3, stmt) ->
		let expr = check_expr scope expr in
		let expr2 = check_expr scope expr2 in
		let expr3 = check_expr scope expr3 in
		let stmt = check_stmt scope stmt in
		Sast.For(expr, expr2, expr3, stmt)
	| While(expr, stmt) ->
		let expr = check_expr scope expr in
		let stmt = check_stmt scope stmt in
		Sast.While(expr, stmt)

let process_var_decl (scope : symbol_table) v =
	let triple = match v with
		Variable(t, name) -> (name, v, t)
		| Variable_Initialization(t, name, expr) -> if t <> check_expr expr then raise Failure "wrong type" else (name, v, t) 
		| Array_Initialization(t, name, el) ->
			try 
				let _ = List.find( fun elem -> t <> check_expr elem) el in raise Failure "wrong type"
			with Not_found -> (name, v, t)
		| Struct_Initialization(t, name, el) -> match var_types with
			Struct(id) -> 
				try
					let s = find_struct scope id in
					try
						let _ = List.iter2 (
							fun a b -> match a with
							(t, _ ) -> if t <> check_expr b then raise Failure "wrong type" else t
							| (t, _, _)  -> if t <> check_expr b then raise Failure "wrong type" else t
						) s.variable_decls el in
						(name, v, t)
					with _ -> raise Failure "Not enough arguments or wrong type."
				with Not_found -> raise Failure ("struct " ^ id ^ " not found")
			| _ -> raise Failure "Not a struct"
	in scope.variables <- scope.variables :: triple; (* Update the scope *)
	Sast.variable_decl(triple)

let process_func_decl (env : translation_environment) f =
	try
		let _ = find_func env.scope.functions f.fname in
			raise Failure ("Function already declared with name " ^ f.fname)
	with Not_found ->
		let scope' = { env.scope with parent = Some(env.scope); variables = [] } in
		let _ = List.iter process_var_decl scope' f.formals::f.locals in
		(* should we keep result of process_var_decl? *)
		let statments = List.fold_left (
			fun a s -> a :: check_statement scope' s
		) [] f.body in
		env.scope.functions <- env.scope.functions :: f; (* throw away scope of function *)
		{ f with body = statements }

let process_assert (scope: symbol_table) a =
	let (expr, stml) = a in
	if check_expr expr <> Boolean then raise Failure "assert expr must be boolean " else
	List.iter check_stmt stml

let check_struct (scope : symbol_table) s =
	let scope' = { scope with parent = Some(scope); variables = [] } in
	let _ = List.iter process_var_decl scope' s.variable_decls in
	(* should we keep result of process_var_decl? *)
	List.iter process_assert scope' s.asserts

let process_struct_decl (env : translation_environment) s =
	try
		let _ = find_struct env.scope.structs s.sname in
			raise Failure ("struct already declared with name " ^ s.sname)
	with Not_found ->
		check_struct env.scope s;(* Throw away scope of the struct *)
		env.scope.structs <- env.scope.structs :: s; s

let process_global_decl (env : translation_environment) g =
	try
		let _ = check_id env.scope g in
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
			fun a s -> a :: process_struct_decl env s
		) [] structs in
	let globals =
		List.fold_left (
			fun a g -> a :: process_global_decl env g
		) [] vars in
	let funcs = List.fold_left (
			fun a f -> a :: process_func_decl env f
		) [] funcs in
	try
		let _ = List.find( fun (_, f, _, _) -> f == "main" ) env.scope.functions in
		structs, globals, funcs
	with Not_found -> raise Failure "No main function defined."

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
