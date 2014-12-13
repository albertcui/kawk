open Ast
open Sast
open Lexing
open Map

type function_table = {
	funcs : func_decl list
}

type symbol_table = {
	mutable parent : symbol_table option;
	mutable variables : (string * checked_var_decl * var_types) list;
	mutable functions : function_decl list;
	mutable structs : struct_decl list;
}

type translation_environment = {
	mutable scope : symbol_table (* symbol table for vars *)
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
				) [] f.checked_formals el in
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
						fun (t, _) -> match t with
						Variable(_, n) -> n = id
						| Variable_Initialization(_, n, _) -> n = id
						| Array_Initialization(_, n, _) -> n = id
						| Struct_Initialization(_, n, _) -> n = id
					) s.variable_decls in
					let (var, _) = var in 
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
	Ast.Uniop(op, expr) -> (
		match op with
			Not ->
				let e = check_expr scope expr in
				let (_, t) = e in 
				if (t <> Boolean) then raise (Failure "Incorrect type for ! ") else Sast.Uniop(op, e), Boolean
			| _ -> raise (Failure "Not a unary operator")
		)
	| _ -> raise (Failure "Not a uniop")	
(* 
let process_func_formals (env : translation_environment) f =
	let scope' = { env.scope with parent = Some(env.scope); variables = [] } in
	let scope' = List.iter (fun var -> scope.variables:: head) *)

let rec check_stmt (scope : symbol_table) (stmt : Ast.stmt) = match stmt with
	Block(sl) -> Sast.Block(List.fold_left ( fun a s -> (check_stmt scope s) :: a) [] sl)
	| Expr(e) -> Sast.Expr(check_expr scope e)
	| Return(e) -> Sast.Return(check_expr scope e)
	| If(expr, stmt1, stmt2) -> 
		let new_expr = check_expr scope expr in
		let new_stmt1 = check_stmt scope stmt1 in
		let new_stmt2 = check_stmt scope stmt2 in
		Sast.If(new_expr, new_stmt1, new_stmt2)
	| For(expr1, expr2, expr3, stmt) ->
		let expr = check_expr scope expr1 in
		let expr2 = check_expr scope expr2 in
		let expr3 = check_expr scope expr3 in
		let stmt = check_stmt scope stmt in
		Sast.For(expr, expr2, expr3, stmt)
	| While(expr, stmt) ->
		let expr = check_expr scope expr in
		let stmt = check_stmt scope stmt in
		Sast.While(expr, stmt)

let process_var_decl (scope : symbol_table) (v : Ast.var_decl) =
	let triple = match v with
		Variable(t, name) -> (name, Sast.Variable(t, name), t)
		| Variable_Initialization(t, name, expr) ->
			let expr = check_expr scope expr in
			let (_, t2 ) = expr in
			if t <> t2 then raise (Failure "wrong type") else (name, Sast.Variable_Initialization(t, name, expr), t) 
		| Array_Initialization(t, name, el) ->
			let el = List.fold_left (
				fun a elem ->
				let expr = check_expr scope elem in 
				let (_, t2 ) = expr in 
				if t <> t2 then raise (Failure "wrong type") else expr :: a
			) [] el in (name, Sast.Array_Initialization(t, name, el), t)
		| Struct_Initialization(t, name, el) -> 
			(
				match t with
					Struct(id) -> 
						(
							try
								let s = find_struct scope.structs id in
								(* DO NOT THROW AWAY RESPONSE`*)
								let el = List.fold_left2 (
									fun a b c -> let t = 
									let (b, _ ) = b in match b with
									Variable(t, _) -> t
									| Variable_Initialization(t, _, _) -> t
									| Array_Initialization(t, _, _) -> t
									| Struct_Initialization(t, _, _) -> t in
									let e = check_expr scope c in
									let (_, t2) = e in
									if t <> t2 then raise (Failure "types are not the same") else e :: a
								) [] s.variable_decls el in (name, Sast.Struct_Initialization(t, name, el), t)
							with _ -> raise (Failure ("struct " ^ id ^ " not found"))
						)
					| _ -> raise (Failure "Not a struct")
			) in
	let (_, decl, t) = triple in
	scope.variables <- triple :: scope.variables; (* Update the scope *)
	(decl, t)

let rec check_func_stmt (scope : symbol_table) (stml : Sast.stmt list) (ftype : Ast.var_types) = 
	List.iter (
		fun s -> match s with 
		Sast.Block (sl) ->
			check_func_stmt scope sl ftype
		| Sast.Return(e) -> 
			let (_, t) = e in 
			if t <> ftype then raise (Failure "return type is incorrect") else ()
		| Sast.If(_, s1, s2) -> 
			check_func_stmt scope [s1] ftype; check_func_stmt scope [s2] ftype
		| Sast.For(_, _, _, s) ->
			check_func_stmt scope [s] ftype
		| Sast.While(_, s) ->
			check_func_stmt scope [s] ftype
		| _ -> ()
	) stml

let process_func_stmt (scope : symbol_table) (stml : Ast.stmt list) (ftype : Ast.var_types) = 
	List.fold_left (
		fun a s -> let stmt = check_stmt scope s in
		match stmt with 
		Sast.Block (sl) ->
			check_func_stmt scope sl ftype; stmt :: a
		| Sast.Return(e) -> 
			let (_, t) = e in 
			if t <> ftype then raise (Failure "return type is incorrect") else
			stmt :: a 
		| Sast.If(_, s1, s2) -> 
			check_func_stmt scope [s1] ftype; check_func_stmt scope [s2] ftype; stmt :: a
		| Sast.For(_, _, _, s) ->
			check_func_stmt scope [s] ftype; stmt :: a
		| Sast.While(_, s) ->
			check_func_stmt scope [s] ftype; stmt :: a
		| _ -> stmt :: a
	) [] stml

let process_func_decl (env : translation_environment) (f : Ast.func_decl) =
	try
		let _ = find_func env.scope.functions f.fname in
			raise (Failure ("Function already declared with name " ^ f.fname))
	with Not_found ->
		let scope' = { env.scope with parent = Some(env.scope); variables = [] } in
		let formals = List.fold_left (
			fun a f -> match f with
			Ast.Variable(t, n) -> scope'.variables <- (n, Sast.Variable(t, n), t) :: scope'.variables; (Sast.Variable(t, n), t) :: a
			| _ -> raise (Failure "formal can only be of form <type> <id>")
		) [] f.formals in
		let locals = List.fold_left ( fun a l -> process_var_decl scope' l :: a ) [] f.locals in
		let statements = process_func_stmt scope' f.body f.ftype in 
		let f = { ftype = f.ftype; fname = f.fname; checked_formals = formals; checked_locals = locals; checked_body = statements } in
		env.scope.functions <- f :: env.scope.functions; (* throw away scope of function *)
		f
		
let process_assert (scope: symbol_table) a =
	let (expr, stml) = a in
	let expr  = check_expr scope expr in
	let (_, t) = expr in
	if t <> Boolean then (raise (Failure "assert expr must be boolean")) else
	let stml = List.fold_left ( fun a s -> check_stmt scope s :: a) [] stml in
	(expr, stml)

(* let check_struct (scope : symbol_table) s =
	let scope' = { scope with parent = Some(scope); variables = [] } in
	let vars = List.fold_left ( fun a s -> process_var_decl scope' :: a) [] s.variable_decls in
	(* should we keep result of process_var_decl? *)
	List.iter process_assert scope' s.asserts *)

let process_struct_decl (env : translation_environment) (s : Ast.struct_decl) =
	try
		let _ = find_struct env.scope.structs s.sname in
			raise (Failure ("struct already declared with name " ^ s.sname))
	with Not_found ->
		let scope' = { env.scope with parent = Some(env.scope); variables = [] } in
		let vars = List.fold_left ( fun a v -> process_var_decl scope' v :: a ) [] s.variable_decls in
		let asserts = List.fold_left ( fun a asrt -> process_assert scope' asrt :: a ) [] s.asserts in
		let s = { sname = s.sname; variable_decls = vars; asserts = asserts; } in 
		env.scope.structs <- s :: env.scope.structs; s

let process_global_decl (env : translation_environment) (g : Ast.var_decl) =
	try
		let name = match g with
			Variable(_, id) -> id
			| Variable_Initialization(_, id, _) -> id
			| Array_Initialization(_, id, _) -> id
			| Struct_Initialization(_, id, _) -> id in
		let _ = check_id env.scope name in
		raise (Failure ("Variable already declared with name " ^ name))
	with Not_found -> process_var_decl env.scope g
let check_program (p : Ast.program) =
	let s = { parent = None; variables = []; functions = []; structs = [] } in
	let env = { scope = s } in
	let (structs, vars, funcs) = p in 	
	let structs = 
		List.fold_left (
			fun a s -> process_struct_decl env s :: a
		) [] structs in
	let globals =
		List.fold_left (
			fun a g -> process_global_decl env g :: a
		) [] vars in
	let funcs = List.fold_left (
			fun a f -> process_func_decl env f :: a
		) [] funcs in
	try
		let _ = List.find( fun f -> f.fname == "main" ) env.scope.functions in
		structs, globals, funcs
	with Not_found -> raise (Failure "No main function defined.")

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
