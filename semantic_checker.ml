open Ast
open Sast
open Lexing
open Map

(* check return exists *)
(* have to reverse lists lololol *)

type function_table = {
	funcs : func_decl list
}

type symbol_table = {
	mutable parent : symbol_table option;
	mutable variables : (string * checked_var_decl * var_types) list;
	mutable functions : function_decl list;
	mutable structs : struct_decl list;
	mutable return_found : bool;
}

type translation_environment = {
	mutable scope : symbol_table; (* symbol table for vars *)
	mutable found_main: bool;
}

let the_print_function = {
	ftype = Sast.Void;  
	fname = "print"; 
	checked_formals = [];
	checked_locals = [];
	checked_body = [];
	checked_units = []
}

let the_exit_function = {
	ftype = Sast.Void;  
	fname = "exit"; 
	checked_formals = [];
	checked_locals = [];
	checked_body = [];
	checked_units = []	
}

let find_struct (s : struct_decl list) stru =
	List.find(fun c -> c.sname = stru) s

let find_func (l : function_decl list) f =
	List.find(fun c -> c.fname = f) l

let rec check_id (scope : symbol_table) id =
	try
		(* let _ = print_string ("check_id called, legnth of scope.variables is " ^ string_of_int (List.length scope.variables) ^ "\n") in *)
		(* let _ = List.iter (fun (n, _, _) -> print_string ("try printing in check_id: " ^ n ^ "\n")) scope.variables in *)
		let (_, decl, t) = List.find(fun (n, _, _) -> n = id ) scope.variables in
		decl, t
	with Not_found -> match scope.parent with
		Some(parent) -> check_id parent id
		| _ -> raise Not_found
 
 let rec check_expr (scope : symbol_table) (expr : Ast.expr) = match expr with
 	(* let _ = print_string ("try printing at top of process_var_decl, length of scope.variables is " ^ string_of_int (List.length scope.variables) ^ "\n") in match expr with *)
	Noexpr -> Sast.Noexpr, Void
	| Id(str) -> 
		(try 
			let (decl, t) = check_id scope str in Sast.Id(decl), t 
		with Not_found -> raise (Failure ("Id named " ^ str ^ " not found")))
	| Integer_literal(i) -> Sast.IntConst(i), Sast.Int
	| String_literal(str) -> Sast.StrConst(str), Sast.String
	| Boolean_literal(b) -> Sast.BoolConst(b), Sast.Boolean
	| Array_access(_, _) as a -> check_array_access scope a
	| Assign(_, _) as a -> check_assign scope a
	| Uniop(op, expr) as u -> check_uni_op scope u
	| Binop(_, _, _) as b -> check_op scope b
	| Call(_, _) as c -> check_call scope c
	| Access(_, _) as a -> check_access scope a
	| Struct_Member_Assign(_, _, _) as a -> check_struct_assignment scope a
	| Array_Member_Assign(_, _, _) as a -> check_array_assignment scope a

and check_array_assignment (scope : symbol_table) a = match a with
	Ast.Array_Member_Assign(arr, expr, expr2) ->
		(
			try
				let (original_decl, var_type) = check_id scope arr in match var_type with
					Sast.Array(decl, _) ->
						(
							let access_expr = check_expr scope expr in
							let (_, t) = access_expr in
							if t <> Sast.Int then
								raise (Failure "Array access must be type int")
							else
								(let assign_expr = check_expr scope expr2 in
								let (_, t2) = assign_expr in
								if decl <> t2 then raise (Failure "type assignment is wrong")
								else Sast.Array_Member_Assign(original_decl, access_expr, assign_expr), t2)
						)
					| _ -> raise (Failure (arr ^ " is not an array."))
			with Not_found -> raise (Failure ("Variable " ^ arr ^ " not declared."))
		)
	| _ -> raise (Failure "Not an array assignment")

and check_struct_assignment (scope : symbol_table) a = match a with
	Ast.Struct_Member_Assign(stru, mem, expr) ->
		(
			try
				let (original_decl, var_type) = check_id scope stru in match var_type with
				| Sast.Struct(decl) ->
					(
						try
							let v = List.find(
								fun (v, _) -> match v with
								Variable(_, s) -> s = mem
								| Variable_Initialization(_, s, _) -> s = mem
								| Array_Initialization(_, s, _) -> s = mem
								| Struct_Initialization(_, s, _) -> s = mem
							) decl.variable_decls in
							let expr = check_expr scope expr in
							let (_, t) = v in
							let (_, t2) = expr in
							if t <> t2 then raise (Failure "type assignment is wrong")
							else Sast.Struct_Member_Assign(decl, original_decl, v, expr), var_type
						with Not_found -> raise (Failure (mem ^ " not found in struct " ^ stru))
					)
				| _ -> raise (Failure (stru ^ " is not a struct."))
			with Not_found -> raise (Failure ("Variable " ^ stru ^ " not declared."))
		)
	| _ -> raise (Failure "Not a struct assignment")

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
			| Sub -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for - ") else Sast.Int
			| Mult -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for * ") else Sast.Int
			| Div -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for / ") else Sast.Int
			| Mod -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for % ") else Sast.Int
			| Equal -> if (t1 <> t2) then raise (Failure "Incorrect types for = ") else Sast.Boolean
			| Neq -> if (t1 <> t2) then raise (Failure "Incorrect types for != ") else Sast.Boolean
			| Less -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for < ") else Sast.Boolean
			| Leq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for <= ") else Sast.Boolean
			| Greater -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for > ") else Sast.Boolean
			| Geq -> if (t1 <> Int || t2 <> Int) then raise (Failure "Incorrect types for >= ") else Sast.Boolean
			| Or -> if (t1 <> Boolean || t2 <> Boolean) then raise (Failure "Incorrect types for | ") else Sast.Boolean
			| And -> if (t1 <> Boolean || t2 <> Boolean) then raise (Failure "Incorrect types for & ") else Sast.Boolean
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
		(
			try
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
			with Not_found ->
				if id = "print" then match el with
					| hd :: []-> let expr = check_expr scope hd in
						let (_, t) = expr in
						if (t = Sast.String || t = Sast.Int) then Sast.Call(the_print_function, [expr]), Sast.Void else raise (Failure "Print takes only type string or int")
					| _ -> raise (Failure "Print only takes one argument")  
				else if id = "exit" then match el with
					| hd :: []-> let expr = check_expr scope hd in
						let (_, t) = expr in
						if t = String then Sast.Call(the_exit_function, [expr]), Sast.Void else raise (Failure "Exit takes only type string")
					| _ -> raise (Failure "Exit only takes one argument")
				else if id = "main" then 
					raise (Failure "Cannot fall main function")
				else raise (Failure ("Function not found with name " ^ id))
		)
	| _ -> raise (Failure "Not a call")	

and check_access (scope : symbol_table) a = match a with
	Ast.Access(id, id2) ->
		(let (original_decl, t) = check_id scope id in match t with
			Struct(decl) ->
				(try
					let var = List.find (
						fun (t, _) -> match t with
						Variable(_, n) -> n = id2
						| Variable_Initialization(_, n, _) -> n = id2
						| Array_Initialization(_, n, _) -> n = id2
						| Struct_Initialization(_, n, _) -> n = id2
					) decl.variable_decls in
					let (var, _) = var in 
					let t = match var with
						Variable(t, _) -> t
						| Variable_Initialization(t, _, _) -> t
						| Array_Initialization(t, _, _) -> t
						| Struct_Initialization(t, _, _) -> t
					in Sast.Access(decl, original_decl, var), t
				with Not_found -> raise (
					Failure (id ^ " is type struct " ^ decl.sname ^ " which does not have a member named " ^ id2)
				))
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
		let (_, t) = new_expr in
		if t <> Sast.Boolean then
			raise (Failure "If statement must have a boolean expression")
		else 
			let new_stmt1 = check_stmt scope stmt1 in
			let new_stmt2 = check_stmt scope stmt2 in
			Sast.If(new_expr, new_stmt1, new_stmt2)
	| For(expr1, expr2, expr3, stmt) ->
		let expr = check_expr scope expr1 in
		let expr2 = check_expr scope expr2 in
		let (_, t) = expr2 in
		if t <> Sast.Boolean then
			raise (Failure "If statement must have a boolean expression")
		else 
			let expr3 = check_expr scope expr3 in
			let stmt = check_stmt scope stmt in
			Sast.For(expr, expr2, expr3, stmt)
	| While(expr, stmt) ->
		let expr = check_expr scope expr in
		let (_, t) = expr in
		if t <> Sast.Boolean then
			raise (Failure "If statement must have a boolean expression")
		else 
			let stmt = check_stmt scope stmt in
			Sast.While(expr, stmt)

let rec check_var_type (scope : symbol_table) (v : Ast.var_types) = match v with
	Ast.Void -> Sast.Void
	| Ast.Int -> Sast.Int
	| Ast.String -> Sast.String
	| Ast.Boolean -> Sast.Boolean
	| Ast.Struct(id) ->
		(try
			let s = find_struct scope.structs id in
			Sast.Struct(s)
		with Not_found -> raise (Failure ("Struct " ^ id ^ " not found.")))
	| Ast.Array(v, expr) ->
		let v = check_var_type scope v in
		let expr = check_expr scope expr in
		let (_, t) = expr in
		if t <> Int then raise (Failure "Array size must have integer.")
		else Sast.Array(v, expr) 

let process_var_decl (scope : symbol_table) (v : Ast.var_decl) =
	(* let _ = print_string ("try printing at top of process_var_decl, length of scope.variables is " ^ string_of_int (List.length scope.variables) ^ "\n") in *)
	let triple = match v with
		Variable(t, name) ->
			let t = check_var_type scope t in 
			(name, Sast.Variable(t, name), t)
		| Variable_Initialization(t, name, expr) ->
			let t = check_var_type scope t in
			let expr = check_expr scope expr in
			let (_, t2 ) = expr in
			if t <> t2 then raise (Failure "wrong type for variable initialization") else (name, Sast.Variable_Initialization(t, name, expr), t) 
		| Array_Initialization(t, name, el) -> (match t with
			Ast.Array(v, expr) ->  
				let t = check_var_type scope v in
				let el = List.fold_left (
					fun a elem ->
					let expr = check_expr scope elem in 
					let (_, t2 ) = expr in 
					if t <> t2 then raise (Failure "wrong type for array initilization") else expr :: a
				) [] el in (name, Sast.Array_Initialization(Sast.Array(t, (Noexpr, Void)), name, List.rev el), Sast.Array(t, (Noexpr, Void)))
			| _ -> raise (Failure "Not an arrary!"))
		| Struct_Initialization(t, name, el) ->
			let t = check_var_type scope t in match t with
				Struct(decl) -> 
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
						if t <> t2 then raise (Failure "types are not the same in struct initialization") else e :: a
					) [] decl.variable_decls el in (name, Sast.Struct_Initialization(t, name, el), t)
				| _ -> raise (Failure "Not a struct") in (*test?*)
	let (_, decl, t) = triple in
	if t = Void then
		raise (Failure "Variable cannot be type void.")
	else 
		scope.variables <- triple :: scope.variables; (* List.iter (fun (n, _, _) -> print_string ("try printing in process_var_decl:" ^ n ^ "\n")) scope.variables; *) (* Update the scope *)
		(decl, t)

let rec check_func_stmt (scope : symbol_table) (stml : Sast.stmt list) (ftype : Sast.var_types) = 
	List.iter (
		fun s -> match s with 
		Sast.Block (sl) ->
			check_func_stmt scope sl ftype
		| Sast.Return(e) -> 
			let (_, t) = e in 
			if t <> ftype then raise (Failure "func return type is incorrect") else ()
		| Sast.If(_, s1, s2) -> 
			check_func_stmt scope [s1] ftype; check_func_stmt scope [s2] ftype
		| Sast.For(_, _, _, s) ->
			check_func_stmt scope [s] ftype
		| Sast.While(_, s) ->
			check_func_stmt scope [s] ftype
		| _ -> ()
	) stml

let process_func_stmt (scope : symbol_table) (stml : Ast.stmt list) (ftype : Sast.var_types) = 
	List.fold_left (
		fun a s -> let stmt = check_stmt scope s in
		match stmt with 
		Sast.Block (sl) ->
			check_func_stmt scope sl ftype; stmt :: a
		| Sast.Return(e) -> 
			let (_, t) = e in 
			if t <> ftype then raise (Failure "while processing func statement, return type incorrect") else
			scope.return_found <- true; stmt :: a 
		| Sast.If(_, s1, s2) -> 
			check_func_stmt scope [s1] ftype; check_func_stmt scope [s2] ftype; stmt :: a
		| Sast.For(_, _, _, s) ->
			check_func_stmt scope [s] ftype; stmt :: a
		| Sast.While(_, s) ->
			check_func_stmt scope [s] ftype; stmt :: a
		| _ -> stmt :: a
	) [] stml

let process_func_units (scope : symbol_table) (u : Ast.unit_decl) (formals : Sast.variable_decl list) (ftype : Sast.var_types) = match u with
	Local_udecl (el, e, b) -> 
	let exprs = List.fold_left2 (
				fun a b c -> 
					let (_, t) = b in
					let expr = check_expr scope c in
					let (_, t2) = expr in
					if t <> t2							(*stopped tests here going *)
					then raise (Failure "while processing func units, wrong type")
					else expr :: a
			) [] formals el in
		let expr = check_expr scope e in 
		let (_, t ) = expr in 
		if t <> ftype then raise (Failure "while processing func units, incorrect return type") else
		Sast.Local_udecl(exprs, expr, b)
	| Outer_udecl (f, el, e, b) ->
	(try
		let f = find_func scope.functions f in
		let exprs = List.fold_left2 (
				fun a b c -> 
					let (_, t) = b in
					let expr = check_expr scope c in
					let (_, t2) = expr in
					if t <> t2
					then raise (Failure "wrong type")
					else expr :: a
			) [] f.checked_formals el in
		let expr = check_expr scope e in 
		let (_, t ) = expr in 
		if t <> f.ftype then raise (Failure "Incorrect return type") else
		Sast.Outer_udecl(f, exprs, expr, b)
	with Not_found -> raise (Failure ("Function not found with name " ^ f)))

let check_func_decl (env : translation_environment) (f : Ast.func_decl) =
	let scope' = { env.scope with parent = Some(env.scope); variables = []; return_found = false } in
	let t = check_var_type env.scope f.ftype in
	let formals = List.fold_left (
		fun a f -> match f with
		Ast.Param(t, n) ->
			let t = check_var_type scope' t in 
			scope'.variables <- (n, Sast.Variable(t, n), t) :: scope'.variables; (Sast.Variable(t, n), t) :: a
	) [] f.formals in
	let locals = List.fold_left ( fun a l -> process_var_decl scope' l :: a ) [] f.locals in
	let statements = process_func_stmt scope' f.body t in 
	let units = List.fold_left ( fun a u -> process_func_units scope' u formals t :: a) [] f.units in
	if scope'.return_found then 
		let f = { ftype = t; fname = f.fname; checked_formals = formals; checked_locals = locals; checked_body = statements; checked_units = units } in
		env.scope.functions <- f :: env.scope.functions; (* throw away scope of function *) f
	else (if f.ftype = Void then 
		let f = { ftype = t; fname = f.fname; checked_formals = formals; checked_locals = locals; checked_body = statements; checked_units = units } in
			env.scope.functions <- f :: env.scope.functions; (* throw away scope of function *) f
	else
		raise (Failure ("No return for function " ^ f.fname ^ " when return expected.")))

let process_func_decl (env : translation_environment) (f : Ast.func_decl) =
	try
		let _ = find_func env.scope.functions f.fname in
			raise (Failure ("Function already declared with name " ^ f.fname))
	with Not_found ->
		if f.fname = "print" then raise (Failure "A function cannot be named 'print'")
		else
			if f.fname = "main" then
				(
					if f.ftype <> Void || (List.length f.formals) <> 0 then 
					raise (Failure "main function must be type void with no parameters")
					else 
						let func = check_func_decl env f in
						env.found_main <- true; func
					)
			else
				check_func_decl env f

let rec check_struct_stml (stml : Sast.stmt list) = 
	List.iter (
		fun s -> match s with 
		Sast.Block (sl) ->
			check_struct_stml sl
		| Sast.Return(_) -> raise (Failure "No returns are allowed in asserts")
		| Sast.If(_, s1, s2) -> 
			check_struct_stml [s1]; check_struct_stml [s2]
		| Sast.For(_, _, _, s) ->
			check_struct_stml [s]
		| Sast.While(_, s) ->
			check_struct_stml [s]
		| _ -> ()
	) stml

let process_assert (scope: symbol_table) a =
	let (expr, stml) = a in
	let expr  = check_expr scope expr in
	let (_, t) = expr in
	if t <> Sast.Boolean then (raise (Failure "assert expr must be boolean")) else
	let stml = List.fold_left ( fun a s -> check_stmt scope s :: a) [] stml in
	let _ = check_struct_stml stml in (expr, stml)

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
	with Not_found -> 
		(* let _ = print_string ("p_global_decl called, this id not found, legnth of env.scope.variables is " ^ string_of_int (List.length env.scope.variables) ^ "\n") in *)
		process_var_decl env.scope g

let process_outer_unit_decl (env : translation_environment) (u : Ast.unit_decl) = match u with
	Local_udecl (el, _, _) ->  raise (Failure "Can not define unit of this type in global scope ")
	| Outer_udecl (f, el, e, b) ->
	(try
		let f = find_func env.scope.functions f in
		let exprs = List.fold_left2 (
				fun a b c -> 
					let (_, t) = b in
					let expr = check_expr env.scope c in
					let (_, t2) = expr in
					if t <> t2
					then raise (Failure "wrong type while processing outer unit declaration")
					else expr :: a
			) [] f.checked_formals el in
		let expr = check_expr env.scope e in 
		let (_, t ) = expr in 
		if t <> f.ftype then raise (Failure "Incorrect return type in outer unit test") else
		Sast.Outer_udecl(f, exprs, expr, b)
	with Not_found -> raise (Failure ("Function not found with name " ^ f)))
	
let check_program (p : Ast.program) =
	(* let _ = print_string ("check_program called \n") in *)
	let s = { parent = None; variables = []; functions = []; structs = []; return_found = false } in
	let env = { scope = s; found_main = false } in
	let (structs, vars, funcs, units) = p in 	
	let structs = 
		List.fold_left (
			fun a s -> process_struct_decl env s :: a
		) [] structs in
	let globals =
		List.fold_left (
			fun a g -> process_global_decl env g :: a
		) [] (List.rev vars) in
	let funcs = 
		List.fold_left (
			fun a f -> process_func_decl env f :: a
		) [] (List.rev funcs) in
	let units = 
		List.fold_left (
			fun a u -> process_outer_unit_decl env u :: a
		) [] units in
(* 	try *)
	(* let _ = print_string ("length of env.scope.functions is " ^ string_of_int (List.length env.scope.functions) ^ "\n") in *)
(*     let rec findMain = function
    	[] -> false
    	| hd::tl ->
    		if hd.fname = "main" then
    			(if (hd.ftype <> Void || (List.length hd.checked_formals) <> 0) then (raise (Failure "main function must be type void with no arguments")) else true)
    		else findMain tl
    in let foundMain  = findMain env.scope.functions in *)
    (if env.found_main then structs, globals, funcs, units else (raise (Failure "No main function defined.")))
	    	(* let _ = List.iter( fun f -> if f.fname = "main" then print_string "Found main" else(*  print_string ("did not find main, found " ^ f.fname ^ "\n")) env.scope.functions in  *)
		let _ = List.find( fun f -> f.fname = "main" ) env.scope.functions in
		structs, globals, funcs, units
	with Not_found -> raise (Failure "No main function defined.") *)

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program =
		try Parser.program Scanner.token lexbuf 
		with _ -> Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf; exit (-1) in 
	check_program program *)
