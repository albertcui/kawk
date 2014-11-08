open Ast

let print_op = function
	Add -> print_string " +"
	| Sub -> print_string " -"
	| Mult -> print_string " *"
	| Div -> print_string " /"
	| Mod -> print_string " %"
	| Equal -> print_string " ="
	| Neq -> print_string " !="
	| Less -> print_string " <" 
	| Leq -> print_string " <="
	| Greater -> print_string " >"
	| Geq -> print_string " >="
	| Or -> print_string " |"
	| And -> print_string " &"
	| Not -> print_string " !" 

let rec print_expr = function
	Noexpr -> print_string ""
	| Id(id) -> Printf.printf " %s" id
	| Integer_literal(i) -> Printf.printf " %d" i 
	| String_literal(str) -> Printf.printf " %S" str
	| Boolean_literal(b) -> Printf.printf " %B" b
	| Array_access(str, expr) -> Printf.printf " %s[" str; print_expr expr; print_string "]"
	| Assign(str, expr) -> Printf.printf " %s" str; print_expr expr
	| Uniop(op, expr) -> print_op op; print_expr expr
	| Binop(expr1, op, expr2) -> print_expr expr1; print_op op; print_expr expr2
	| Call(str, expr_list) -> Printf.printf " %s" str; List.iter print_expr expr_list
	| Access(str1, str2) -> Printf.printf " %s.%s" str1 str2

let rec print_stmt = function
	Block(stmt_list) -> print_string " {"; List.iter print_stmt stmt_list; print_string "}"
	| Expr(expr) -> print_expr expr
	| Return(expr) -> print_string "return "; print_expr expr
	| If(expr, stmt1, stmt2) -> print_string "if ("; print_expr expr; print_string ")"; print_stmt stmt1; print_stmt stmt2
	| For(expr1, expr2, expr3, stmt) -> print_string "for ("; print_expr expr1; print_string ";"; print_expr expr2; print_string ";"; print_expr expr3; print_stmt stmt 
	| While(expr, stmt) -> print_string "while ("; print_expr expr; print_string ")"; print_stmt stmt

let rec print_var_types = function
	Void -> print_string "void"
	| Int -> print_string "int"
	| String -> print_string "str" 
	| Boolean -> print_string "bool"
	| Struct(str) -> Printf.printf "struct %s" str 
	| Array(var_types, expr) -> print_var_types var_types; print_string "["; print_expr expr; print_string "]"

let rec print_var_decl = function
	Variable(var_types, str) -> print_var_types var_types; print_string str
	| Variable_Initialization(var_types, str, expr) -> print_var_types var_types; Printf.printf " %s =" str; print_expr expr
	| Array_Initialization(var_types, str, stmt) -> print_var_types var_types; Printf.printf " %s =" str; print_stmt stmt
	| Struct_Initialization(str1, str2, stmt) -> Printf.printf("struct %s %s =") str1, str2; print_stmt stmt

let print_struct_body = function
	S_Variable_Decl(var_decl) -> print_var_types var_decl
	| Assert(expr, stmt_list) -> print_string "@ ("; print_expr expr; print_string " )"; List.iter print_stmt stmt

let rec print_struct_decl s = function
	print_string s.sname
	| List.iter print_struct_body s.sbody
	


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