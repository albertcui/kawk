open Ast
open Lexing

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
	| Call(str, expr_list) -> Printf.printf "%s(" str; let rec print_expr_list_comma = function
	[] -> print_string ""
	| hd::[] -> print_expr hd
	| hd::tl -> print_expr hd; print_string ", "; print_expr_list_comma tl 
	in print_expr_list_comma expr_list; print_string ") "
	| Access(str1, str2) -> Printf.printf "%s.%s " str1 str2 

let rec print_expr_list_comma = function
	[] -> print_string ""
	| hd::[] -> print_expr hd
	| hd::tl -> print_expr hd; print_string ", "; print_expr_list_comma tl 

(* and print_expr_comma expr =
	print_expr expr; print_string ", "
	 *)
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
	| Array(var_types, expr) -> print_var_types var_types; print_string "["; print_expr expr; print_string "] "

let rec print_var_decl = function
	Variable(var_types, str) -> print_var_types var_types; print_string (str ^ ";\n")
	| Variable_Initialization(var_types, str, expr) -> print_var_types var_types; Printf.printf "%s = " str; print_expr_semi expr
	| Array_Initialization(var_types, str, expr_list) -> print_var_types var_types; Printf.printf "[]%s = { " str; print_expr_list_comma expr_list; print_string "};\n"
	| Struct_Initialization(var_types, str, expr_list) -> print_var_types var_types; Printf.printf "%s = { " str; List.iter print_expr expr_list; print_string "};\n"


let print_asserts a =
	let (expr, stmt_list) = a in
	print_string "@("; print_expr expr; print_string ") "; List.iter print_stmt stmt_list

(* FIX THIS *)
let print_struct_decl s =
	print_string "struct ";
	print_string s.sname; 
	print_string " {\n";
	List.iter print_var_decl s.variable_decls;
	List.iter print_asserts s.asserts;
	print_string "}"
	
let print_unit_decl = function
	Local_udecl(udecl_params, udecl_check_val, true) -> print_string "unit("; print_expr_list_comma udecl_params; print_string "):equals("; print_expr udecl_check_val; print_string "):accept;\n"
	| Local_udecl(udecl_params, udecl_check_val, false) -> print_string "unit("; print_expr_list_comma udecl_params; print_string "):equals("; print_expr udecl_check_val; print_string "):reject;\n"
	| Outer_udecl(str, udecl_params, udecl_check_val, true) -> print_string "unit:"; print_string (str ^ "(");  print_expr_list_comma udecl_params; print_string "):equals("; print_expr udecl_check_val; print_string "):accept;\n"
	| Outer_udecl(str, udecl_params, udecl_check_val, false) -> print_string "unit:"; print_string (str ^ "(");  print_expr_list_comma udecl_params; print_string "):equals("; print_expr udecl_check_val; print_string "):reject;\n"

let print_param = function
	Param(var_types, str) -> print_var_types var_types; print_string (str)

let rec print_param_list = function
	[] -> print_string "";
	| hd::[] -> print_param hd;
	| hd::tl -> print_param hd; print_string ", "; print_param_list tl

let print_func_decl f =
	print_var_types f.ftype;
	print_string f.fname; 
	print_string "(";
	print_param_list f.formals; 
	print_string ") {\n";
	List.iter print_var_decl f.locals; 
	List.iter print_stmt f.body;
	List.iter print_unit_decl f.units;
	print_string "}\n"

let print_program p = 
	let (structs, vars, funcs, unts) = p in 	
		List.iter print_struct_decl structs;
		List.iter print_var_decl vars;
		List.iter print_func_decl (List.rev funcs);
		List.iter print_unit_decl unts 

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
