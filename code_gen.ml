(* Code gen*)
open Ast
open Sast
open Jast
open Sast_to_jast
open Semantic_checker
open Lexing

let print_op = function
	Add -> print_string "+ "
	| Sub -> print_string "- "
	| Mult -> print_string "* "
	| Div -> print_string "/ "
	| Mod -> print_string "% "
	| Equal -> print_string "== "
	| Neq -> print_string "!= "
	| Less -> print_string "< " 
	| Leq -> print_string "<= "
	| Greater -> print_string "> "
	| Geq -> print_string ">= "
	| Or -> print_string "|| "
	| And -> print_string "&& "
	| Not -> print_string "! " 

(* FIX ID *)
let rec print_expr (e : Sast.expr_detail) = match e with
	Noexpr -> print_string ""
	| This -> print_string "this "
	| Null -> print_string "null "
	(* | Id(id) -> print_var_decl id *)
	| IntConst(i) -> Printf.printf "%d " i
	| StrConst(str) -> Printf.printf "%s " str
	| BoolConst(b) -> Printf.printf "%B " b
(* 	| ArrayAccess(str, expr) -> Printf.printf "%s[" str; print_expr expr; print_string "]"
	| Assign(str, expr) -> Printf.printf "%s = " str; print_expr expr
	| Uniop(op, expr) -> print_op op; print_expr expr
	| Binop(expr1, op, expr2) -> print_expr expr1; print_op op; print_expr expr2 
	| Call(f, expr_list) -> 
		(if f.fname = "print" then print_string "System.out.println("
		else Printf.printf "%s(" f.fname);
		let rec print_expr_list_comma = function
			[] -> print_string ""
			| (e, _)::[] -> print_expr e
			| (e, _)::tl -> print_expr e; print_string ", "; print_expr_list_comma tl 
			in print_expr_list_comma expr_list; print_string ") " *)
	(* | Access(str1, str2) -> Printf.printf "%s.%s " str1 str2  *)
	| _ -> print_string ""

let rec print_expr_list_comma (el : Sast.expression list) = match el with
	[] -> print_string ""
	| hd::[] -> let (expr_detail, _) = hd in print_expr expr_detail
	| hd::tl -> let (expr_detail, _) = hd in print_expr expr_detail; print_string ", "; print_expr_list_comma tl 

let print_expr_semi (e : Sast.expression) = 
	let (expr_detail, _) = e in 
	print_expr expr_detail; print_string ";\n"

let rec print_stmt = function
	Block(stmt_list) -> print_string "{"; List.iter print_stmt stmt_list; print_string "}\n"
	| Expr(expr) -> print_expr_semi expr
	| Return(expr) -> print_string "return "; print_expr_semi expr
	| If(expr, stmt1, stmt2) -> print_string "if ("; print_expr_semi expr; print_string ")"; print_stmt stmt1; print_stmt stmt2
	| For(expr1, expr2, expr3, stmt) -> print_string "for ("; print_expr_semi expr1; print_expr_semi expr2; let (expr_detail, _) = expr3 in print_expr expr_detail; print_stmt stmt 
	| While(expr, stmt) -> print_string "while ("; print_expr_semi expr; print_string ")"; print_stmt stmt

(* CHANGE: CANNOT DECLARE VARIABLE AS VOID *)
(* DID NOT CHECK IF ARRAY(VAR_TYPES, EXPR) WHERE EXPR IS INT
   JAST should redecine var_types so that struct of struct_decl and array expr of expr_detail *)
let rec print_var_types = function
	Void -> print_string "void "
	| Int -> print_string "int "
	| String -> print_string "String " 
	| Boolean -> print_string "boolean "
	(* | Struct(str) -> Printf.printf "public class %s " str  *)
	(* | Array(var_types, expr) -> print_var_types var_types; print_string "["; print_expr expr; print_string "] " *)
 
let rec print_var_decl  (v : Sast.variable_decl) =
	let (var_types, _) = v in match var_types with
		Variable(var_types, str) -> print_var_types var_types; print_string (str ^ ";\n")
		| Variable_Initialization(var_types, str, expr) -> print_var_types var_types; Printf.printf "%s = " str; print_expr_semi expr
		| Array_Initialization(var_types, str, expr_list) -> print_var_types var_types; Printf.printf "[] %s = { " str; print_expr_list_comma expr_list; print_string "};\n"
	(* | Struct_Initialization(var_types, str, expr_list) -> print_var_types var_types; Printf.printf "%s = { " str; print_expr expr_list; print_string "};\n" *)

(* let print_assert_name expr = *)

(* let print_asserts a =
	(*let (expr, stmt_list) = a in
		if List.length (stmt_list) != 0 then 
			print_string "set"; print_assert_name expr; print_string "() {\n";
			(* do assignment *)
			print_string "if ( ";print_expr expr;print_string ") "; List.iter print_stmt stmt_list; print_string "}"   
		else print_string "";*)

	print_string "@("; print_expr expr; print_string ") "; List.iter print_stmt stmt_list
 *)
(*
	struct potato { 
		int size; 
		int potat; 
		@(potate<2 ) 
			{print "Asdasldkfasd"}
		int j;
		@(potato == 1 ) {print "ASfdasdfas"}
	}


 	public class Potato {
		 int size;
		 int potat;

		public Blah(size,potat){
			this.size = size;
			this.potat = potat;
		}

		public getSize(){
			return size;
		}
		public setSize(int size){
			this.size = size;
			if (this.size>1){
				return ("AHH THIS IS > 1");
			}
		}
		public getPotat(){
			return size;
		}
		public setPotat(int potat){
			this.potat = potat;
		}
 	}
*)

(* let print_struct_decl (j : Jast.j_var_struct_decl) =
	let List.iter (fun ) *)

(* FIX THIS *)
(* let print_struct_decl s =
	if s.sname = "main" then 
		print_string "public static void main(String[] args) {\n";
		List.iter print_var_decl s.variable_decls;
		List.iter print_asserts s.asserts;
		print_string "}"
	else
		print_string "public class ";
		print_string s.sname; 
		print_string " {\n";
		List.iter print_var_decl s.variable_decls;
		List.iter print_asserts s.asserts;
		print_string "}" *)
	
(* let print_unit_decl = function
	Local_udecl(udecl_params, udecl_check_val, true) -> print_string "unit("; print_expr_list_comma udecl_params; print_string "):equals("; print_expr udecl_check_val; print_string "):accept;\n"
	| Local_udecl(udecl_params, udecl_check_val, false) -> print_string "unit("; print_expr_list_comma udecl_params; print_string "):equals("; print_expr udecl_check_val; print_string "):reject;\n"
	| Outer_udecl(str, udecl_params, udecl_check_val, true) -> print_string "unit:"; print_string (str ^ "(");  print_expr_list_comma udecl_params; print_string "):equals("; print_expr udecl_check_val; print_string "):accept;\n"
	| Outer_udecl(str, udecl_params, udecl_check_val, false) -> print_string "unit:"; print_string (str ^ "(");  print_expr_list_comma udecl_params; print_string "):equals("; print_expr udecl_check_val; print_string "):reject;\n"
 *)

let print_param v =
	let (var_types, _) = v in match var_types with
	Variable(var_types, str) -> print_var_types var_types; print_string str
	| _ -> raise (Failure "This should never happen")

let rec print_param_list (p : Sast.variable_decl list) = match p with
	[] -> print_string "";
	| hd::[] -> print_param hd;
	| hd::tl -> print_param hd; print_string ", "; print_param_list tl

let print_func_decl (f : Sast.function_decl) =
	if f.fname = "main" then 
		(print_string "public static void main(String[] args) {\n";
		List.iter print_var_decl f.checked_locals;
		List.iter print_stmt f.checked_body;
		print_string "}")
	else
		print_var_types f.ftype;
		print_string f.fname; 
		print_string "(";
		print_param_list f.checked_formals; 
		print_string ") {\n";
		List.iter print_var_decl f.checked_locals; 
		List.iter print_stmt f.checked_body;
		(* List.iter print_unit_decl f.ch0ecked_units; *)
		print_string "}\n"

let code_gen j =
	let _ = print_string "public class Program {\n" in
	let (structs, vars, funcs, unts) = j in
			(* List.iter print_struct_decl structs; *)
			List.iter print_var_decl vars;
			List.iter print_func_decl (List.rev funcs);
			(* List.iter print_unit_decl unts; *)
			print_string "\n}"

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	let sast = check_program ast in
	let jast = sast_to_jast sast in
	code_gen jast