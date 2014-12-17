(* Code gen*)
open Ast
open Sast
open Jast
open Sast_to_jast
open Semantic_checker
open Lexing

let jast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	let sast = check_program ast in
	sast_to_jast sast

let (j_struct_decl_list, _, _, _) = jast

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
	| Not -> print_string "!" 

let get_instance_name = function
	Variable(_, str) -> str
	(* if not a Variable we drop the unnecessary stuff *)
	| Variable_Initialization(_, str, _) -> str
	| Array_Initialization(_, str, _) -> str
	| Struct_Initialization(_, str, _) -> str

let rec print_expr (e : Sast.expression) = 
	let (e, _) = e in match e with
	Noexpr -> print_string ""
	| Id(decl) -> let str = match decl with
		Variable(_, str) -> str
		(* if not a Variable we drop the unnecessary stuff *)
		| Variable_Initialization(_, str, _) -> str
		| Array_Initialization(_, str, _) -> str
		| Struct_Initialization(_, str, _) -> str in
		print_string str
	| IntConst(i) -> Printf.printf "%d " i
	| StrConst(str) -> Printf.printf "%s " str
	| BoolConst(b) -> Printf.printf "%B " b
	(* | ArrayAccess(str, expr) -> Printf.printf "%s[" str; print_expr expr; print_string "]" *)
	(* JANKY FIX FOR WHERE EQUALS IS??*)
	| Assign(decl, expr) -> let str = match decl with
		Variable(_, str) -> str
		(* if not a Variable we drop the unnecessary stuff *)
		| Variable_Initialization(_, str, _) -> str
		| Array_Initialization(_, str, _) -> str
		| Struct_Initialization(_, str, _) -> str in
		print_string (str^" = "); print_expr expr
	| Uniop(op, expr) -> print_op op; print_expr expr
	| Binop(expr1, op, expr2) -> print_expr expr1; print_op op; print_expr expr2 
	| Call(f, expr_list) -> 
		if f.fname = "exit" then (print_string "\n\tSystem.out.println("; List.iter print_expr expr_list; print_string ");\n\tSystem.exit(0)") 
		else
			((if f.fname = "print" then print_string "\n\tSystem.out.println("
			else Printf.printf "%s(" f.fname);
			let rec print_expr_list_comma = function
				[] -> print_string ""
				| e::[] -> print_expr e
				| e::tl -> print_expr e; print_string ", "; print_expr_list_comma tl 
				in print_expr_list_comma expr_list; print_string ")")
	(* | Access(struc, decl) -> *)
(* 		let j_s_decl = List.find ( fun j -> j.original_struct = struc) j_struct_decl_list in
		let var = List.find ( fun j_v -> j_v.the_variable = decl) j_s_decl in
		if (List.length var.asserts) <> 0 then  *)
	| Struct_Member_Assign(struc, instance, decl, expr) ->
		let j_s_decl = List.find ( fun j -> j.original_struct = struc) j_struct_decl_list in
		let var = List.find ( fun j_v -> j_v.the_variable = decl) j_s_decl.variable_decls in
		if (List.length var.asserts) <> 0 then (print_string (get_instance_name instance); print_string (".set_" ^ var.name ^ "("); print_expr expr; print_string ")")
		else
			(print_string (get_instance_name instance); print_string "."; print_string (var.name ^ "="); print_expr_semi expr)
	| _ -> print_string ""
and print_expr_semi (e : Sast.expression) = 
	print_expr e; print_string ";\n"

let rec print_expr_list_comma (el : Sast.expression list) = match el with
	[] -> print_string ""
	| hd::[] -> print_expr hd
	| hd::tl -> print_expr hd; print_string ", "; print_expr_list_comma tl 

let rec print_stmt = function
	Block(stmt_list) -> print_string "{";  List.iter print_stmt stmt_list; print_string "}\n"
	| Expr(expr) -> print_expr_semi expr
	| Return(expr) -> print_string "return "; print_expr_semi expr
	| If(expr, stmt1, stmt2) -> print_string "if ("; print_expr expr; print_string ") "; print_stmt stmt1; print_string "else "; print_stmt stmt2
	| For(expr1, expr2, expr3, stmt) -> print_string "for ("; print_expr_semi expr1; print_expr_semi expr2; print_expr expr3; print_stmt stmt 
	| While(expr, stmt) -> print_string "while ("; print_expr_semi expr; print_string ")"; print_stmt stmt

let rec print_var_types = function
	Void -> print_string "void "
	| Int -> print_string "int "
	| String -> print_string "String " 
	| Boolean -> print_string "boolean "
	| Struct(s) -> Printf.printf "%s " (String.capitalize s.sname) 
	| Array(var_types, expr) ->
		print_var_types var_types;
		print_string "[";
		print_expr expr;
		print_string "] "
 
let print_param v =
	let (var_types, _) = v in match var_types with
	Variable(var_types, str) -> print_var_types var_types; print_string str
	(* if not a Variable we drop the unnecessary stuff *)
	| Variable_Initialization(var_types, str, _) -> print_var_types var_types; print_string str
	| Array_Initialization(var_types, str, _) -> print_var_types var_types; print_string str
	| Struct_Initialization(var_types, str, _) -> print_var_types var_types; print_string str

let rec print_var_decl  (v : Sast.variable_decl) =
	let (var_types, _) = v in match var_types with
		Variable(var_types, str) -> (match var_types with
			Struct(decl) ->
				let s = List.find (fun j -> j.original_struct = decl) j_struct_decl_list in
				print_string (String.capitalize s.sname); Printf.printf " %s = new %s();\n" str (String.capitalize s.sname)
			| _ -> print_var_types var_types; print_string (str ^ ";\n"))
		| Variable_Initialization(var_types, str, expr) -> print_var_types var_types; Printf.printf "%s = " str; print_expr_semi expr
		| Array_Initialization(var_types, str, expr_list) -> print_var_types var_types; Printf.printf "[] %s = { " str; print_expr_list_comma expr_list; print_string "};\n"
		| Struct_Initialization(var_types, str, expr_list) -> match var_types with
			Struct(decl) ->
				let s = List.find (fun j -> j.original_struct = decl) j_struct_decl_list in
				print_string (String.capitalize s.sname); Printf.printf " %s = new %s(" str (String.capitalize s.sname); print_expr_list_comma expr_list; print_string ");\n"
			| _ -> raise (Failure "shouldn't happen")

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
let rec print_function_params (v : Jast.j_var_struct_decl list) = match v with
	[] -> print_string "";
	| hd::[] -> print_param hd.the_variable;
	| hd::tl -> print_param hd.the_variable; print_string ", "; print_function_params tl

let print_asserts a_list =
	List.iter (
		fun (expr, stmt_list) ->
			print_string "if(!(";
			print_expr expr;
			print_string ")){\n";
			List.iter ( fun s ->
				print_stmt s; print_string "\n"
			) stmt_list;
			print_string "}\n"
	) a_list

let print_j_var_decl (dec : j_var_struct_decl) =
	print_var_decl dec.the_variable;
	if (List.length dec.asserts) <> 0 then
		(
			print_string("\npublic void set_" ^ dec.name ^ "(");
			print_param dec.the_variable;
			print_string "){\n";
			print_string ("this." ^ dec.name ^ "=" ^ dec.name ^ ";\n");
			print_asserts dec.asserts;
			print_string "}\n"
		)
	else ()

let print_constructors (name : string) (s : Jast.j_var_struct_decl list) =
	print_string ("public " ^ (String.capitalize name) ^ "(");
	print_function_params s;
	print_string "){\n";
	List.iter (
		fun dec -> print_string ("this." ^ dec.name ^ "=" ^ dec.name ^ ";\n")
	) s;
	print_string "\n}\n";
	(* Empty constructor*)
	print_string ("public " ^ (String.capitalize name) ^ "(){}\n")

let print_struct_decl (s : Jast.j_struct_decl) =
	print_string "static class ";
	print_string (String.capitalize s.sname);
	print_string " {\n\t\t";
	List.iter print_j_var_decl s.variable_decls;
	(* Make the constructors *)
	print_constructors s.sname s.variable_decls;
	print_string "\n}\n"
	
let print_unit_decl (u : Sast.unit_decl) = match u with
	Outer_udecl(str, udecl_params, udecl_check_val, true) -> print_string "if("; print_string (str.fname ^ "(");  print_expr_list_comma udecl_params; print_string ")==("; print_expr udecl_check_val; print_string ")) System.out.println(\"The test passes\"); \n"
	| Outer_udecl(str, udecl_params, udecl_check_val, false) -> print_string "if("; print_string (str.fname ^ "(");  print_expr_list_comma udecl_params; print_string ")==("; print_expr udecl_check_val; print_string ")) System.out.println(\"The test fails\");\n"
	| Local_udecl(udecl_params, udecl_check_val, false) -> print_string "local_inner_false:"
	| Local_udecl(udecl_params, udecl_check_val, true) -> print_string "local_inner_true:"

let rec print_param_list (p : Sast.variable_decl list) = match p with
	[] -> print_string "";
	| hd::[] -> print_param hd;
	| hd::tl -> print_param hd; print_string ", "; print_param_list tl

let print_func_decl (f : Sast.function_decl) =
	if f.fname = "main" then 
		(print_string "public static void main(String[] args) {\n";
		List.iter print_var_decl f.checked_locals;
		List.iter print_stmt f.checked_body;
		List.iter print_unit_decl f.checked_units; 
		print_string "}")
	else
		(
			print_string " static ";
			print_var_types f.ftype;
			print_string f.fname; 
			print_string "(";
			print_param_list f.checked_formals; 
			print_string ") {\n";
			List.iter print_var_decl f.checked_locals; 
			List.iter print_stmt f.checked_body;
			List.iter print_unit_decl f.checked_units; 
			print_string "}\n"
		)

let code_gen j =
	let _ = print_string "public class Program {\n\n\t" in
	let (structs, vars, funcs, unts) = j in
			List.iter print_struct_decl structs;
			List.iter print_var_decl vars;
			List.iter print_func_decl (List.rev funcs);
			List.iter print_unit_decl unts;
			print_string "\n}\n"

let _ =	
	code_gen jast