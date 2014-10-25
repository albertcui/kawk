{ open Parser } (* Get the token types *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
  | "/*" { comment lexbuf } (* Comments *)
  | '(' { LPAREN }  | ')' { RPAREN } (* Punctuation *)
  | '{' { LBRACE }  | '}' { RBRACE }
  | '[' { LBRACK }  | ']' { RBRACK }
  | ';' { SEMI }    | ',' { COMMA }
  | '+' { PLUS }    | '-' { MINUS }
  | '*' { TIMES }   | '/' { DIVIDE }
  | '=' { ASSIGN } 
  | '<' { LT }      | '>' { GT }
  | "==" { EQ }     | "!=" { NEQ } 
  | "<=" { LEQ }    | ">=" { GEQ }
  | '|' { OR }      | '&' { AND } (* Short circuits *)
  | "@" { ASSERT }  | '.' { ACCESS }
  | "else" { ELSE } | "if" { IF } (* Keywords *)
  | "while" { WHILE } | "for" { FOR }
  | "return" { RETURN }
  | "struct" { STRUCT }
  | "this" { THIS } | "null" { NULL }
  | "bool" { BOOL } | "int" { INT } | "string" { STRING } 
  | '"'_*'"' as str { STRING_LITERAL(str) }  (* Strings *)
  | ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) } (* Integers *)
  | ["true" | "false"] as boolean { BOOL_LITERAL(bool_of_string boolean) }
  | eof { EOF } (* End-of-file *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | _ as char { raise (Failure("illegal character " ^
      Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf } (* End-of-comment *)
  | _ { comment lexbuf } (* Eat everything else *)
