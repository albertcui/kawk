default: code_gener pretty semantic sast_jast

code_gener: scanner parser semantic sast_to_jast code_gen
	ocamlc -o code_gen scanner.cmo parser.cmo semantic_checker.cmo sast_to_jast.cmo code_gen.cmo

sast_jast: scanner parser semantic sast_to_jast
	ocamlc -o sast_to_jast scanner.cmo parser.cmo semantic_checker.cmo sast_to_jast.cmo 	

pretty: scanner parser pretty_printer
	ocamlc -o pretty parser.cmo scanner.cmo pretty_printer.cmo

code_gen: jast
	ocamlc -c code_gen.ml

semantic: scanner parser semantic_checker
	ocamlc -o semantic parser.cmo scanner.cmo semantic_checker.cmo

sast_to_jast: jast
	ocamlc -c sast_to_jast.ml 

semantic_checker: sast scanner
	ocamlc -c semantic_checker.ml

scanner: parser
	ocamllex scanner.mll; ocamlc -c scanner.ml

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

jast: sast ast
	ocamlc -c jast.mli

sast: ast
	ocamlc -c sast.mli

ast:
	ocamlc -c ast.mli

pretty_printer:
	ocamlc -c pretty_printer.ml

.PHONEY: clean

clean: 
	rm -f test/output*.k
	rm -f code_gen semantic pretty sast_to_jast *.cmo *.cmi *~ parser.mli parser.ml scanner.ml
