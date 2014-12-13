default: pretty semantic sast_jast

sast_jast: scanner parser semantic sast_to_jast
	ocamlc -o scanner.cmo parser.cmo semantic_checker.cmo sast_to_jast.cmo 	

pretty: scanner parser pretty_printer
	ocamlc -o pretty parser.cmo scanner.cmo pretty_printer.cmo

semantic: scanner parser semantic_checker
	ocamlc -o semantic parser.cmo scanner.cmo semantic_checker.cmo

scanner: parser
	ocamllex scanner.mll; ocamlc -c scanner.ml

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

sast_to_jast: jast 
	ocamlc -c sast_to_jast.ml 

semantic_checker: sast
	ocamlc -c semantic_checker.ml

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
	rm -f semantic pretty *.cmo *.cmi *~ parser.mli parser.ml scanner.ml
