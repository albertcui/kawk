default: pretty semantic

pretty: scanner parser ast pretty_printer
	ocamlc -o pretty parser.cmo scanner.cmo pretty_printer.cmo

semantic: scanner parser ast sast semantic_checker
	ocamlc -o semantic parser.cmo scanner.cmo semantic_checker.cmo

scanner: parser
	ocamllex scanner.mll; ocamlc -c scanner.ml

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

semantic_checker: sast
	ocamlc -c semantic_checker.ml

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
