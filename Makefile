default: scanner parser ast pretty
	ocamlc -o pretty parser.cmo scanner.cmo pretty_printer.cmo

scanner: parser
	ocamllex scanner.mll; ocamlc -c scanner.ml

parser: ast
	ocamlyacc parser.mly; ocamlc -c parser.mli; ocamlc -c parser.ml

ast:
	ocamlc -c ast.mli

pretty:
	ocamlc -c pretty_printer.ml

.PHONEY: clean

clean: 
	rm -f *.cmo *.cmi 
