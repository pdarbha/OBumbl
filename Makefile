repl:
	ocamlbuild -pkg netclient -pkg netstring -pkg yojson -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean

test:
		ocamlbuild -use-ocamlfind test.byte && ./test.byte

gui:
	ocamlbuild -use-ocamlfind gui.byte && ./gui.byte
