repl:
	ocamlbuild -pkg netclient -pkg netstring -pkg yojson -use-ocamlfind main.byte && ./main.byte REPL

clean:
	ocamlbuild -clean

test:
		ocamlbuild -pkg netclient -pkg netstring -use-ocamlfind test.byte && ./test.byte

gui:
		ocamlbuild -pkg netclient -pkg netstring -pkg yojson -use-ocamlfind main.byte && ./main.byte GUI
