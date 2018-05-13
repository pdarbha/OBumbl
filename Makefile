repl:
	ocamlbuild -pkg netclient -pkg netstring -pkg yojson -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean

test:
		ocamlbuild -pkg netclient -pkg netstring -pkg yojson -use-ocamlfind test.byte && ./test.byte

gui:
		ocamlbuild -pkg netclient -pkg netstring -pkg yojson -use-ocamlfind gui.byte && ./gui.byte
