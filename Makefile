repl:
	ocamlbuild -pkg netclient -pkg netstring -pkg yojson -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean