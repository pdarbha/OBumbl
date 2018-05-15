OBumbl - Tinder (Bumble) for CS partners.

Installation: Several packages are necessary for running this project: Yojson, ocamlnet, graphics and camlimages.
Also install XQuartz software on your local machine.
  - opam install yojson
  - opam intstall ocamlnet
  - opam install graphics
  - opam install camlimages
  - opam switch --reinstall 4.06.0

Usage: Run XQuartz. Enter the following terminal commands:
  - export DISPLAY=:0.0
  - make gui
(Internet access required.)

Make (other):
  - make test
  - make repl
  - make clean
