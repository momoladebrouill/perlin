
all:
	dune build
	./_build/default/main.exe
	kitten icat result.png
