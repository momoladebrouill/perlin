
all:
	dune build --profile release
	./_build/default/main.exe
	kitten icat result.png
