
all:
	dune build --profile=default
	./_build/default/main.exe
	kitten icat result.png
