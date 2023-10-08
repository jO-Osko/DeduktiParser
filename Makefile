.PHONY: default release clean

default: format
	dune build

format:
	dune build @fmt --auto-promote

release: format
	dune build --profile release

utop: format
	dune utop

clean:
	dune clean

run: default
	./solution.exe $(DAY) $(YEAR)