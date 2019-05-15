.PHONY: all install clean

all:
	dune build

install:
	dune install

clean:
	dune clean
