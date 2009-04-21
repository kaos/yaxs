
#.PHONY: test

all:
	erl -make

test: all
	cd test ; ./test.escript
