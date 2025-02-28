# Assignment 5 Makefile.

NAME = lab5
WARN = -w A-32-70

all: compile test

compile: clean
	ocamlfind ocamlc ${WARN} -c ${NAME}.mli ${NAME}.ml

test:
	ocamlfind ocamlc ${WARN} -o tests_${NAME} \
	  -package ounit2 -linkpkg \
	  ${NAME}.cmo tests_${NAME}.ml
	./tests_${NAME}

testc:
	ocamlfind ocamlc ${WARN} -o tests_${NAME}c \
	  -package ounit2 -linkpkg \
	  ${NAME}.cmo tests_${NAME}c.ml
	./tests_${NAME}c

clean:
	rm -f *.cm* *.log *.cache tests_${NAME} tests_${NAME}c
