# Assignment 6 Makefile.

NAME = lab6
WARN = -w A-32-70

all: compile test

compile: clean
	ocamlfind ocamlc ${WARN} -c ${NAME}.mli ${NAME}.ml

test:
	ocamlfind ocamlc ${WARN} -o tests_${NAME} \
	  -package ounit2 -linkpkg \
	  ${NAME}.cmo tests_${NAME}.ml
	./tests_${NAME}

clean:
	rm -f *.cm* *.log *.cache tests_${NAME}
