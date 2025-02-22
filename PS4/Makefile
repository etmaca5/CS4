# Assignment 4 Makefile.

NAME = lab4
WARN = -w A-32-70

all: compile test

compile: clean
	ocamlfind ocamlc ${WARN} -g -c -package num ${NAME}.mli ${NAME}.ml

test:
	ocamlfind ocamlc ${WARN} -g -o tests_${NAME} \
	  -package ounit2,num -linkpkg \
	  ${NAME}.cmo tests_${NAME}.ml
	./tests_${NAME}

clean:
	rm -f *.cm* *.log *.cache tests_${NAME}
