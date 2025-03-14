DEST  = ./_build/default
TEST  = klotski_tests
SOLVE = klotski_solve
INTER = klotski_interact

build_test:
	dune build ${TEST}.exe
	mv ${DEST}/${TEST}.exe ${TEST}
	chmod +w ${TEST}

test: build_test
	./${TEST}

repl:
	utop -init ./utop_init

solve:
	dune build ${SOLVE}.exe
	mv ${DEST}/${SOLVE}.exe ${SOLVE}
	chmod +w ${SOLVE}

interact:
	dune build ${INTER}.exe
	mv ${DEST}/${INTER}.exe ${INTER}
	chmod +w ${INTER}

all: build_test solve interact

clean:
	dune clean
	rm -f ${TEST} ${SOLVE} ${INTER}

