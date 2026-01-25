
ADVENT_CODENAME:=safe_combinations

MAKEFILE_DIR:=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))

all: clean ${ADVENT_CODENAME}

DEBUG_LEVEL:=0
${ADVENT_CODENAME}: 
	echo $@ > ${MAKEFILE_DIR}.gitignore
	ghc -prof -fprof-auto -rtsopts -O${DEBUG_LEVEL} ${MAKEFILE_DIR}$@.hs && ${MAKEFILE_DIR}$@ +RTS -p

clean:
	rm -f ${MAKEFILE_DIR}${ADVENT_CODENAME}

	