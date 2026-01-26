
ADVENT_CODENAME:=invalid_ids

MAKEFILE_DIR:=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))

all: clean ${ADVENT_CODENAME}

OPT_LEVEL:=0
${ADVENT_CODENAME}: 
	echo $@ > ${MAKEFILE_DIR}.gitignore
	ghc -fprof-auto -rtsopts -O${OPT_LEVEL} ${MAKEFILE_DIR}$@.hs && ${MAKEFILE_DIR}$@ -p

clean:
	rm -f ${MAKEFILE_DIR}${ADVENT_CODENAME}

	