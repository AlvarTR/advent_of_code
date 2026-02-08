
ADVENT_CODENAME:=forklift_rolls

MAKEFILE_DIR:=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))

all: build run

OPT_LEVEL:=0
build: 
	echo ${ADVENT_CODENAME} > ${MAKEFILE_DIR}.gitignore
	ghc -prof -fprof-auto -rtsopts -O${OPT_LEVEL} ${MAKEFILE_DIR}${ADVENT_CODENAME}.hs 
	
run:
	${MAKEFILE_DIR}${ADVENT_CODENAME} +RTS -p

clean:
	rm -f ${MAKEFILE_DIR}${ADVENT_CODENAME}

	