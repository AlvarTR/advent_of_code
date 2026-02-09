
ADVENT_CODENAME:=app/Main

MAKEFILE_DIR:=$(dir $(realpath $(lastword $(MAKEFILE_LIST))))

all: clean run

OPT_LEVEL:=0
run: ${MAKEFILE_DIR}dist-newstyle
	HASKELL_EXE=$(shell cabal list-bin ${MAKEFILE_DIR}); \
	echo $$HASKELL_EXE; \
	$$HASKELL_EXE +RTS -p

clean:
	rm -rf ${MAKEFILE_DIR}dist-newstyle

${MAKEFILE_DIR}dist-newstyle: 
	cabal build --enable-profiling --profiling-detail=late
