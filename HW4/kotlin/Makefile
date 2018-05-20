.PHONY: all compile run pack clean out

SOURCES=$(shell find src -type f -name "*.kt")
JARFILE=hw4.jar
ZIPNAME=hw4.zip

all: prepare-kotlin compile

run:
	java -jar ${JARFILE}

pack:
	zip ${ZIPNAME} -r Makefile src

clean:
	rm -rf ${ZIPNAME}
	rm -rf ${JARFILE}

compile: ${SOURCES}
	${KOTLINC} -cp src $^ -include-runtime -d ${JARFILE}

include kotlin.mk