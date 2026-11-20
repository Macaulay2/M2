# run "make clean && make" when updating the LanguageServer package version
# and/or the M2-language-server --help output, and then commit
# M2-language-server.1 to git

all: M2-language-server.1

M2-language-server.1: M2-language-server
	help2man -N -n "Macaulay2 language server" ./$< > $@

clean:
	rm -f M2-language-server.1

.PHONY: all clean
