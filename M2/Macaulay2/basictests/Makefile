include ../../Makeconf

TESTRESULTS := $(patsubst %.m2, %.okay, $(wildcard *.m2))

all : $(TESTRESULTS)

$(TESTRESULTS) : ../bin/Macaulay2

%.okay : %.m2
	@ echo testing $<
	@ ../bin/Macaulay2 -silent $< '-e exit 0'
	@ touch $@

clean  :	
	rm -f *.okay

allfiles : *.m2 Makefile
	echoout $^ >allfiles
