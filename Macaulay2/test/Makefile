include ../../Makeconf

TESTRESULTS2 := $(patsubst %.m2, %.okay, $(wildcard *.m2)) dumpdata.okay

all : $(TESTRESULTS2)

$(TESTRESULTS2) : ../bin/Macaulay2 ../cache/Macaulay2-$(ARCH).data ../cache/Macaulay2.doc

dumpdata.okay : dumpdata.1 dumpdata.2 dumpdata.3
	@ echo testing dumpdata
	@ ../bin/Macaulay2 '-eloaddata("../cache/Macaulay2-$(ARCH).data")' -- '-epath=append(path,"../m2")' '-erunStartFunctions()' '-eerrorDepth(0)' -silent dumpdata.1 '-eexit(0)'
	@ echo exit 0 | ../bin/Macaulay2 '-eloaddata("../cache/Macaulay2-$(ARCH).data")' -- '-epath=append(path,"../m2")' '-erunStartFunctions()' '-eerrorDepth(0)' -silent dumpdata.2 -- dumpdata.3
	@ touch dumpdata.okay

%.okay : %.m2
	@ echo testing $<
	@ ../bin/Macaulay2 '-eloaddata("../cache/Macaulay2-$(ARCH).data")' -- '-epath=append(path,"../m2")' '-erunStartFunctions()' '-eerrorDepth(0)' -silent $< '-eexit(0)'
	@ touch $@

clean  :	
	rm -f *.okay allfiles

allfiles : *.m2 Makefile COPYRIGHT dumpdata.1 dumpdata.2 dumpdata.3
	echo $^ |tr ' ' \\012 >allfiles
