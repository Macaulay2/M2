TOPDIR = ../..
include $(TOPDIR)/Makeconf

RESULTS := $(patsubst %.m2, %.okay, $(wildcard *.m2)) 
ifdef DUMPDATA
RESULTS := $(RESULTS) dumpdata.okay
endif

all :: $(RESULTS)

$(RESULTS) : ../bin/Macaulay2 ../cache/Macaulay2.doc ../cache/Macaulay2.made

ifdef DUMPDATA
dumpdata.okay : dumpdata.1 dumpdata.2 dumpdata.3
	@ echo testing dumpdata
	@ ../bin/Macaulay2 -q '-eloaddata("../cache/Macaulay2-$(ARCH).data")' -- '-epath=append(path,"../m2")' '-erunStartFunctions()' '-eerrorDepth(0)' -silent dumpdata.1 '-eexit(0)'
	@ echo exit 0 | ../bin/Macaulay2 -q '-eloaddata("../cache/Macaulay2-$(ARCH).data")' -- '-epath=append(path,"../m2")' '-erunStartFunctions()' '-eerrorDepth(0)' -silent dumpdata.2 -- dumpdata.3
	@ touch dumpdata.okay
endif

ifdef DUMPDATA
ARGS := -q '-eloaddata("../cache/Macaulay2-$(ARCH).data")' -- '-epath=append(path,"../m2")' '-erunStartFunctions()' '-eerrorDepth(0)' -silent
else
ARGS := -q -ephase=1 ../m2/setup.m2 -ephase=0 '-erunStartFunctions()' '-eerrorDepth(0)' -silent
endif

%.okay : %.m2
	@ echo testing $<
	../bin/Macaulay2 $(ARGS) $< '-eexit(0)'
	@ touch $@

clean  :	
	rm -f *.okay allfiles

allfiles : *.m2 Makefile COPYRIGHT dumpdata.1 dumpdata.2 dumpdata.3
	echo $^ |tr ' ' \\012 >allfiles

docStructure.out : docStructure.m2
