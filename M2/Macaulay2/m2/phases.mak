# for Windows NT

MACAULAY = Macaul~1

EXE = ..\bin\$(MACAULAY).exe

all : phase2 tests phase3 phase4

..\cache\$(MACAULAY).tmp : $(EXE) *.m2 scriptde/*.m2 ../tutorial/final/*.out
	del $@
	$(EXE) -ephase=2 setup.m2 -eexit(0)

phase2 : ..\cache\$(MACAULAY).tmp

%.okay : %.m2
	@echo testing $<
	$(EXE) -silent setup.m2 $< -eexit(0)
	@echo okay >$@

TESTFILES := $(wildcard ../tmp/Tests/*.m2)
TESTRESULTS := $(patsubst %.m2, %.okay, $(TESTFILES))

tests : $(TESTRESULTS)

phase3 :

phase4 :



