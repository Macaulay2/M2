# for Windows NT
.SUFFIXES : .m2 .okay .out

!ifndef FILES
files : phase2 testresults egresults
    $(MAKE) -k FILES=1 -f phases.mak
testresults : ../tmp/Tests/*.m2
	echo TESTRESULTS=\>testresults
	ls ../tmp/Tests/*.m2 | sed -e s=/=\\=g -e s/.m2$$/.okay\\/ >>testresults
egresults : ../tmp/Examples/*.m2
	echo EGRESULTS=\>egresults
    ls ../tmp/Examples/*.m2 | sed -e s=/=\\=g -e s/.m2$$/.out\\/ >>egresults
!else
!include testresults

!include egresults

!endif
EXE = ..\bin\Macaulay2.exe
all : phase3 phase4
..\cache\Macaulay2.pre : $(EXE) *.m2 ../tutorial/final/*.out
	rm -f ../cache/Macaulay2.tmp
	$(EXE) -ephase=2 setup.m2 -eexit(0)
	mv ../cache/Macaulay2.tmp ../cache/Macaulay2.pre
..\cache\Macaulay2.doc : ..\cache\Macaulay2.pre ..\tmp\Examples\*.out
	rm -f ../cache/Macaulay2.tmp
	$(EXE) -ephase=4 setup.m2 -eexit(0)
	mv ../cache/Macaulay2.tmp ../cache/Macaulay2.doc
.m2.okay:
	@ echo testing $<
	@- $(EXE) --silent setup.m2 $< -eexit(0)
	@ echo okay >$@
.m2.out:
	@ echo running example $*.m2
	@ sed 1d <$*.m2 >tmp.in
	@- $(EXE) --silent -x -s -ephase=3 setup.m2 <tmp.in >tmp.out
	@ del tmp.in
	@ mv tmp.out $*.out
phase2 : ..\cache\Macaulay2.pre
phase3 : phase2 $(TESTRESULTS) $(EGRESULTS)
phase4 : phase3 ..\cache\Macaulay2.doc 

