
all ::; $(MAKE) -f Makefile.secondary all
clean ::; $(MAKE) -f Makefile.secondary NODEPENDS=1 clean
m2.exe ::; $(MAKE) -f Makefile.secondary m2.exe
tar ::; $(MAKE) -f Makefile.secondary NODEPENDS=1 tar
TAGS :: Makefile.secondary; $(MAKE) -f Makefile.secondary NODEPENDS=1 TAGS
allfiles :: Makefile.secondary; $(MAKE) -f Makefile.secondary NODEPENDS=1 allfiles
e.tgz ::; tar czf e.tgz `cat allfiles`
wc ::; wc -l *.hpp *.cpp 

