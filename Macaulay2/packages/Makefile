all :: initialize

all :: doc
doc :
	../bin/M2 -q '-e phase=2' StateTable.m2 '-e exit 0'
	../bin/M2 -q '-e phase=2' eliminate.m2  '-e exit 0'
	$(MAKE) -k -C cache/doc -f ../../../m2/Makefile.egs

tests : doc
	$(MAKE) -k -C cache/tests -f ../../../m2/Makefile.tests

initialize : cache cache/doc cache/tests
clean :; rm -rf cache
cache cache/doc cache/tests :; mkdir $@
