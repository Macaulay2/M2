DOC=cache/doc
TEST=cache/tests

all :: initialize

all :: doc
doc :
	../bin/M2 -q '-e phase=2' StateTable.m2 '-e exit 0'
	../bin/M2 -q '-e phase=2' eliminate.m2  '-e exit 0'
	$(MAKE) -k -C $(DOC) -f ../../../m2/Makefile.egs

tests : doc
	$(MAKE) -k -C $(TEST) -f ../../../m2/Makefile.tests

initialize :: cache $(DOC) $(TEST)
clean :; rm -rf cache
cache $(DOC) $(TEST) :; mkdir $@

initialize :: $(DOC)/up.gif $(DOC)/next.gif $(DOC)/previous.gif $(DOC)/top.gif \
		$(DOC)/index.gif $(DOC)/null.gif 
$(DOC)/up.gif $(DOC)/next.gif $(DOC)/previous.gif $(DOC)/top.gif $(DOC)/index.gif $(DOC)/null.gif :
	ln -s ../../../html/$(shell basename $@) $(DOC)

