doc : cache-tmp/Examples
	../bin/M2 '-ephase=2' StateTable.m2 '-eexit(0)'
	$(MAKE) -k -C cache-tmp/Examples -f ../../../m2/Makefile.egs

clean :
	rm -rf cache-tmp

cache-tmp/Examples : cache-tmp
	mkdir $@

cache-tmp :
	mkdir $@