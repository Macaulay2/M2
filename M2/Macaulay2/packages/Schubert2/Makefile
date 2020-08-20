M = ~/src/M2/1.2/BUILD/dan/builds/ubuntu32/M2
STOP = --stop
demo :
	$M $(STOP) --no-debug -e 'input "demo.m2"; exit 0' </dev/null
	$M $(STOP) --no-debug -e 'input "demo2.m2"; exit 0' </dev/null
	$M $(STOP) --no-debug -e 'input "demo3.m2"; exit 0' </dev/null
	$M $(STOP) --no-debug -e 'input "test-dan.m2"; exit 0' </dev/null
#	$M $(STOP) --no-debug -e 'input "test-mike.m2"; exit 0' </dev/null

download :
	wget -rLN http://www.math.sunysb.edu/~sorin/online-docs/schubert/schubertmanual.txt
	wget -rLN http://rasmus.uib.no/People/nmasr/schubert/ALPHA/schubert-0.996easter.tar.gz
#	wget http://math.uib.no/schubert/0.996/schubert-0.996.tar.gz
#	wget http://math.uib.no/schubert/0.992/schubert/schubertmanual
