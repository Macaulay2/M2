CFLAGS = -g -Wall

all : nash

check : diffs test1
diffs : env.nonash env.nash
	diff -u $^
env.nonash : nash
	printenv >$@
env.nash : nash
	env _=`type -p printenv` ./nash -c "exec printenv" >$@
test1 : nash
	./nash -c "exec echo hi there"
clean :
	rm -f env.nonash env.nash nash
ci :
	cvs ci -m ''

