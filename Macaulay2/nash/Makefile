
# transparent gdb string
GDB = gdb -batch -nx -x gdb.commands --args

# strange result with this environment variable:
export i=foo

CFLAGS = -g -Wall
PROGS = nash printenv unsetenv
all : nash
ENV = env _=`type -p printenv` SHELL=./nash
ENV = env _=./printenv SHELL=./nash
ENV = ./unsetenv _ env SHELL=./nash
all : nash printenv unsetenv
check : diff1 diff2 test1 
diff1 : env.nonash env.nash;	diff -u $^
diff2 : env.nonash env.gdb;	diff -u $^
test1 : nash;			./nash -c "exec echo hi there"
clean :;			rm -f env.nonash env.nash nash printenv unsetenv
ci :;				cvs ci -m ''

env.nonash : $(PROGS);		$(ENV) ./printenv >$@
env.nash : $(PROGS);		$(ENV) ./nash -c "exec ./printenv" >$@
env.gdb : $(PROGS);		$(ENV) $(GDB) ./printenv |sed -e '/^$$/d' -e '/Program exited normally./d' >$@
