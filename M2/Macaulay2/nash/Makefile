
# transparent gdb string
GDB = gdb -batch -nx -x gdb.commands --args

NASH = ./nash

# strange result with this environment variable:
export i=foo

CFLAGS = -g -Wall
PROGS = nash printenv unsetenv
all : nash
ENV = env _=`type -p printenv` SHELL=$(NASH)
ENV = env _=./printenv SHELL=$(NASH)
ENV = ./unsetenv _ env SHELL=$(NASH)
all : nash printenv unsetenv
check : diff1 diff2 test1 
diff1 : env.nonash env.nash;	diff -u $^
diff2 : env.nonash env.gdb;	diff -u $^
test1 : nash;			$(NASH) -c "exec echo hi there"
clean :;			rm -f env.nonash env.nash nash printenv unsetenv
ci :;				cvs ci -m ''

env.nonash : $(PROGS);		$(ENV) ./printenv >$@
env.nash : $(PROGS);		$(ENV) $(NASH) -c "exec ./printenv" >$@
env.gdb : $(PROGS);		$(ENV) $(GDB) ./printenv |sed -e '/^$$/d' -e '/Program exited normally./d' >$@
