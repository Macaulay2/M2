
# transparent gdb string
TGDB = env OLDSHELL=$(SHELL) SHELL=$(NASH) gdb
GDBARGS = -batch -nx -x gdb.commands --args
GDBSED = sed -e '/^$$/d' -e '/Program exited normally./d'

NASH = ./nash

# strange result with this environment variable:
export i=foo

CFLAGS = -g -Wall
PROGS = nash printenv unsetenv
all : nash
all : nash printenv unsetenv
check : diff1 diff2 test1 
diff1 : env.nonash env.nash;	diff -u $^
diff2 : env.nonash env.gdb;	diff -u $^
test1 : nash;			$(NASH) -c "exec echo hi there"
clean :;			rm -f env.nonash env.nash nash printenv unsetenv
ci :;				cvs ci -m ''

# bash always sets "_" to argv[0], so we simulate that when bash is out of the picture
ENV = env _=./printenv

env.nonash : $(PROGS);		./printenv >$@
env.nash : $(PROGS);		$(ENV) $(NASH) -c "exec ./printenv" >$@
env.gdb : $(PROGS);		$(ENV) $(TGDB) $(GDBARGS) ./printenv |$(GDBSED) >$@
