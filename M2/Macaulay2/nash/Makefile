NASH = ./nash

# transparent gdb string
TGDB = env OLDSHELL=$(SHELL) SHELL=$(NASH) gdb
GDBARGS = -batch -nx -x gdb.commands --args
GDBSED = sed -e '/^$$/d' -e '/Program exited normally./d'


# strange result with this environment variable:
#  export i=foo

CFLAGS = -g -Wall
PROGS = nash printenv unsetenv args
all : nash
all : nash printenv unsetenv
check : diff1 diff2 diff3 diff4 test1 
test1 : nash;			$(NASH) -c "exec echo hi there"
clean :;			rm -f nash printenv unsetenv \
				  args.nash args.nonash args.gdb \
				  env.nash env.nonash env.gdb
ci :;				cvs ci -m ''

# bash always sets "_" to argv[0], so we simulate that when bash is out of the picture
ENV = env _=./printenv

# check whether the environment variables are undisturbed
diff1 : env.nonash env.nash;	diff -u $^
diff2 : env.nonash env.gdb;	diff -u $^
env.nonash : $(PROGS);		./printenv >$@
env.nash : $(PROGS);		$(ENV) $(NASH) -c "exec ./printenv" >$@
env.gdb : $(PROGS);		$(ENV) $(TGDB) $(GDBARGS) ./printenv |$(GDBSED) >$@

# check whether argv[0] is undisturbed
ENV2 = env _=./printenv OLDARG0=./args
diff3 : args.nonash args.nash;	diff -u $^
diff4 : args.nonash args.gdb;	diff -u $^
args.nonash : $(PROGS);		./args a 'b c' 2>$@
args.nash : $(PROGS);		$(ENV2) $(NASH) -c "exec ./args a 'b c'" 2>$@
args.gdb : $(PROGS);		$(ENV2) $(TGDB) $(GDBARGS) ./args a 'b c' 2>&1 |$(GDBSED) >$@
