# we set this to agree
SHELL = /bin/sh
export SHELL

NASH = ./nash

# transparent gdb string
TGDB = env OLDSHELL=$(SHELL) SHELL=$(NASH) gdb
GDBARGS = -batch -nx -x gdb.commands --args
GDBSED = sed -e '/^$$/d' -e '/Program exited normally./d'


# strange result with this environment variable:
#  export i=foo

CFLAGS = -g -Wall
LDFLAGS = -L.
LDLIBS = -lenv
PROGS = nash printenv unsetenv args
all : nash
all : nash printenv unsetenv
nash unsetenv : libenv.a
check : diff1 diff2 diff3 diff4 test1 
test1 : nash;			$(NASH) -c "exec echo hi there"
clean :;			rm -f nash printenv unsetenv \
				  args.nash args.nonash args.gdb \
				  env.nash env.nonash env.gdb setenv3.o \
				  unsetenv3.o libenv.a
ci :;				cvs ci -m ''

# libenv
libenv.a : unsetenv3.o setenv3.o ; ar ru $@ $? ; ranlib $@

# bash always sets "_" to argv[0], so we simulate that when bash is out of the picture
export _ = ./printenv
# ENV = env _=./printenv
# ENV = env

# someone sets LINES to 24, so we do, too
export LINES = 24

# check whether the environment variables are undisturbed
diff1 : env.nonash env.nash;	diff $^ || true
diff2 : env.nonash env.gdb;	diff $^ || true
env.gdb env.nash env.nonash : $(PROGS)
	$(ENV) ./printenv >env.nonash ; $(ENV) $(NASH) -c "exec ./printenv" >env.nash ; $(ENV) $(TGDB) $(GDBARGS) ./printenv |$(GDBSED) >env.gdb

# check whether argv[0] is undisturbed
ENV2 = env _=./printenv OLDARG0=./args
diff3 : args.nonash args.nash;	diff $^ || true
diff4 : args.nonash args.gdb;	diff $^ || true
args.nonash : $(PROGS);		./args a 'b c' 2>$@
args.nash : $(PROGS);		$(ENV2) $(NASH) -c "exec ./args a 'b c'" 2>$@
args.gdb : $(PROGS);		$(ENV2) $(TGDB) $(GDBARGS) ./args a 'b c' 2>&1 |$(GDBSED) >$@
