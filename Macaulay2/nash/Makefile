CFLAGS = -g -Wall
ENV = env _=./printenv SHELL=./nash
PROGS = nash printenv unsetenv
all : nash
ENV = env _=`type -p printenv` SHELL=./nash
ENV = ./unsetenv _
all : nash printenv unsetenv
check : diff1 diff2 test1 
diff1 : env.nonash env.nash;	diff -u $^
diff2 : env.nonash env.gdb;	diff -u $^
test1 : nash;			./nash -c "exec echo hi there"
clean :;			rm -f env.nonash env.nash nash printenv unsetenv
ci :;				cvs ci -m ''

env.nonash : $(PROGS);		$(ENV) ./printenv >$@
env.nash : $(PROGS);		$(ENV) ./nash -c "exec ./printenv" >$@
env.gdb : $(PROGS);		$(ENV) gdb -batch -nx -x gdb.commands --args ./printenv |sed -e '/^$$/d' -e '/Program exited normally./d' >$@
