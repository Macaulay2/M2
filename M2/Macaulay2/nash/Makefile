CFLAGS = -g -Wall
ENV = env _=./printenv SHELL=./nash
all : nash
ENV = env _=`type -p printenv` SHELL=./nash
all : nash printenv unsetenv
check : diff1 diff2 test1 
diff1 : env.nonash env.nash;	diff -u $^
diff2 : env.nonash env.gdb;	diff -u $^
env.nonash : nash;		$(ENV) ./printenv >$@
env.nash : nash;		$(ENV) ./nash -c "exec ./printenv" >$@
env.gdb : nash printenv;	gdb -batch -nx -x gdb.commands --args $(ENV) ./printenv |sed -e '/^$$/d' -e '/Program exited normally./d' >$@
test1 : nash;			./nash -c "exec echo hi there"
clean :;			rm -f env.nonash env.nash nash
ci :;				cvs ci -m ''
