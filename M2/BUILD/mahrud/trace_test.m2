-- Testing stack trace functionality under -DPROFILING=ON

debug Core

g=method()

f=a->(
    print "f before"; 
    stacktrace();
    print "f after"; 
    )
g ZZ :=a->(print "g"; f())
h=a->(print "h"; g(2))
i=a->(print "i"; h())
j=a->(print "j"; i())
k=a->(print "k"; j())

end--

restart
load("trace_test.m2")

k()

