needs "test-mem-leaks.m2"
testF(10000,()->(monoid[a..d];))
testF(10000,()->(monoid[a..d,Degrees=>{{1,0},{1,2},{2,3},{0,5}}];))
