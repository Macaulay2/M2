restart
needs "../test-mem-leaks.m2"
debug Core

R = RR_53
outM = mutableMatrix{{7_R}}; 
testF(1000000, () -> (entries raw outM;))

R = RR_1000
outM = mutableMatrix{{7_R}}; 
testF(1000000, () -> (entries raw outM;))

R = ZZ
outM = mutableMatrix{{7_R}}; 
testF(100000, () -> (entries raw outM;))

