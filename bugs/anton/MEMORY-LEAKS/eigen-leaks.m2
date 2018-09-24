restart
needs "test-mem-leaks.m2"
getRSS()

KK = RR_1000;
A = random(KK^20,KK^10);
B = random(KK^10,KK^30);
Am = mutableMatrix A;
Bm = mutableMatrix B;
M = A * B;
Mm = Am * Bm;
getRSS()

f = () -> (SVD M;)
g = () -> (random(KK^20, KK^10);)
h = () -> (mutableMatrix M;)
i = () -> (A*B;)
j = () -> (Am*Bm;)
k = () -> (matrix Mm;) -- doesn't appear to leak

testF(1000, f)
testF(1000, f) -- no leak
testF(100000, g) -- leaks like a sieve, maybe about 29k per call...
testF(10000, h) -- 
testF(10000, h) -- 
testF(10000, h) -- 
testF(10000, h) -- 
testF(100000, h) -- 

-- the following takes quite a bit of time, but doesn't leak?
testF(10000, i) -- (leaks 715 bytes/call, time 50.37 ms/call), 503 sec total
-- same on 1.12: (leaks 38 bytes/call, time 5.466 ms/call), 54.7 sec total.

testF(10000, j) 
  -- realloc: (leaks 2.7k bytes/call, time 2 ms/call),  20.1 sec total
  -- 1.12: (leaks 113k bytes/call, time 2 ms/call),  20.5 sec total
  
testF(1000, k)  
testF(10000, k)
  -- realloc: no leak, 3.1 ms/call.  total: 31.4 sec
  -- 1.12: no leak, .13 ms/call, total: 1.32 sec