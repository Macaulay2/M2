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
g' = prec ->( 
    M := mutableMatrix(RR_(first prec),20,10);
    fillMatrix(M);
    )
h = () -> (mutableMatrix M;)
i = () -> (A*B;)
j = () -> (Am*Bm;)
k = () -> (matrix Mm;) -- doesn't appear to leak

end
restart
needs "eigen-leaks.m2"

testF(1,f)
testF(100, f)
-- Anton's MacBook Pro
-- elapsed time = 21.1636
-- leaks 39321.6 bytes, takes 211.636 ms. (per call)
testF(100, f)
testF(100, f)
testF(100, f)
testF(100, f)
-- Anton's MacBook Pro
-- elapsed time = 20.7562
-- leaks 0 bytes, takes 207.562 ms. (per call)

testF(1000, g) 

testF(1000, g'_1000)
testF(1000, g'_100) 
testF(10000, g'_53) 

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