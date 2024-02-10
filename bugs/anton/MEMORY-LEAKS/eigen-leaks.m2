needs "test-mem-leaks.m2"
getRSS()

KK = RR_1000;
A = random(KK^20,KK^10);
B = random(KK^10,KK^30);
Am = mutableMatrix A
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
    --setRandomSeed "a";
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
testF(1000, f)
  -- takes about 133 ms/call. probably not leaking... Mike MBP

testF(1000, g) 

testF(1000, g'_1000)
testF(1000, g'_100) 
testF(10000, g'_53) 


-- the following takes quite a bit of time, but doesn't leak?
testF(10000, i) -- (leaks 715 bytes/call, time 50.37 ms/call), 503 sec total
-- same on 1.12: (leaks 38 bytes/call, time 5.466 ms/call), 54.7 sec total.

------------------------
-- Tests on mike MBP ---
------------------------
-- This one calls SVD, so cannot be called on 1.12.
-- also probably doesn't leak
restart
needs "eigen-leaks.m2"
testF(1,f)
testF(100, f)
  -- (mike MBP): elapsed time = 13.2412
  -- (mike MBP): leaks 93306.9 bytes, takes 132.412 ms. (per call)
testF(1000, f)
  -- (mike MBP): elapsed time = 132.194
  -- (mike MBP): leaks 1318.91 bytes, takes 132.194 ms. (per call)

-- g': doesn't leak, but is extremely slow, compared to 1.12.
restart
needs "eigen-leaks.m2"
testF(10, g'_1000)
testF(10000, g'_1000)
  -- (Mike MBP) elapsed time = 17.1255
  -- (Mike MBP) leaks 101.581 bytes, takes 1.71255 ms. (per call)
  -- (Mike MBP battery) elapsed time = 1.35208
  -- (Mike MBP battery) leaks 24.1664 bytes, takes .135208 ms. (per call)

testF(10000, g'_100) 
  -- (Mike MBP) elapsed time = 3.34028
  -- (Mike MBP) leaks 80.6912 bytes, takes .334028 ms. (per call)
testF(10000, g'_53) 
  -- (Mike MBP) elapsed time = .362827
  -- (Mike MBP) leaks 20.48 bytes, takes .0362827 ms. (per call)
testF(1000000, g'_1000)

restart
needs "eigen-leaks.m2"
-- I think that 'h' does not leak.
testF(10, h) -- 
testF(10000, h) -- 
  -- (Mike MBP) elapsed time = 2.29537
  -- (Mike MBP) leaks 2579.25 bytes, takes .229537 ms. (per call)
  -- (Mike MBP 1.12) elapsed time = 1.55341
  -- (Mike MBP 1.12) leaks 8.192 bytes, takes .155341 ms. (per call)
testF(10000, h) -- 
testF(10000, h) -- 
testF(10000, h) -- 
testF(100000, h) -- 
  -- (Mike MBP) elapsed time = 22.2329
  -- (Mike MBP) leaks 94.5357 bytes, takes .222329 ms. (per call)
testF(1000000, h) -- 
  -- (Mike MBP) elapsed time = 212.086
  -- (Mike MBP) leaks 8.75315 bytes, takes .212086 ms. (per call)
  -- (Mike MBP 1.12): elapsed time = 187.02
  -- (Mike MBP 1.12): leaks .004096 bytes, takes .18702 ms. (per call)

restart
needs "eigen-leaks.m2"
testF(10, i)
testF(5000, i)
  -- (mike MBP): elapsed time = 404.446
  -- (mike MBP): leaks 1.6384 bytes, takes 80.8892 ms. (per call)
  -- (mike MBP 1.12) elapsed time = 26.8642
  -- (mike MBP 1.12) leaks 64.7168 bytes, takes 5.37284 ms. (per call)

-- (might really leak)
restart
needs "eigen-leaks.m2"
testF(10, j)
testF(5000, j)
  -- (mike MBP): elapsed time = 10.0919
  -- (mike MBP): leaks 5283.02 bytes, takes 2.01838 ms. (per call)
  -- (mike MBP 1.12): elapsed time = 10.1912
  -- (mike MBP 1.12): leaks 111476 bytes, takes 2.03823 ms. (per call)

-- new one is really slow currently:
restart
needs "eigen-leaks.m2"
testF(10, k)  
testF(100000, k)
  -- (mike MBP): elapsed time = 320.27
  -- (mike MBP): leaks 1.59744 bytes, takes 3.2027 ms. (per call)
  -- (mike MBP 1.12): elapsed time = 13.8928
  -- (mike MBP 1.12): leaks 3.4816 bytes, takes .138928 ms. (per call)
