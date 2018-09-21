~/src/M2-reallocate-heap-to-GC-heap/M2/BUILD/dan/builds.tmp/as-mth-indigo.local-reallocate-heap-to-GC-heap/
restart
needs "test-mem-leaks.m2"
getRSS()

-- on realloc branch
R = ZZ/101[a..d];
I = ideal random(R^1, R^{5:-2});
f = () -> (gb (ideal I_*);)
testF(10000,f) -- .29 ms per call, 2.9 sec total
testF(100000,f) -- .28 ms per call, no leak, 28.2 sec total

-- on realloc branch
R = ZZ[a..d];
I = ideal random(R^1, R^{5:-2});
f = () -> (gb (ideal I_*);)
getRSS()
testF(10000,f) -- 1.2 ms per call, 11.9 sec total
testF(100000,f) -- 1.2-1.3 ms per call, leak 194-215 bytes/call, 119 sec total

-- on realloc branch
R = QQ[a..d];
I = ideal random(R^1, R^{5:-2});
f = () -> (gb (ideal I_*);)
getRSS()
testF(10000,f) -- .85 ms per call, 8.5 sec total
testF(100000,f) -- .84 ms per call, no leak, 84.9 sec total

--------------------------------------------
-- on M2 1.12
R = ZZ/101[a..d];
I = ideal random(R^1, R^{5:-2});
f = () -> (gb (ideal I_*);)
testF(10000,f) -- .28 ms per call, 2.8 sec total
testF(100000,f) -- .27 ms per call, no leak, 27.4 sec total

-- on M2 1.12
R = ZZ[a..d];
I = ideal random(R^1, R^{5:-2});
f = () -> (gb (ideal I_*);)
getRSS()
testF(10000,f) -- .91 ms per call, 9.1 sec total
testF(100000,f) -- .89 ms per call, no leak, 88.8 sec total

-- on M2 1.12
R = QQ[a..d];
I = ideal random(R^1, R^{5:-2});
f = () -> (gb (ideal I_*);)
getRSS()
testF(10000,f) -- .59 ms per call, 5.9 sec total
testF(100000,f) -- .59 ms per call, no leak, 59.3 sec total
