-- This tests whether various operations leak memory.  Currently,
-- you must look at the answers by hand, to see if any objects are
-- not being freed properly.

-- In each test below, the two calls to 'mem' should report the same
-- number of inuse objects.  The only valid
-- exception is if the garbage collector doesn't collect the computation
-- for some reason (although this only occasionally is the case).  Simply rerun the
-- CollectGarbage() a few times, until 'mem' actually removes the computation.

----------------------
-- Hermite computation
----------------------
check1 = () -> (m := matrix(Z^4, Z^11, (i,j) -> random 10000); gb m;)

CollectGarbage()
mem
scan(100, i -> check1())
CollectGarbage()
mem

----------------------
-- GB computation
----------------------
R = Z/32003[a..d]
R^1
kbasis(1,R)
check2 = () -> (m := randomMatrix(R^1, R^{-2,-2,-2}); gb m;)
check2 = () -> (m := randomMatrix(R^1, R^{-2,-2,-2});)

CollectGarbage()
mem
scan(100, i -> check2())
CollectGarbage()
mem

----------------------
-- res computation
----------------------

setup3 = (
    R = Z/32003[vars(0..17)];
    m1 = genericMatrix(R,0,3,3);
    m2 = genericMatrix(R,9,3,3);
    I = flatten(m1*m2-m2*m1))

check3 = (count) -> scan(count, i -> (m := matrix I; res m;))

setup3()
CollectGarbage()
mem
check3 10
CollectGarbage()
mem

----------------------
-- inhomogeneous GB computation
----------------------
R = Z/32003[a..d]
R^1                  -- this is placed here because the module R^1 is created
                     -- and stashed away.
check4 = () -> (m := matrix{a^2-1, a*d-b, c^2-c*d-1}; gb m;)

CollectGarbage()
mem
scan(100, i -> check4())
CollectGarbage()
mem

----------------------
-- random matrix construction
----------------------
R = Z/32003[a..d]
R^1
kbasis(1,R) -- kbasis creates an internal matrix, so we call this before 
            -- starting the test
check2 = () -> (m := randomMatrix(R^1, R^{-2,-2,-2});)

CollectGarbage()
mem
scan(100, i -> check2())
CollectGarbage()
mem

    -- ERROR: this seems to be leaking monomials...

----------------------
-- Hilbert function computation
----------------------
R = Z/32003[a..d]
m = borel matrix{c*d^3}
check5 = () -> (p := matrix m; poincare p;)

CollectGarbage()
mem
scan(100, i -> (check5();CollectGarbage();))
CollectGarbage()
mem

----------------------
-- Determinant computation
----------------------
R = Z/32003[vars(0..24)]
m = genericMatrix(R,0,5,5)
check5 = () -> (minors(2,m);)  -- leaks nothing
check5 = () -> (minors(3,m);)  -- leaks nothing

CollectGarbage()
mem
time scan(100, i -> (check5();CollectGarbage();))
CollectGarbage()
mem

----------------------
-- Pfaffian computation
----------------------
R = Z/32003[vars(0..24)]
m = genericSkew(R,0,7)
check5 = () -> (pfaffians(4,m);)  -- leaks nothing
check5 = () -> (pfaffians(5,m);)  -- leaks nothing
check5 = () -> (pfaffians(6,m);)  -- leaks nothing
check5 = () -> (pfaffians(7,m);)  -- leaks nothing

CollectGarbage()
mem
time scan(100, i -> (check5();CollectGarbage();))
CollectGarbage()
mem
