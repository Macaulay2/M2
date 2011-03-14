-- Clean up the implementation of Gauss elim and hermite normal forms, and add in
-- an implementation of smith normal form.

-- example:
M = matrix{
     {-3770, 12810, 8577, -1293}, 
     {3328, -676, 12841, -1079}, 
     {8967, -5504, 14686, -1548}, 
     {-11147, -9238, 4105, 6260}, 
     {2104, -1220, -5870, -15245}, 
     {3, -9490, -14043, 5568}, 
     {-9986, -1617, 1110, 9669}}
M' = mutableMatrix M
debug Core
rawFFLU raw M'
M'
N = matrix reverse entries M
loadPackage "MatrixNormalForms"
hermiteNF N
M'' = matrix reverse entries matrix M'
hermiteNF M''

-- Times: on my MBP, 10.5.8 macosx
setRandomSeed 8374238947238234
kk = ZZ/32003

M = random(kk^100, kk^50)
time gens gb M;  -- .022 sec
time (P,L,U) = LUdecomposition M;

------------------------------------------------------
setRandomSeed 8374238947238234
kk = ZZ/32003
M0 = random(kk^1000, kk^500);
M = M0;
time gens gb M;  -- 19.31 sec
setRandomSeed 8374238947238234
M = random(kk^1000, kk^500);
M = M ** kk[];
time gens gb M;  -- 14.1 sec
setRandomSeed 8374238947238234
M = random(kk^1000, kk^500);
M = M ** kk[];
time gens gb(M, Algorithm=>LinearAlgebra);  -- 3.26 sec  -- CRASH!!
time (P,L,U) = LUdecomposition M0; -- 2.12 sec 
------------------------------------------------------
restart
setRandomSeed 8374238947238234
kk = ZZ/32003
time M0 = random(kk^3000, kk^2000);
M = M0;
time gens gb M;  --  sec
setRandomSeed 8374238947238234
M = random(kk^3000, kk^2000);
M = M ** kk[];
time gens gb M;  --  sec
setRandomSeed 8374238947238234
M = random(kk^3000, kk^2000);
M = M ** kk[];
time gens gb(M, Algorithm=>LinearAlgebra);  -- 122.48  sec
time (P,L,U) = LUdecomposition M0; -- 109.4 sec
debug Core
N = mutableMatrix M0;
time rawFFLU raw N; -- 84.95  sec

------------------------------------------------------
setRandomSeed 8374238947238234
kk = GF(9)
M = random(kk^1000, kk^500);
time gens gb M;  -- 28.09 sec

setRandomSeed 8374238947238234
kk = GF(9)
M = random(kk^1000, kk^500);
M = M ** kk[];
time gens gb M;  -- 20.65 sec
------------------------------------------------------
setRandomSeed 8374238947238234
kk = QQ
M0 = random(ZZ^200, ZZ^100);
M = M0 ** QQ;
time gens gb M;  -- 6.7 sec
M = M0 ** QQ[];
time gens gb M;  -- 2.28 sec
loadPackage "MatrixNormalForms"
time hermiteNF M0; -- 18.7 sec
N = mutableMatrix M0;
debug Core
time rawFFLU raw N

------------------------------------------------------
setRandomSeed 8374238947238234
kk = QQ
M0 = random(ZZ^400, ZZ^200);
M = M0 ** QQ;
time gens gb M;  -- 101.76 sec
M = M0 ** QQ[];
time gens gb M;  -- 34.9 sec
debug Core
N = mutableMatrix M0;
time rawFFLU raw N; -- 7.96 sec
------------------------------------------------------
-- some sparse matrices
------------------------------------------------------
setRandomSeed 8374238947238234
time M0 = matrix randomMutableMatrix(3000,1500,.9,100); -- VERY SLOW -- 38.45 sec
time M = M0 ** (ZZ/32003);
time gens gb M;  -- > 141 sec
time M = M0 ** (ZZ/32003[]);
time gens gb M;  -- > 300 sec

------------------------------------------------------
-- Hermite normal forms
------------------------------------------------------
-- Over ZZ
restart
needsPackage "MatrixNormalForms"
setRandomSeed 8374238947238234
(n,m) = (20,15)
M0 = random(ZZ^n, ZZ^m)
time gens gb M0
time hermiteNF(M0, ChangeMatrix => false)
M = M0 ** ZZ[];
time gens gb M;

restart
needsPackage "MatrixNormalForms"
setRandomSeed 8374238947238234
(n,m) = (40,30)
M0 = random(ZZ^n, ZZ^m);
time gens gb M0;
time hermiteNF(M0, ChangeMatrix => false);
M = M0 ** ZZ[];
time gens gb M;

restart
needsPackage "MatrixNormalForms"
setRandomSeed 8374238947238234
(n,m) = (80,60)
M0 = random(ZZ^n, ZZ^m);
time gens gb M0; -- 51.9 sec 
time hermiteNF(M0, ChangeMatrix => false); -- 2.63 sec
M = M0 ** ZZ[];
time gens gb M; -- 108.38 sec
