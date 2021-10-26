restart
debug Core
kk = ZZp(32003, Strategy=>"Ffpack")
kk1 = ZZp(32003, Strategy=>"Flint")
kk2 = ZZp(32003, Strategy=>"Aring")
raw kk
raw kk1
raw kk2

restart
kk = ZZ/101
M = mutableMatrix(ZZ, 2, 2)
M_(1,1) = 4
assert(M != 0)
assert(matrix promote(M, kk) == promote(matrix M, kk)) -- false
-- the following should not be 0!
assert(promote(M,ZZ,kk) != 0)

-- lift has the same problem
M1 = mutableMatrix(kk, 2, 2)
M1_(1,1) = 4
assert(M1 != 0)
assert(matrix lift(M1, ZZ) == lift(matrix M1, ZZ)) -- false
-- the following should not be 0!
assert(lift(M1,kk,ZZ) != 0)


fillMatrix M
elapsedTime rank M


MZZ = lift(M,ZZ); -- BUG...
M1 = promote(MZZ, kk1);

MZZ = mutableMatrix(ZZ, 500, 600)
fillMatrix(MZZ, Height=>30000, Density => .1)

promote(MZZ, kk1)

k1 = ZZ/32003
M = mutableMatrix(k1, 2, 2)
M_(1,1) = 13_k1
M
lift(M,ZZ) -- WRONG
mutableMatrix lift(matrix M, ZZ)

options random
random(ZZ^3, ZZ^4, Height=>32000)
M = random(ZZ^500, ZZ^500, Height=>32000);
time mutableMatrix M;
time M1 = mutableMatrix promote(M,kk1);

time rank M1

-----
restart
debug Core
kk = ZZp(32003, Strategy=>"Ffpack")
kk1 = ZZp(32003, Strategy=>"Flint")
elapsedTime M = random(ZZ^4000, ZZ^4000, Height=>32000, Density=>.2);
time M0 = mutableMatrix promote(M,kk);
time M1 = mutableMatrix promote(M,kk1);
time rank M0  -- this line uses the blas heavily
time rank M1  -- this line doesn't use the blas as far as I know.

elapsedTime M = random(ZZ^6000, ZZ^6000, Height=>32000, Density=>.2);
time M2 = mutableMatrix promote(M,kk);
time M3 = mutableMatrix promote(M,kk1);
time rank M2  -- this line uses the blas heavily
time rank M3  -- this line doesn't use the blas as far as I know.

-- the times for the 4 rank commands
-- MacBookPro, running 10.10.5, 16 GB ram, Mid 2014 Retina MacBookPro.
time rank M0 -- 2.27 sec
time rank M1 -- 7.82 sec
time rank M2 -- 7.01 sec
time rank M3 -- 40.99 sec

-- On an SL machine, which seems to be about the same speed (perhaps a bit faster) than
-- my mac:
time rank M0 -- 16.72 sec
time rank M1 -- 7.85 sec
time rank M2 -- 52.32 sec
time rank M3 -- 23.9 sec

-- the blas code appears to be running somewhat more than 7 times slower on
-- SL than on the mac.  I think ubuntu is similar to SL in speed here.
-- perhaps openblas can improve this?
