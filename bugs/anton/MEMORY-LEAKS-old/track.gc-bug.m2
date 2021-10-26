-- 2017
-- There may be a memory leak in trackHomotopy. 
-- Status: we believe there is no leak.

needsPackage "NumericalAlgebraicGeometry"
load (currentFileDirectory|"PointArray.m2")
R = CC[x,y,z];
T = apply(numgens R, i->random(d,R)-1);
(S,solsS) = totalDegreeStartSystem T;
end


restart ------------------------------------------------
d = 5
load "track.gc-bug.m2"
sols = null
elapsedTime for i to 100 do sols = track(random CC * S,random CC * T,solsS) -- this is fine

-- RHEL
sols = new MutableHashTable;
elapsedTime for i to 100 do sols#i = coordinates first track(S,T,solsS)  -- eats 0Mb
elapsedTime for i to 100 do sols#i = first track(S,T,solsS)  -- eats 6Mb
elapsedTime for i to 100 do sols#i = track(S,T,solsS) -- eats additional 21Mb -- not disposing of the tracker and polynomial systems

restart ---------------------------------------------
d = 5
load "track.gc-bug.m2"
debug NumericalAlgebraicGeometry
H = segmentHomotopy(S,T)
elapsedTime for i to 100 do sols = trackHomotopy(H,solsS) 
sols = new MutableHashTable;
elapsedTime for i to 100 do sols#i = coordinates first trackHomotopy(H,solsS)  -- eats 0Mb
elapsedTime for i to 100 do sols#i = first trackHomotopy(H,solsS) -- eats 0Mb
elapsedTime for i to 100 do sols#i = trackHomotopy(H,solsS) -- eats 5Mb

restart  -- PointArray seems to be OK
d = 8
load "track.gc-bug.m2"  
arrayT = pointArray track(S,T,solsS)
length arrayT -- expect d^3
T' = apply(numgens R, i->random(d,R)-1)
arrayT' = pointArray{};
debug NumericalAlgebraicGeometry
setRandomSeed 1
TT' = segmentHomotopy(random CC * T, random CC * T')
T'T = segmentHomotopy(random CC * T', random CC * T)
elapsedTime for i to 100 do (
    pts = trackHomotopy(TT', points arrayT);
    appendPoints(arrayT', select(pts,s->status s==Regular));
    pts = trackHomotopy(T'T, points arrayT');
    appendPoints(arrayT, select(pts,s->status s==Regular));
    )
length arrayT

restart 
d = 10
load "track.gc-bug.m2"  
debug NumericalAlgebraicGeometry
H = segmentHomotopy(S,T)
sols = new MutableHashTable
N = 100
elapsedTime for i to N-1 do sols#i = trackHomotopy(H,solsS)/coordinates 
elapsedTime for i to N-1 do sols#i = (trackHomotopy(H,solsS))/(m -> mutableMatrix {coordinates m})
elapsedTime for i to N-1 do sols#i = mutableMatrix((trackHomotopy(H,solsS))/(m -> coordinates m)) -- 92.6-80

elapsedTime for i to N-1 do sols#i = (for j from 1 to 1000 list for k from 1 to 3 list random(CC_53)) -- 154-80
elapsedTime for i to N-1 do sols#i = (for j from 1 to 3000 list random(CC_53)) -- 144-80
elapsedTime for i to N-1 do sols#i = (for j from 1 to 3000 list random(CC_60)) -- 144-80
elapsedTime for i to N-1 do sols#i = mutableMatrix(for j from 1 to 1000 list for k from 1 to 3 list random(CC_53)) -- 86-80
elapsedTime for i to N-1 do sols#i = (for j from 1 to 1000 list mutableMatrix{for k from 1 to 3 list random(CC_53)}) -- 137-80 SLOW (9.xx sec)
elapsedTime for i to N-1 do sols#i = (for j from 1 to 1000 list matrix{for k from 1 to 3 list random(CC_53)}) -- 438-80, ALSO SLOW (8.4 sec)
M = mutableMatrix(CC_53, 4, 1)
fillMatrix M

M_(1,1) = 3.1
1e6*(164-87)/(N*d^3) -- N = 100
1e6*(1025-87)/(N*d^3) -- N = 1000 
1e6*(8800-87)/(N*d^3) -- N = 10000 
