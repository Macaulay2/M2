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


