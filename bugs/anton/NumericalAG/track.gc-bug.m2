needsPackage "NumericalAlgebraicGeometry"
-- load (currentFileDirectory|"PointArray.m2")
R = CC[x,y,z];
d = 5
T = apply(numgens R, i->random(d,R)-1);
(S,solsS) = totalDegreeStartSystem T;
end


restart ------------------------------------------------
load "track.gc-bug.m2"
sols = null
elapsedTime for i to 100 do sols = track(random CC * S,random CC * T,solsS) -- this is fine

-- RHEL
sols = new MutableHashTable;
elapsedTime for i to 100 do sols#i = coordinates first track(S,T,solsS)  -- eats 0Mb
elapsedTime for i to 100 do sols#i = first track(S,T,solsS)  -- eats 6Mb
elapsedTime for i to 100 do sols#i = track(S,T,solsS) -- eats additional 21Mb -- not disposing of the tracker and polynomial systems

restart ---------------------------------------------
load "track.gc-bug.m2"
debug NumericalAlgebraicGeometry
H = segmentHomotopy(S,T)
elapsedTime for i to 100 do sols = trackHomotopy(H,solsS) 
sols = new MutableHashTable;
elapsedTime for i to 100 do sols#i = coordinates first trackHomotopy(H,solsS)  -- eats 0Mb
elapsedTime for i to 100 do sols#i = first trackHomotopy(H,solsS) -- eats 0Mb
elapsedTime for i to 100 do sols#i = trackHomotopy(H,solsS) -- eats 5Mb

restart  -- PointArray seems to be OK
load "track.gc-bug.m2"  
sols = pointArray{}
elapsedTime for i to 100 do (
    pts = track(random CC * S, random CC * T, solsS);
    for pt in pts do (
	if not member(pt,sols) then appendPoints(sols, pt)
    	)
    )
sols = new MutableHashTable;


