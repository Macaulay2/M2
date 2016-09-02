needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z];
d = 5
T = apply(numgens R, i->random(d,R)-1);
(S,solsS) = totalDegreeStartSystem T;
end

restart
load "track.gc-bug.m2"

elapsedTime for i to 100 do sols = first track(random CC * S,T,solsS) -- this is fine
sols = new MutableHashTable;
elapsedTime for i to 100 do sols#i = track(S,T,solsS) -- eats 27Mb -- not disposing of the tracker and polynomial systems
elapsedTime for i to 100 do sols#i = first track(S,T,solsS)  -- eats 8Mb -- why?
elapsedTime for i to 100 do sols#i = coordinates first track(S,T,solsS)  -- eats 0Mb


restart
load "track.gc-bug.m2"
debug NumericalAlgebraicGeometry
H = segmentHomotopy(S,T)
elapsedTime for i to 100 do sols = trackHomotopy(H,solsS) 
sols = new MutableHashTable;
elapsedTime for i to 100 do sols#i = track(S,T,solsS) -- eats 35Mb
elapsedTime for i to 100 do sols#i = first track(S,T,solsS)  -- eats 1Mb
elapsedTime for i to 100 do sols#i = coordinates first track(S,T,solsS)  -- eats 0Mb

 