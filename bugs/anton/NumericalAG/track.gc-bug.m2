restart
needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z];
d = 5
T = apply(numgens R, i->random(d,R)-1);
(S,solsS) = totalDegreeStartSystem T;
elapsedTime for i to 100 do sols = first track(random CC * S,T,solsS) -- this is fine
sols = new MutableHashTable;
elapsedTime for i to 100 do sols#i = track(S,T,solsS) -- eats 30Mb
elapsedTime for i to 100 do sols#i = first track(S,T,solsS)  -- eats 8Mb
elapsedTime for i to 100 do sols#i = coordinates first track(S,T,solsS)  -- eats 2Mb
elapsedTime for i to 100 do sols#i = new MutableList from coordinates first track(S,T,solsS)  -- eats 2Mb

needsPackage "MonodromySolver"
...
...
 