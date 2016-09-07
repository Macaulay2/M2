needsPackage "NumericalAlgebraicGeometry"
load (currentFileDirectory|"PointArray.m2")
R = CC[x,y,z];
d = 5
T = apply(numgens R, i->random(d,R)-1);
(S,solsS) = totalDegreeStartSystem T;
end

restart
load "track.gc-bug.m2"

sols = new MutableHashTable
elapsedTime for i to 100 do sols#i = apply(d^3, j->point{apply(3,k->random CC)})
elapsedTime for i to 100 do sols#i = track(S,T,solsS)/coordinates

restart
load "track.gc-bug.m2"
sols = null
elapsedTime for i to 100 do sols = track(random CC * S,random CC * T,solsS) -- this is fine

sols = new MutableHashTable
-- this is what happens on RHEL
elapsedTime for i to 100 do sols#i = track(S,T,solsS) -- eats 30Mb
elapsedTime for i to 100 do sols#i = first track(S,T,solsS)  -- eats 8Mb
elapsedTime for i to 100 do sols#i = coordinates first track(S,T,solsS)  -- eats 2Mb



sols = pointArray{}
elapsedTime for i to 100 do (
    pts = track(random CC * S, random CC * T, solsS);
    for pt in pts do (
	if not member(pt,sols) then appendPoints(sols, pt)
    	)
    )
sols = new MutableHashTable;

elapsedTime for i to 100 do sols#i = new MutableList from coordinates first track(S,T,solsS)  -- eats 2Mb

