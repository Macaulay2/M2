restart
needs "test-mem-leaks.m2"
needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z]
F = {random(2,R)-1,random(2,R)-1,random(2,R)-1}
(G,solsG) = totalDegreeStartSystem F
testF(10000, () -> (track(G,F,solsG);))

