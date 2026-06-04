restart
needsPackage "NumericalAlgebraicGeometry"
needsPackage "ExampleSystems"
T = katsura(10,CC_53)
(S,solsS) = totalDegreeStartSystem T;
H = segmentHomotopy(ii*S,T)
NAGtrace 2
numTBBThreads = 4
elapsedTime sols = trackHomotopy(H,solsS);
assert(#solsS == #sols)
