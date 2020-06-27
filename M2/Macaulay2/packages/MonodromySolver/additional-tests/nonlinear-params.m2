needsPackage "MonodromySolver"
declareVariable \ {A,B,C, t}
P=gateSystem(matrix{{A,B,C}},matrix{{t}},transpose matrix{{A^2-t,B-t^2,C-t^3}})
createSeedPair P -- throws an error
