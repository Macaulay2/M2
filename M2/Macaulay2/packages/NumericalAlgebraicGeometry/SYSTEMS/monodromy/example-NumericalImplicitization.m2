restart
setRandomSeed 1
needsPackage "NumericalImplicitization" 
R = CC[s,t]; F = flatten entries basis(5,R);
W = numericalImageDegree(F, ideal 0_R)
R = CC[s,t]; F = flatten entries basis(5,R);
W' = numericalImageDegree(F, ideal(s-1))

R = CC[s,t]; F = flatten entries basis(5,R);
W = numericalImageDegree(F, ideal 0_R, Software=>MonodromySolver)
R = CC[s,t]; F = flatten entries basis(5,R);
W' = numericalImageDegree(F, ideal(s-1), Software=>MonodromySolver)
