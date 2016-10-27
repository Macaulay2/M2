restart
setRandomSeed 0
needsPackage "NumericalImplicitization" 
R = CC[s,t]; F = flatten entries basis(5,R);
numericalImageDim(F,ideal 0_R)
W = numericalImageDegree(F, ideal 0_R)
--R = CC[s,t]; F = flatten entries basis(5,R);
--W' = numericalImageDegree(F, ideal(s-1))

R = CC[s,t]; F = flatten entries basis(15,R);
W = numericalImageDegree(F, ideal 0_R, Software=>MonodromySolver)
R = CC[s,t]; F = flatten entries basis(15,R);
W' = numericalImageDegree(F, ideal(s-1), Software=>MonodromySolver) 
W' = numericalImageDegree(F, ideal(random(1,R)-1), Software=>MonodromySolver)

R = CC[s,t]; F = flatten entries basis(50,R);
W = numericalImageDegree(F, ideal 0_R, Software=>BERTINI)
W = numericalImageDegree(F, ideal 0_R)
W' = numericalImageDegree(F, ideal(random(1,R)-1), Software=>BERTINI) -- PointArray error
