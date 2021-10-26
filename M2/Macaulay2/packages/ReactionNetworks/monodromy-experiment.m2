restart
FF = CC_53
needsPackage "ReactionNetworks"
needsPackage "NAGtools"
CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B + E"
stSpace = stoichiometricSubspace CRN
F = matrix{steadyStateEquations(CRN,FF)}
R = ring F
n = numgens R
pre0 = point{apply(n,i->random FF)}
F0 = sub(F, apply(gens R,coordinates pre0,(x,x0)->x=>x0))
V = numericalIrreducibleDecomposition ideal F0
W = first V#3
c0 = sample W

normals = sub(gens stSpace,FF)
L = (vars R - matrix pre0) * normals

setDefault(Software=>PHCPACK)
stop = (n,L)->n>1
rMatrix = random(FF^(numgens R),FF^(numcols(F|L)))
elapsedTime sols = solveViaMonodromy(sub(rMatrix,R) * transpose (F|L),c0,{pre0},
    StoppingCriterion=>stop);

end





R' = FF[gens R]
toR' = map(R',R, vars R' | random(FF^1,FF^(numgens coefficientRing R)))
F' = toR' matrix{F}
L = vars R' * sub(gens stSpace,FF) - random(FF^1,FF^(numgens stSpace))





I = ideal (F')
isPrime I
dec = decompose I
sols = solveSystem polySystem transpose (F'|L_{1})
#sols

