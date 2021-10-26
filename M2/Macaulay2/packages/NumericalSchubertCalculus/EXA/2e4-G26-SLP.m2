debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0
VERIFY'SOLUTIONS = true 

-- Problem (2)^4 = 3 in G(2,6)

k = 2; n = 6;
SchPblm = randomSchubertProblemInstance ({{2},{2},{2},{2}},k,n);
conds = SchPblm/first; 
(X,P,PS) = parametricSchubertProblem(conds,k,n)
needsPackage "NAGtools"
PH = parametricSegmentHomotopy(PS,X,P)
(s0,X,inverse'flags) = oneSolutionForOneInstance(conds,k,n)
p0 = point{inverse'flags/entries//flatten//flatten}
-*
vec'p0 = transpose matrix p0
evaluateH(specialize(PH,vec'p0||vec'p0), transpose matrix s0,0)
*-
setDefault(Software=>M2)
sols = preimageViaMonodromy(PH,p0,{s0})
assert (#sols == 3)

end

restart
load "NumericalSchubertCalculus/EXA/2e4-G26-SLP.m2"

