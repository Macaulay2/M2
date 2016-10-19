debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0
-- DBG = 2
VERIFY'SOLUTIONS = true 

-- Problem (1)^9 = 42 in G(3,6)
Pblm = randomSchubertProblemInstance(
    {{1},{1},{1},{1},{1},{1},{1},{1},{1}},3,6
    );
conds = Pblm/first; k = 3; n = 6;
(X,P,PS) = parametricSchubertProblem(conds,k,n)

needsPackage "NAGtools"
PH = parametricSegmentHomotopy(PS,X,P)
(s0,X,inverse'flags) = oneSolutionForOneInstance(conds,k,n)
p0 = point{inverse'flags/entries//flatten//flatten}
vec'p0 = transpose matrix p0
evaluateH(specialize(PH,vec'p0||vec'p0), transpose matrix s0,0)
setDefault(Software=>M2)
sols = preimageViaMonodromy(PH,p0,{s0})
assert (#sols == 2)
end
restart
load "NumericalSchubertCalculus/EXA/1e9-G36-SLP.m2"



