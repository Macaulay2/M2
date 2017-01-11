debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0
-- DBG = 2
VERIFY'SOLUTIONS = true 
-- 4 lines in P^3 wrt standard and 3 random flags

SchPblm = {
    ({1},id_(FFF^4)), 
    ({1},rsort id_(FFF^4)),
    ({1},random(FFF^4,FFF^4)), 
    ({1},random(FFF^4,FFF^4))
    };

conds = SchPblm/first; k = 2; n = 4;
(X,P,PS) = parametricSchubertProblem(conds,k,n)

{*
S = solveSchubertProblem(SchPblm,2,4)
assert all(S,s->checkIncidenceSolution(s,SchPblm))
inverseFlags = SchPblm/last/(F->solve(F,id_(FFF^4)))
p0 = point{drop(inverseFlags,2)/entries//flatten//flatten}
s0 = point{select(flatten entries transpose S#0, c->c!=1 and c!=0) / (c->1/c) }
*}

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
load "NumericalSchubertCalculus/EXA/4lines-SLP.m2"



