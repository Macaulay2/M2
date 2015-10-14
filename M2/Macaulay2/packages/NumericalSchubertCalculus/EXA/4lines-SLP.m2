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
end

restart
load "NumericalSchubertCalculus/EXA/4lines-SLP.m2"
(X,P,PS) = parametricSchubertProblem(SchPblm/first,2,4)
S = solveSchubertProblem(SchPblm,2,4)
assert all(S,s->checkIncidenceSolution(s,SchPblm))

inverseFlags = SchPblm/last/(F->solve(F,id_(FFF^4)))
needsPackage "NAGtools"
p0 = point{drop(inverseFlags,2)/entries//flatten//flatten}
vec'p0 = transpose matrix p0
PH = parametricSegmentHomotopy(PS,X,P)

s0 = point{select(flatten entries transpose S#0, c->c!=1 and c!=0) / (c->1/c) }
evaluateH(specialize(PH,vec'p0||vec'p0), transpose matrix s0,0)
setDefault(Software=>M2)
preimageViaMonodromy(PH,p0,{s0})

skewSchubertVariety((2,4),{1},{1})
keys PH


