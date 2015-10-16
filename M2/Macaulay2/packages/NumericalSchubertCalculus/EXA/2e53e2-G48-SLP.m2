restart
recursionLimit=1000
debug needsPackage "NumericalSchubertCalculus"

-- 26 4-planes in C^8 wrt standard and 6 random flags
--  2^5  3^2
(k,n)=(4,8);
SchPblm = {({3},id_(FFF^8)), 
    ({3},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8))};
conds = SchPblm/first; 
(X,P,PS) = parametricSchubertProblem(conds,k,n);
needsPackage "NAGtools"
PH = parametricSegmentHomotopy(PS,X,P);
(s0,X,inverse'flags) = oneSolutionForOneInstance(conds,k,n)
p0 = point{inverse'flags/entries//flatten//flatten}
{*
vec'p0 = transpose matrix p0
evaluateH(specialize(PH,vec'p0||vec'p0), transpose matrix s0,0)
*}
setDefault(Software=>M2)
sols = preimageViaMonodromy(PH,p0,{s0})
end

restart
time load "NumericalSchubertCalculus/EXA/2e53e2-G48-SLP.m2"
