restart
recursionLimit=1000
debug needsPackage "NumericalSchubertCalculus"

(k,n)=(3,6);
-- Problem (1)^6*(21) = 16 in G(3,6)
SchPblm={
    ({2,1}, id_(FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6))
    }
conds = SchPblm/first; 
(X,P,PS) = parametricSchubertProblem(conds,k,n);
needsPackage "NAGtools"
PH = parametricSegmentHomotopy(PS,X,P);
(s0,X,inverse'flags) = oneSolutionForOneInstance(conds,k,n)
p0 = point{inverse'flags/entries//flatten//flatten}
-*
vec'p0 = transpose matrix p0
evaluateH(specialize(PH,vec'p0||vec'p0), transpose matrix s0,0)
evaluateHx(specialize(PH,vec'p0||vec'p0), transpose matrix s0,0)
*-
setDefault(Software=>M2)
nextP = ()->point{apply(#coordinates p0,i->exp(2*pi*ii*random RR))}
sols = preimageViaMonodromy(PH,p0,{s0},RandomPointFunction=>nextP)
end

restart
time load "NumericalSchubertCalculus/EXA/1e621-G36-SLP.m2"

