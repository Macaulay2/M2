needsPackage "NumericalSchubertCalculus"
setRandomSeed 0

-- 4 lines in P^3 wrt standard and 3 random flags

SchPblm = {
    ({1},id_(FFF^4)), 
    ({1},random(FFF^4,FFF^4)),
    ({1},random(FFF^4,FFF^4)), 
    ({1},random(FFF^4,FFF^4))
    };

S = solveSchubertProblem(SchPblm,2,4)
assert all(S,s->checkIncidenceSolution(s,SchPblm))
end
restart
load "NumericalSchubertCalculus/TST/4lines.m2"
