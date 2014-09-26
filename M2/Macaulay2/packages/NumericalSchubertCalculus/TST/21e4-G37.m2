debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 2
DBG = 2
-- Problem (2,1)^4 = 8 in Gr(3,7)
-- a problem with 8 solutions

SchPblm = {
    ({2,1}, id_(FFF^7)),
    ({2,1}, random(FFF^7,FFF^7)),
    ({2,1}, random(FFF^7,FFF^7)),
    ({2,1}, random(FFF^7,FFF^7))    
    }

S = solveSchubertProblem(SchPblm, 3,7);

assert all(S,s->checkIncidenceSolution(s, SchPblm))

S = solveSchubertProblem(SchPblm, 3,7);

assert all(S,s->checkIncidenceSolution(s, SchPblm))

end

restart
load "21e4-G37.m2"

