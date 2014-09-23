needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

-- Problem (2,1)^3 = 2 in Gr(3,6)
-- a problem with 2 solutions

SchPblm = {
    ({2,1}, id_(FFF^6)),
    ({2,1}, random(FFF^6,FFF^6)),
    ({2,1}, random(FFF^6,FFF^6))    
    }

S = solveSchubertProblem(SchPblm, 3,6);

assert all(S,s->checkIncidenceSolution(s, SchPblm))

end

restart
load "21e3-G36.m2"

