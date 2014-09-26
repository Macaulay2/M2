debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

-- Problem (2,1)^3 = 2 in Gr(3,6)
-- a problem with 2 solutions

SchPblm = {
    ({2,2,1}, random(FFF^6,FFF^6)),
    ({2,1,1}, random(FFF^6,FFF^6))    
    }

S = solveSchubertProblem(SchPblm, 3,6);

assert all(S,s->checkIncidenceSolution(s, SchPblm))

{*
S2 = solveSchubertProblem(SchPblm, 3,6,LinearAlgebra=>false) --takes a lot of time
assert all(S2,s->checkIncidenceSolution(s, SchPblm))
*}

end

restart
load "poincare-G36.m2"
