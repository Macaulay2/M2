needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

-- Problem (2,1)^3 = 2 in Gr(3,6)
-- a problem with 2 solutions

SchPblm = randomSchubertProblemInstance (
    {{2,1},{2,1},{2,1}}, 3,6);

time S = solveSchubertProblem(SchPblm, 3,6);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

end

restart
load "NumericalSchubertCalculus/TST/21e3-G36.m2"

S2 = solveSchubertProblem(SchPblm, 3,6, LinearAlgebra=>false); --takes a lot of time
assert all(S,s->checkIncidenceSolution(s, SchPblm))


