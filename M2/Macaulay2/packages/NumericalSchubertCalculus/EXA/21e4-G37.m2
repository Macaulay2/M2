needsPackage "NumericalSchubertCalculus"
setRandomSeed 5
--setRandomSeed 2
setVerboseLevel 0

-- Problem (2,1)^4 = 8 in Gr(3,7)
-- a problem with 8 solutions

SchPblm = randomSchubertProblemInstance(
    {{2,1},{2,1},{2,1},{2,1}},3,7
    );

time S = solveSchubertProblem(SchPblm, 3,7);
assert all(S,s->checkIncidenceSolution(s, SchPblm))

end

time S2 = solveSchubertProblem(SchPblm, 3,7,LinearAlgebra=>false);--takes a lot of time!
assert all(S2,s->checkIncidenceSolution(s, SchPblm))

end

restart
load "NumericalSchubertCalculus/EXA/21e4-G37.m2"

