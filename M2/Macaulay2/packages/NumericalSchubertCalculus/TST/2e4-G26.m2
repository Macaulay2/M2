needsPackage "NumericalSchubertCalculus"
setRandomSeed 0
VERIFY'SOLUTIONS = true 

-- Problem (2)^4 = 3 in G(2,6)

SchPblm = randomSchubertProblemInstance ({{2},{2},{2},{2}},2,6);
time S1 = solveSchubertProblem(SchPblm,2,6, LinearAlgebra=>true);
time S2 = solveSchubertProblem(SchPblm,2,6,LinearAlgebra=>false); --takes more time

assert all(S1, s-> checkIncidenceSolution(s,SchPblm))
assert all(S2, s-> checkIncidenceSolution(s,SchPblm))
assert (#S1==3 and #S2==3)
end

restart
load "NumericalSchubertCalculus/TST/2e4-G26.m2"

