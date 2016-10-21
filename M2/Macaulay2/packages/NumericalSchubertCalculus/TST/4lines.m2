needsPackage "NumericalSchubertCalculus"
setRandomSeed 0

-- 4 lines in P^3 wrt standard and 3 random flags
SchPblm = randomSchubertProblemInstance ({{1},{1},{1},{1}},2,4)

time S = solveSchubertProblem(SchPblm,2,4)

assert all(S,s->checkIncidenceSolution(s,SchPblm))
end
restart
load "NumericalSchubertCalculus/TST/4lines.m2"
