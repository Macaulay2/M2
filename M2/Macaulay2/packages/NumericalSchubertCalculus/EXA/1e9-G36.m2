needsPackage "NumericalSchubertCalculus"
setRandomSeed 21
setVerboseLevel 0;

-- Problem (1)^9 = 42 in G(3,6)
Pblm = randomSchubertProblemInstance(
    {{1},{1},{1},{1},{1},{1},{1},{1},{1}},3,6
    );

time S = solveSchubertProblem(Pblm,3,6)
#S
assert all(S,s->checkIncidenceSolution(s,Pblm))
end

restart
load "NumericalSchubertCalculus/EXA/1e9-G36.m2"
