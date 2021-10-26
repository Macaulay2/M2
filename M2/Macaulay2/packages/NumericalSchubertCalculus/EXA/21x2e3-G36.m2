needsPackage "NumericalSchubertCalculus"
setRandomSeed 4

setVerboseLevel 1

-- Problem (2,1)*(2)^3 = 2 in G(3,6) 
-- This problem has a non-trivial tree (not like the problem of 4 lines)
Pblm= randomSchubertProblemInstance(
    {{2},{2},{2},{2,1}},3,6
    );

time S = solveSchubertProblem(Pblm,3,6)

assert all(S, s-> checkIncidenceSolution(s,Pblm))

end

restart
load "NumericalSchubertCalculus/EXA/21x2e3-G36.m2"
