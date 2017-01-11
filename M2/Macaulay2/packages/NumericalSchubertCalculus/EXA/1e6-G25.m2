debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 21

-- Problem (1)^6 = 5 in G(2,5)

Pblm= randomSchubertProblemInstance({{1},{1},{1},{1},{1},{1}}, 2,5);
time S = solveSchubertProblem(Pblm,2,5)

end

restart
load "NumericalSchubertCalculus/EXA/1e6-G25.m2"


