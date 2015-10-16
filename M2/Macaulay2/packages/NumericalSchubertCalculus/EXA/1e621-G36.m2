needsPackage "NumericalSchubertCalculus"
setRandomSeed 21

-- Problem (1)^6*(21) = 16 in G(3,6)
Pblm= randomSchubertProblemInstance(
    {{2,1},{1},{1},{1},{1},{1},{1}}, 3,6);

time S = solveSchubertProblem(Pblm,3,6)

end

restart
time load "NumericalSchubertCalculus/EXA/1e621-G36.m2"

