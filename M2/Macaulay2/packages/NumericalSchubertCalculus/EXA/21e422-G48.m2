restart
recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
setRandomSeed 3
--setDefault(Software=>M2engine) --works ~305.39 s
setDefault(Software=>BERTINI) --works ~180.021s
--setDefault(Software=>PHCPACK) --works ~141.548s 


-- 42 4-planes in C^8 wrt 5 random unitary flags
--  21^4  22

SchPblm = randomSchubertProblemInstance (
    {{2,2},{2,1},{2,1},{2,1},{2,1}}, 4,8);

time sols = solveSchubertProblem(SchPblm,4,8);
assert(#sols == 42)

end

restart
time load "NumericalSchubertCalculus/EXA/21e422-G48.m2"
