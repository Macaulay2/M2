recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
setVerboseLevel 1
setRandomSeed 0 

setDefault(Software=>M2engine) --
-- setDefault(Software=>BERTINI) --singularity
-- setDefault(Software=>PHCPACK) --"singularity"

SchPblm = randomSchubertProblemInstance (toList (5:{2,1}) | {{1}},4,8);

sols = solveSchubertProblem(SchPblm,4,8)
assert all(sols,s->checkIncidenceSolution(s,SchPblm))

assert(#sols==138)
printStatistics()
end

restart
elapsedTime load "NumericalSchubertCalculus/EXA/21e5x1-G48.m2"
-- tracking time = 52.5297
-- 376.668 seconds elapsed

