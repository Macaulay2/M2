recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
setVerboseLevel 1
setRandomSeed 0 

setDefault(Software=>M2engine) --
-- setDefault(Software=>BERTINI) --singularity
-- setDefault(Software=>PHCPACK) --"singularity"

SchPblm = randomSchubertProblemInstance (toList (5:{2,1}) | {{1}},4,8);

time sols = solveSchubertProblem(SchPblm,4,8)
-- about 10 min

assert(#sols==138)
end

restart
elapsedTime load "NumericalSchubertCalculus/EXA/21e5x1-G48.m2"

