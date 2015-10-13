recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
setRandomSeed 0
setDefault(Software=>M2engine) --"singularity"
setDefault(Software=>BERTINI) --works
--setDefault(Software=>PHCPACK) --"singularity"

-- 10 4-planes in C^8 
--  1^4  3^3 111

SchPblm = randomSchubertProblemInstance ({{1,1,1},{3},{3},{3},{1},{1},{1},{1}},4,8)

sols = solveSchubertProblem(SchPblm,4,8)
assert(#sols==10)
end

restart
time load "NumericalSchubertCalculus/EXA/1e43e3111-G48.m2"

