recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"

setRandomSeed 23

-- 126 4-planes in C^8 wrt  random flags
--  2^8  
-- This was computed in the alpha certified paper
SchPblm = randomSchubertProblemInstance( 
    {{2},{2},{2},{2},{2},{2},{2},{2}},4,8);
    
setDefault(Software=>M2engine) --works ~1801.89s

elapsedTime S = solveSchubertProblem(SchPblm,4,8);

assert(#S == 126)

setDefault(Software=>BERTINI) -- works 1771.24s

elapsedTime S = solveSchubertProblem(SchPblm,4,8);

assert(#S == 126)


setDefault(Software=>PHCPACK) -- "Singularity"

elapsedTime S = solveSchubertProblem(SchPblm,4,8);

assert(#S == 126)


end

restart
load "NumericalSchubertCalculus/EXA/2e8-G48.m2"
