recursionLimit=5000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"

--setDefault(Software=>M2engine) --"singularity"
--setDefault(Software=>BERTINI) --"singularity" with randSeed 23 + randSeed 0
setDefault(Software=>PHCPACK) --"singularity" with randSeed 23 and 0

setRandomSeed 23


-- 126 4-planes in C^8 wrt  random flags
--  2^8  
-- This was computed in the alpha certified paper
SchPblm = randomSchubertProblemInstance( 
    {{2},{2},{2},{2},{2},{2},{2},{2}},4,8);
    
time S = solveSchubertProblem(SchPblm,4,8);

assert(#S == 126)

end

restart
load "NumericalSchubertCalculus/EXA/2e8-G48.m2"
