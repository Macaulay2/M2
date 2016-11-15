recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"

setRandomSeed 23

-- 26 3-planes in C^7 wrt  random flags
--  1^2 2^5 
-- This Fails
SchPblm = randomSchubertProblemInstance( 
     {{1},{1},{2},{2},{2},{2},{2}} ,3,7);
    
setDefault(Software=>M2engine) 

elapsedTime S = solveSchubertProblem(SchPblm,3,7);

assert(#S == 26)

end

restart
load "NumericalSchubertCalculus/EXA/1e22e5-G37.m2"
