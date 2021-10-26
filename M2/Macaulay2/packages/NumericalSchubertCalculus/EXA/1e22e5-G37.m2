recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"

-- this does not work (with either of the given seeds and either software)
SchPblm = randomSchubertProblemInstance( 
     {{1},{1},{2},{2},{2},{2},{2}} ,3,7); 
 
setRandomSeed 1 -- fails: "trackHomotopy: singularity encountered"

setRandomSeed 23 -- fails: "a solution does not fit the expected pattern (numerical error occurred)"            

setRandomSeed 23; 
setDefault(ErrorTolerance=>1e-12) -- fails

setRandomSeed 1; setDefault(Software=>BERTINI) --fails

-- this works (with M2engine but not BERTINI)
SchPblm = randomSchubertProblemInstance( 
     {{2},{2},{2},{2},{2},{1},{1}} ,3,7);

setRandomSeed 1; setDefault(Software=>M2engine)

elapsedTime S = solveSchubertProblem(SchPblm,3,7);

assert(#S == 26)

end

restart
load "NumericalSchubertCalculus/EXA/1e22e5-G37.m2"
