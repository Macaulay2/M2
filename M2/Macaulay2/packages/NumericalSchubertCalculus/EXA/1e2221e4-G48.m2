recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
setRandomSeed 23

-- 194 4-planes in C^8 wrt  random flags
--  1^2 2 21^4
-- This is far beyond what symbolic can do 
SchPblm = randomSchubertProblemInstance(
    {{2,1},{2,1},{2,1},{2,1},{2},{1},{1}},4,8);

time S = solveSchubertProblem(SchPblm,4,8)

assert(#S == 194)

end

restart
needsPackage "NumericalSchubertCalculus"
setVerboseLevel 1
time load "NumericalSchubertCalculus/EXA/1e2221e4-G48.m2"
needsPackage "NumericalAlgebraicGeometry"; setDefault(Software=>BERTINI)
