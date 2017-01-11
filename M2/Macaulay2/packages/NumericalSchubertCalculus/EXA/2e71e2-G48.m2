recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
setRandomSeed 23

setVerboseLevel 1
-- setDefault(Software=>BERTINI)

SchPblm = randomSchubertProblemInstance (toList((7:{2}) | (2:{1})),4,8) 
sols = solveSchubertProblem(SchPblm,4,8)
end

restart
elapsedTime load "NumericalSchubertCalculus/EXA/2e71e2-G48.m2"
