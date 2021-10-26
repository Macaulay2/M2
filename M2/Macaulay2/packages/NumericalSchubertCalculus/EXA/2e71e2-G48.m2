recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
setRandomSeed 23

-- setVerboseLevel 1

problem = randomSchubertProblemInstance (toList((7:{2}) | (2:{1})),4,8) 
k = 4; n = 8;
print "LR..."
elapsedTime sols = solveSchubertProblem(problem,k,n)
print "Monodromy..."
elapsedTime solsM = solveSchubertProblemViaMonodromy(problem,k,n,Verbose=>false)

end

restart
elapsedTime load "NumericalSchubertCalculus/EXA/2e71e2-G48.m2"
