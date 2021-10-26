debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0

-- Problem (1)^9 = 42 in G(3,6)
problem = randomSchubertProblemInstance(
    {{1},{1},{1},{1},{1},{1},{1},{1},{1}},3,6
    );
k = 3; n = 6;
print "Monodromy..."
elapsedTime solsM = solveSchubertProblemViaMonodromy(problems,k,n,Verbose=>false)
print "LR..."
setVerboseLevel 0
elapsedTime solsLR = solveSchubertProblem(problem,k,n);
end

restart
errorDepth=2
load "NumericalSchubertCalculus/EXA/1e9-G36-SLP.m2"



