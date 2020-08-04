debug needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
setRandomSeed 0

-- Problem (1)^9 = 42 in G(3,6)
Pblm = randomSchubertProblemInstance(
    {{1},{1},{1},{1},{1},{1},{1},{1},{1}},3,6
    );
conds = Pblm/first; k = 3; n = 6;
print "Monodromy..."
elapsedTime solsM = solveSchubertProblemViaMonodromy(conds,k,n,Verbose=>false)
print "LR..."
setVerboseLevel 0
elapsedTime solsLR = solveSchubertProblem(Pblm,k,n);
end

restart
errorDepth=2
load "NumericalSchubertCalculus/EXA/1e9-G36-SLP.m2"



