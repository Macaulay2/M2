restart
recursionLimit=1000
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 23

-- 26 4-planes in C^8 wrt standard and 6 random flags
--  2^5  3^2
problem = {({3},id_(FFF^8)), 
    ({3},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8))};

k = 4; n = 8;
print "LR..."
elapsedTime sols = solveSchubertProblem(problem,k,n)
print "Monodromy..."
elapsedTime solsM = solveSchubertProblemViaMonodromy(problem,k,n,Verbose=>false)

end

restart
elapsedTime load "NumericalSchubertCalculus/EXA/2e53e2-G48.m2"
