debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0
-- DBG = 2
VERIFY'SOLUTIONS = true 
-- 4 lines in P^3 wrt standard and 3 random flags

k=2; n=4;
problem = {
    ({1},id_(FFF^4)), 
    ({1},rsort id_(FFF^4)),
    ({1},random(FFF^4,FFF^4)), 
    ({1},random(FFF^4,FFF^4))
    };

print "Monodromy..."
elapsedTime solsM = solveSchubertProblemViaMonodromy(problem/first,k,n,Verbose=>false)
print "LR..."
elapsedTime solsLR = solveSchubertProblem(problem,k,n);
end

restart
load "NumericalSchubertCalculus/EXA/4lines.m2"



