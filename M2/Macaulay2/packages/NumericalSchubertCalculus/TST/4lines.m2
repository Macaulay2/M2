needsPackage "NumericalSchubertCalculus"
setRandomSeed 4

root = playCheckers({1},{1},2,4)
resolveNode(root, {({1},random(FFF^4,FFF^4)), ({1},random(FFF^4,FFF^4))})


-- 4 lines in P^3 wrt standard and 3 random flags
SchPblm = {({1},id_(FFF^4)), 
    ({1},random(FFF^4,FFF^4)),
    ({1},random(FFF^4,FFF^4)), 
    ({1},random(FFF^4,FFF^4))};

solveSchubertProblem(SchPblm,2,4)

end

restart
load "4lines.m2"