needsPackage "NumericalSchubertCalculus"
setRandomSeed 4

-- 4 4-planes in C^8 wrt standard and 3 random flags
SchPblm = {({2,2},id_(FFF^8)), 
    ({2,2},random(FFF^8,FFF^8)),
    ({2,2},random(FFF^8,FFF^8)), 
    ({2,2},random(FFF^8,FFF^8))};

solveSchubertProblem(SchPblm,4,8)

end

restart
load "4fourspaces.m2"