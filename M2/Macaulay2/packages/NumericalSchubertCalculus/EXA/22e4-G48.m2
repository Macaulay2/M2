debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 4
DBG=1

-- 4 4-planes in C^8 wrt standard and 3 random flags
SchPblm = {({2,2},id_(FFF^8)), 
    ({2,2},random(FFF^8,FFF^8)),
    ({2,2},random(FFF^8,FFF^8)), 
    ({2,2},random(FFF^8,FFF^8))};

solveSchubertProblem(SchPblm,4,8)

end

restart
time load "NumericalSchubertCalculus/EXA/22e4-G48.m2"
