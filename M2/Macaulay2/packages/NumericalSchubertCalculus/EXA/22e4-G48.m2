debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 4
--DBG=1

-- 4 4-planes in C^8 wrt standard and 3 random flags
SchPblm = {({2,2},id_(FFF^8)), 
    ({2,2},random(FFF^8,FFF^8)),
    ({2,2},random(FFF^8,FFF^8)), 
    ({2,2},random(FFF^8,FFF^8))};

time solveSchubertProblem(SchPblm,4,8)

-- same problem but with respect to all 4 flags random
Pblm= randomSchubertProblemInstance({{2,2},{2,2},{2,2},{2,2}}, 4,8)
time S2 = solveSchubertProblem(Pblm,4,8)

end

restart
load "NumericalSchubertCalculus/EXA/22e4-G48.m2"
