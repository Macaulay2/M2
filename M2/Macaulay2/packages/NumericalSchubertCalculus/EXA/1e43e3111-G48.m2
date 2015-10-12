restart
recursionLimit=1000
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 3
DBG=0

-- 10 4-planes in C^8 wrt standard and 7 random flags
--  1^4  3^3 111
SchPblm = {({1,1,1},id_(FFF^8)), 
    ({3},random(FFF^8,FFF^8)),
    ({3},random(FFF^8,FFF^8)),
    ({3},random(FFF^8,FFF^8)),
    ({1},random(FFF^8,FFF^8)), 
    ({1},random(FFF^8,FFF^8)), 
    ({1},random(FFF^8,FFF^8)), 
    ({1},random(FFF^8,FFF^8))
    };

solveSchubertProblem(SchPblm,4,8)

end

restart
time load "NumericalSchubertCalculus/EXA/1e43e3111-G48.m2"
