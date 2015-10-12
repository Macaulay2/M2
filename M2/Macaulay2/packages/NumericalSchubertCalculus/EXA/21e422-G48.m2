restart
recursionLimit=1000
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 3
DBG=0

-- 42 4-planes in C^8 wrt standard and 4 random flags
--  21^4  22
SchPblm = {({2,2},id_(FFF^8)), 
    ({2,1},random(FFF^8,FFF^8)),
    ({2,1},random(FFF^8,FFF^8)),
    ({2,1},random(FFF^8,FFF^8)),
    ({2,1},random(FFF^8,FFF^8))
    };

solveSchubertProblem(SchPblm,4,8)

end

restart
time load "NumericalSchubertCalculus/EXA/21e422-G48.m2"
