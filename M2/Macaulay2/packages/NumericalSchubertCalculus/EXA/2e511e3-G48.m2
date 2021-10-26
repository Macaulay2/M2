restart
recursionLimit=1000
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 23
DBG=0

-- 91 4-planes in C^8 wrt  random flags
--  2^5 11^3
SchPblm = {
    ({1,1},random(FFF^8,FFF^8)), 
    ({1,1},random(FFF^8,FFF^8)), 
    ({1,1},random(FFF^8,FFF^8)), 
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8))
    };

solveSchubertProblem(SchPblm,4,8)

end

restart
time load "NumericalSchubertCalculus/EXA/2e511e3-G48.m2"
