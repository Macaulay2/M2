restart
recursionLimit=1000
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 23
DBG=0

-- 126 4-planes in C^8 wrt  random flags
--  2^8  
-- This was computed in the alpha certified paper
SchPblm = {
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8))};

solveSchubertProblem(SchPblm,4,8)

end

restart
time load "NumericalSchubertCalculus/EXA/2e8-G48.m2"
