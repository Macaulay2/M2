restart
recursionLimit=1000
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 23
DBG=0

-- 194 4-planes in C^8 wrt  random flags
--  1^2 2 21^4
-- This is far beyond what symbolic can do 
SchPblm = {
    ({2,1},random(FFF^8,FFF^8)),
    ({2,1},random(FFF^8,FFF^8)),
    ({2,1},random(FFF^8,FFF^8)),
    ({2,1},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({1},random(FFF^8,FFF^8)),
    ({1},random(FFF^8,FFF^8))};

solveSchubertProblem(SchPblm,4,8)

end

restart
time load "NumericalSchubertCalculus/EXA/1e2e2221e4-G48.m2"
