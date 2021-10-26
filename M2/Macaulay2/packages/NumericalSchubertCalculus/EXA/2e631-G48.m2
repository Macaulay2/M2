restart
recursionLimit=1000
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 23
DBG=0

-- 60 4-planes in C^8 wrt  random flags
--  2^6  31
SchPblm = {
    ({3,1},random(FFF^8,FFF^8)), 
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8))};

solveSchubertProblem(SchPblm,4,8)

end

restart
time load "NumericalSchubertCalculus/EXA/2e631-G48.m2"
