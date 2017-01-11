restart
recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
setRandomSeed 23

-- 26 4-planes in C^8 wrt standard and 6 random flags
--  2^5  3^2
SchPblm = {({3},id_(FFF^8)), 
    ({3},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8)),
    ({2},random(FFF^8,FFF^8))};

solveSchubertProblem(SchPblm,4,8)

end

restart
elapsedTime load "NumericalSchubertCalculus/EXA/2e53e2-G48.m2"
