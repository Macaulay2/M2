restart
recursionLimit=1000
needsPackage "NumericalSchubertCalculus"
setRandomSeed 1
DBG=0

-- 10 4-planes in C^8 wrt standard and 7 random flags
--  1^4  3^3 111

SchPblm = randomSchubertProblemInstance ({{1,1,1},{3},{3},{3},{1},{1},{1},{1}},4,8)

sols = solveSchubertProblem(SchPblm,4,8)
assert(#sols==10)
end

restart
time load "NumericalSchubertCalculus/EXA/1e43e3111-G48.m2"

