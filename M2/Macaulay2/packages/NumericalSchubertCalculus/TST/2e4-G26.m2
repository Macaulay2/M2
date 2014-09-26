debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0
DBG = 2
-- Problem (2)^4 = 3 in G(2,6)
Pblm={
    ({2}, id_(FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6))
    }
S1 = solveSchubertProblem(Pblm,2,6,LinearAlgebra=>true)
S2 = solveSchubertProblem(Pblm,2,6,LinearAlgebra=>false) --takes a lot of time

assert all(S1, s-> checkIncidenceSolution(s,Pblm))
assert all(S2, s-> checkIncidenceSolution(s,Pblm))
end

restart
load "2e4-G26.m2"

