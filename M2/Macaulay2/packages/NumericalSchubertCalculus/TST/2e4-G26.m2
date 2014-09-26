debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0

-- Problem (2)^4 = 3 in G(2,6)
Pblm={
    ({2}, id_(FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6))
    }
time S1 = solveSchubertProblem(Pblm,2,6,LinearAlgebra=>true)
time S2 = solveSchubertProblem(Pblm,2,6,LinearAlgebra=>false) --takes more time

assert all(S1, s-> checkIncidenceSolution(s,Pblm))
assert all(S2, s-> checkIncidenceSolution(s,Pblm))
assert (#S1==3 and #S2==3)
end

restart
load "2e4-G26.m2"

