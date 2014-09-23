needsPackage "NumericalSchubertCalculus"
setRandomSeed 0

-- Problem (2)^4 = 3 in G(2,6)
Pblm={
    ({2}, id_(FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6))
    }
S = solveSchubertProblem(Pblm,2,6)

end

restart
load "2e4-G26.m2"

