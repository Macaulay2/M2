needsPackage "NumericalSchubertCalculus"
setRandomSeed 21

-- Problem (2)^4 = 2 in G(2,6)
Pblm={
    ({2}, id_(FFF^4)),
    ({2}, random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6))
    }
S = solveSchubertProblem(Pblm,3,6)

end

restart
load "2e4-G26.m2"

