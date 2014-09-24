needsPackage "NumericalSchubertCalculus"
setRandomSeed 21

-- Problem (1)^6 = 5 in G(2,5)
Pblm={
    ({1}, id_(FFF^4)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6))
    }
S = solveSchubertProblem(Pblm,2,5)

end

restart
load "1e6-G25.m2"

