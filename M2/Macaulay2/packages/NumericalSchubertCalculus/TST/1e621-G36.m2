needsPackage "NumericalSchubertCalculus"
setRandomSeed 21

-- Problem (1)^6*(21) = 16 in G(3,6)
Pblm={
    ({2,1}, id_(FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6))
    }
S = solveSchubertProblem(Pblm,3,6)

end

restart
load "1e621-G36.m2"

