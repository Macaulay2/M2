debug 
needsPackage "NumericalSchubertCalculus"
setRandomSeed 21
setFlags(NSC'DBG=>2)

-- Problem (1)^6 = 5 in G(2,5)
Pblm={
    ({1}, id_(FFF^5)),
    ({1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5))
    }
S = solveSchubertProblem(Pblm,2,5)

end

restart
load "1e6-G25.m2"


