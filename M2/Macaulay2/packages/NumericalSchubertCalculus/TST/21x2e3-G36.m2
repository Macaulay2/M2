needsPackage "NumericalSchubertCalculus"
setRandomSeed 4

-- Problem (2,1)*(2)^3 = 2 in G(3,6) 
-- This problem has a non-trivial tree (not like the problem of 4 lines)
Pblm={({2},random(FFF^6,FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6)), 
    ({2,1},random(FFF^6,FFF^6))};

S = solveSchubertProblem(Pblm,3,6)

assert all(S, s-> checkIncidenceSolution(s,Pblm))

end

restart
load "21x2e3-G36.m2"
