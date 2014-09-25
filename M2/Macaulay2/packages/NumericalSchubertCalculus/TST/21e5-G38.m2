needsPackage "NumericalSchubertCalculus"
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 21
setFlags(NSC'DBG=>0,NSC'VERIFY'SOLUTIONS=>false)

-- Problem (2,1)^5 = ? in Gr(3,8)
-- a problem with ? solutions

-- Does not work!

SchPblm = {
    ({2,1}, id_(FFF^8)),
    ({2,1}, random(FFF^8,FFF^8)),
    ({2,1}, random(FFF^8,FFF^8)),
    ({2,1}, random(FFF^8,FFF^8)),
    ({2,1}, random(FFF^8,FFF^8))    
    }

S = solveSchubertProblem(SchPblm, 3,8);

assert all(S,s->checkIncidenceSolution(s, SchPblm))

#S

end

restart
load "21e5-G38.m2"

