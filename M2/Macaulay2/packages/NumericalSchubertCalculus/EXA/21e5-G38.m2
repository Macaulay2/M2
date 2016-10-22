needsPackage "NumericalSchubertCalculus"
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 21
setDebugOptions("debug"=>1,"verify solutions"=>false)

-- Problem (2,1)^5 in Gr(3,8)
-- a problem with 32 solutions

SchPblm = {
    ({2,1}, id_(FFF^8)),
    ({2,1}, random(FFF^8,FFF^8)),
    ({2,1}, random(FFF^8,FFF^8)),
    ({2,1}, random(FFF^8,FFF^8)),
    ({2,1}, random(FFF^8,FFF^8))    
    }

S = solveSchubertProblem(SchPblm, 3,8);

-- time assert all(S,s->checkIncidenceSolution(s, SchPblm))

print(#S)

end

restart
time load "21e5-G38.m2"

