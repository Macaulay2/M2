debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 21
DBG = 2

-- Problem (1)^9 = 42 in G(3,6)
Pblm={
    ({1}, id_(FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6))
    }

S = solveSchubertProblem(Pblm,3,6)
assert all(S,s->checkIncidenceSolution(s,Pblm))
end

restart
load "1e9-G36.m2"

TSt := apply(Pblm, P-> all(S,s->checkIncidenceSolution(s,{P})))

TST := apply()

S:=newDag.Solutions
all(S,s-> checkIncidenceSolution(newDag.FlagM*s, {(l1,newDag.FlagM)}))
all(S,s-> checkIncidenceSolution(newDag.FlagM*s, {(l2,ID)}))

all(S,s->checkIncidenceSolution(newDag.FlagM*s, new'remaining'conditions'flags))
#new'remaining'conditions'flags
