needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

-- Problem 1: {3,2,1} {1,1} {1} in Gr(3,6)
SchPblem = {
    ({3,2,1}, random(FFF^6,FFF^6)),
    ({1,1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6))
    }

Solutions = solveSchubertProblem(SchPblem, 3,6)
assert all(Solutions, s-> checkIncidenceSolution(s, SchPblem))

-- Problem 2: {2,1} {2,1} {2,1} in Gr(3,6)
-- a problem with 2 solutions
SchPblm = {
    ({2,1}, random(FFF^6,FFF^6)),
    ({2,1}, random(FFF^6,FFF^6)),
    ({2,1}, random(FFF^6,FFF^6))    
    }

S = solveSchubertProblem(SchPblm, 3,6);

assert all(S,s->checkIncidenceSolution(s, SchPblm))

-- Problem with 3 solutions
-- Problem {4,2} {4,2} {4,2} in Gr(3,9)
SchPblm = {
    ({4,2}, random(FFF^9,FFF^9)),
    ({4,2}, random(FFF^9,FFF^9)),
    ({4,2}, random(FFF^9,FFF^9))    
    }

S = solveSchubertProblem(SchPblm, 3,9);

assert all(S,s->checkIncidenceSolution(s, SchPblm))

-- Problem with 5 solutions
-- {5,3,1}*{4,2,1}*{4,3,1} in G(4,10)
SchPblm = {
    ({5,3,1}, random(FFF^10,FFF^10)),
    ({4,2,1}, random(FFF^10,FFF^10)),
    ({4,3,1}, random(FFF^10,FFF^10))    
    }

S = solveSchubertProblem(SchPblm, 4,10);

assert all(S,s->checkIncidenceSolution(s, SchPblm))


end

restart
load "TripleIntersection.m2"

