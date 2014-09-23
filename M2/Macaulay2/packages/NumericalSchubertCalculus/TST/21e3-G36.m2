needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

n=6
M = MovingFlag'at'Root n
Minv = solve(M, id_(FFF^n))
T1 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S1 = solve(T1, id_(FFF^n))
Idop = rsort id_(FFF^n)
F1 = S1*Idop

-- a problem with 2 solutions
S = solveInternalProblem((3,6),{2,1},{2,1},{({2,1},F1)})
s1 = first S
s1
s2 = last S

assert checkIncidenceSolution(s1,{({2,1},M),({2,1},(id_(FFF^n))), ({2,1},F1)})
assert checkIncidenceSolution(s2,{({2,1},M),({2,1},(id_(FFF^n))), ({2,1},F1)})

end

restart
load "21e3-G36.m2"
