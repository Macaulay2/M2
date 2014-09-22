-- problems in Gr(3,6)
n=6
M = MovingFlag'at'Root n
T1 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S1 = solve(T1, id_(FFF^n))
Idop = rsort id_(FFF^n)
F1 = S1*Idop

-- Problem 1: {3,2,1} {1,1} {1}
S = solveInternalProblem((3,6),{3,2,1},{1,1},{({1,0},F1, T1)})
s = first S
s
checkIncidenceSolution(s, {({3,2,1},M), ({1,1},rsort(id_(FFF^n))), ({1,0},F1)})

-- Problem 2: {2,1} {2,1} {2,1}
-- a problem with 2 solutions

S = solveInternalProblem((3,6),{2,1},{2,1},{({2,1},F1, T1)})
s1 = first S
s1
s2 = last S

checkIncidenceSolution(s1,{({2,1},M),({2,1},(id_(FFF^n))), ({2,1},F1)})
checkIncidenceSolution(s2,{({2,1},M),({2,1},(id_(FFF^n))), ({2,1},F1)})

-- Problem with 3 solutions in Gr(3,9)
-- Problem {4,2} {4,2} {4,2}
n=9
M = MovingFlag'at'Root n
Minv = solve(M, id_(FFF^n))
T1 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S1 = solve(T1, id_(FFF^n))
Idop = rsort id_(FFF^n)
F1 = S1*Idop

S = solveInternalProblem((3,9),{4,2},{4,2},{({4,2},F1,T1)})
s1 = first S
s2 = S_1
s3 = last S

checkIncidenceSolution(s1,{({4,2},M),({4,2},(id_(FFF^n))), ({4,2},F1)})
checkIncidenceSolution(s2,{({4,2},M),({4,2},(id_(FFF^n))), ({4,2},F1)})
checkIncidenceSolution(s3,{({4,2},M),({4,2},(id_(FFF^n))), ({4,2},F1)})

