restart
needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

n=4
--n = 6
M = MovingFlag'at'Root n
--Minv = solve(M, id_(FFF^n))
T1 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S1 = solve(T1, id_(FFF^n)) --inverse matrix of T1
Idop = rsort id_(FFF^n)
F1 = S1*Idop

--F1 = S1*sub(M,FFF)
S = solveInternalProblem((2,4),{1,1},{1,0},{({1,0},F1, T1)})
s = first S
checkIncidenceSolution(s, {({1,1},M), ({1,0},rsort(id_(FFF^n))), ({1,0},F1)})

-- --- TEST Sept 21 ------------
restart
needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

n=6
M = MovingFlag'at'Root n
Minv = solve(M, id_(FFF^n))
T1 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S1 = solve(T1, id_(FFF^n))
Idop = rsort id_(FFF^n)
F1 = S1*Idop

--F1 = S1*sub(M,FFF)
S = solveInternalProblem((3,6),{3,2,1},{1,1},{({1,0},F1, T1)})
s = first S
s
checkIncidenceSolution(s, {({3,2,1},M), ({1,1},rsort(id_(FFF^n))), ({1,0},F1)})


restart
needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

-- a problem with 2 solutions
S = solveInternalProblem((3,6),{2,1},{2,1},{({2,1},F1, T1)})
s1 = first S
s1
s2 = last S

checkIncidenceSolution(s1,{({2,1},M),({2,1},(id_(FFF^n))), ({2,1},F1)})
checkIncidenceSolution(s2,{({2,1},M),({2,1},(id_(FFF^n))), ({2,1},F1)})

-- a problem with 3 solutions
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

--Thus, our software solve the schubert problem l_1, l_2, l_3
-- with respect to the flags: M, Id, F_1

-------- end of TEST Sept 21 ------------------

-- Now we test for 4 conditions
-- problem of 4 lines
restart
needsPackage "NumericalSchubertCalculus"
setRandomSeed 2

n=4

M = MovingFlag'at'Root n
Minv = solve(M, id_(FFF^n))
T1 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S1 = solve(T1, id_(FFF^n))
Idop = rsort id_(FFF^n)
F1 = S1*Idop

T2 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S2 = solve(T2, id_(FFF^n))
--F2 = S1*sub(M,FFF)
F12 = S2*Minv*F1
--s2 = clean_0.0001 (S2*Minv*s)
F2 = S2*Minv*Idop

S2 = solveInternalProblem((2,4),{1,0},{1,0},{({1,0},F2, T2), ({1,0},F12,T1)})
