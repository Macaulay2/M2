-- NumSchCalcTEST.m2
restart
loadPackage ("NumericalSchubertCalculus", FileName=>currentFileDirectory|"/../NumericalSchubertCalculus.m2")
setRandomSeed 2


-- this should return local coords w.r.t. Id and op.Id
-- June4,2014: right now, it returns M*s where
-- M is the moving flag and s is a solution
-- to L1 vs L2 wrt Id and opId respectivamente
solveInternalProblem((2,4),{2,1},{1},{})
solveInternalProblem((3,6),{2,1},{3,2,1},{})
solveInternalProblem((3,6),{3,2,1},{2,1},{})
-- this should return {}
solveInternalProblem((2,4),{1,1},{2},{})
solveInternalProblem((3,6),{2,1,1},{3,1},{})
solveInternalProblem((3,6),{3,2,1},{1,1},{})

-- this should call resolveNode
---------------------------------
restart
loadPackage ("NumericalSchubertCalculus", FileName=>currentFileDirectory|"/../NumericalSchubertCalculus.m2")
setRandomSeed 2

n=4
--n = 6
M = MovingFlag'at'Root n
Minv = solve(M, id_(FFF^n))
T1 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S1 = solve(T1, id_(FFF^n))
Idop = rsort id_(FFF^n)
F1 = S1*Idop

--F1 = S1*sub(M,FFF)
S = solveInternalProblem((2,4),{1,1},{1,0},{({1,0},F1, T1)})
s = first S
checkIncidenceSolution(s, {({1,1},M), ({1,0},rsort(id_(FFF^n))), ({1,0},F1)})

-- --- TEST June 15 ------------
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
checkIncidenceSolution(s,{({2,1},rsort(id_(FFF^n)))})
checkIncidenceSolution(s,{({3,2,1},M), ({2,0},rsort(id_(FFF^n)))})

S = solveInternalProblem((3,6),{2,1},{2,1},{({2,1},F1, T1)})
s = first S
s

-------- end of TEST june 15 ------------------

T2 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S2 = solve(T2, id_(FFF^n))
--F2 = S1*sub(M,FFF)
F12 = S2*Minv*F1
s2 = clean_0.0001 (S2*Minv*s)
F2 = S2*Minv*Idop

S2 = solveInternalProblem((2,4),{1,0},{1,0},{({1,0},F2, T2), ({1,0},F12,T1)})
s2 = first S2
checkIncidenceSolution(s, {({1,1},M), ({1,0},id_(FFF^n)), ({1,0},F1)})

checkIncidenceSolution(s2,  {({1,1},id_(FFF^n)), ({1,0},S2*Minv), ({1,0},F12)})

checkIncidenceSolution(s2,  {({1,1},id_(FFF^n)), ({1,0},S2*Minv), ({1,0},F12)})

--------
T1 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S1 = solve(T1, id_(FFF^n))
T2 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S2 = solve(T2, id_(FFF^n))
F2 = S1*Minv
F1 = S2*Minv*S1*Idop
--F12 =S2*Minv*F1


S = solveInternalProblem((2,4), {1,0}, {1,0}, {({1,0}, F2, T2),({1,0},F1,T1)})







--solveInternalProblem((2,4),{1,0},{1,0},{})

s = M*first S
checkIncidenceSolution(s,{({1,1},M),({1},id_(FFF^n)),({1},F1)})

------------------------
setRandomSeed 2
n=4
T1 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S1 = solve(T1, id_(FFF^n))
F1 = S1*sub(M,FFF)
T2 = id_(FFF^n)+random(FFF^n,FFF^n,UpperTriangular=>true)
S2 = solve(T2, id_(FFF^n))
F2 = S1*S2*sub(M,FFF)
S = solveInternalProblem((2,4),{1,0},{1,0},{({1,0},F1, T1), ({1,0},F2,T2)})
s = first S
checkIncidenceSolution(s,{({1},M),({1},id_(FFF^n)),({1},F1), ({1},F2)})




-- this one should break as the flags are not in general position
-- so it shouldn't have equations
solveInternalProblem((2,4),{1,1},{1},{({1},id_(FFF^4), id_(FFF^4))})

solveInternalProblem((3,6),{2,1},{3,1},{})


(k,n)=(3,6);
-- OK
l1 = {2,1,0}
l2 = {3,2,1}
checkOrthogonal=l1+reverse l2 
(l1+reverse l2)/(i->n-k-i)
l1 = {2,1,0}
l2 = {3,1,0}
checkOrthogonal=l1+reverse l2 
(l1+reverse l2)/(i->n-k-i)
-- not OK
l1 = {2,1,0}
l2 = {3,3,0}
checkOrthogonal=l1+reverse l2 
(l1+reverse l2)/(i->n-k-i)
l1 = {2,1,1}
l2 = {3,1,0}
checkOrthogonal=l1+reverse l2 
(l1+reverse l2)/(i->n-k-i)



-------------------------------------------
SchPblm = {({2},random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6)), 
    ({2},random(FFF^6,FFF^6)), 
    ({2,1},random(FFF^6,FFF^6))};

t1 = cpuTime();
S1=solveSchubertProblem(SchPblm,3,6);
t2 = cpuTime();

SchPblm =reverse SchPblm;
t3 = cpuTime();
S2=solveSchubertProblem(SchPblm,3,6);
t4 = cpuTime();

<<"time it consumed first run: "<< t2-t1<<endl;
<<"time it consumed second run: "<< t4-t3<<endl;
<< "Solutions 1"<<endl<< S1<<endl;
<< "Solutions 2"<<endl<<S2<<endl;

S1reduced = apply(S1, s->(
	k:=numColumns(s);
    	b:= id_(CC^k);
    	T:= solve(submatrix(s,0..k-1,0..k-1),b);
    	clean(0.001,s*T)
	));
S1reduced

S2reduced = apply(S2, s->(
	k:=numColumns(s);
    b:= id_(CC^k);
    T:= solve(submatrix(s,0..k-1,0..k-1),b);
    clean(0.001,s*T)
	));
S2reduced

-- Problem of 4 osculating lines (real solutions)
Pblm = {({1},id_(FFF^4)),
    ({1},rsort id_(FFF^4)), 
    ({1},transpose matrix {{1,1,1,1},{0,1,2,3},{0,0,2,6},{0,0,0,1}}),
    ({1},transpose matrix {{1,2,4,8}, {0,1,4,12}, {0,0,1,6}, {0,0,0,1}})}
solveSchubertProblem(Pblm, 2,4)

-- 1^10 = 42 in G(2,7)
setRandomSeed 2

Pblm = {({1}, random(FFF^7,FFF^7)),
    ({1}, random(FFF^7,FFF^7)),
    ({1}, random(FFF^7,FFF^7)),
    ({1}, random(FFF^7,FFF^7)),
    ({1}, random(FFF^7,FFF^7)),
    ({1}, random(FFF^7,FFF^7)),
    ({1}, random(FFF^7,FFF^7)),
    ({1}, random(FFF^7,FFF^7)),
    ({1}, random(FFF^7,FFF^7)),
    ({1}, random(FFF^7,FFF^7))
    };
-- increase the recursion level:
recursionLimit = 1000;
solveSchubertProblem(Pblm, 2,7)


-- 1^8 = 14 in G(2,6)
setRandomSeed 2

Pblm = {({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6))
    };
solveSchubertProblem(Pblm, 2,6)


-- 1^6 = 5 in G(2,5)
setRandomSeed 2

Pblm = {({1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5)),
    ({1}, random(FFF^5,FFF^5))
    };
solveSchubertProblem(Pblm, 2,5)


-- Problem of 4 osculating lines (real solutions)
Pblm = {({1},id_(FFF^4)),
    ({1},rsort id_(FFF^4)), 
    ({1},transpose matrix {{1,1,1,1},{0,1,2,3},{0,0,2,6},{0,0,0,1}}),
    ({1},transpose matrix {{1,2,4,8}, {0,1,4,12}, {0,0,1,6}, {0,0,0,1}})}
solveSchubertProblem(Pblm, 2,4)
-- we need to make a column reduction
-- to see if these solutions are real
S = oo;
Sreduced = apply(S, s->(
	M1:= matrix{
	    {s_(0,0)^(-1), -s_(0,1)*s_(0,0)^-1},
	    {0, 1}};
	s1 := clean_0.001 s*M1;
	M2 := matrix{
	    {1,0},
	    {-s1_(3,0)*s1_(3,1)^-1 ,s1_(3,1)^-1 }
	    };
	s2 := clean(0.001, s1*M2);
	s2
	));
Sreduced

setRandomSeed 2
SchPblm = {({1},id_(FFF^4)), 
    ({1},random(FFF^4,FFF^4)),
    ({1},random(FFF^4,FFF^4)), 
    ({1},random(FFF^4,FFF^4))};
S=solveSchubertProblem(SchPblm,2,4);
S

setRandomSeed 2
SchPblm = {({2,1},random(FFF^6,FFF^6)),
    ({2,1},random(FFF^6,FFF^6)), 
    ({2,1},random(FFF^6,FFF^6))};

S=solveSchubertProblem(SchPblm,3,6);
S

setRandomSeed 2
Pblm={({1},random(FFF^6,FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
     ({2,1},random(FFF^6,FFF^6)),
    ({2,1},random(FFF^6,FFF^6))};
S = solveSchubertProblem(Pblm,3,6);
S


setRandomSeed 2
Pblm={({1},random(FFF^6,FFF^6)),
     ({2},random(FFF^6,FFF^6)),
     ({2,1},random(FFF^6,FFF^6)),
    ({2,1},random(FFF^6,FFF^6))};
S = solveSchubertProblem(Pblm,3,6);


--- Fails here
restart
loadPackage ("NumericalSchubertCalculus", FileName=>currentFileDirectory|"/../NumericalSchubertCalculus.m2")

setRandomSeed 2
Pblm={({1},random(FFF^6,FFF^6)),
    ({1}, random(FFF^6,FFF^6)),
     ({1},random(FFF^6,FFF^6)),
     ({2,1},random(FFF^6,FFF^6)),
    ({2,1},random(FFF^6,FFF^6))};
S = solveSchubertProblem(Pblm,3,6);


quit

----------------------
