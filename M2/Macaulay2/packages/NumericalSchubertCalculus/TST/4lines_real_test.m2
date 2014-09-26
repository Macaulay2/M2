debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 4
DBG = 2

-- problem of 4 lines w.r.t. id, idop, real flags
Pblm = {({1},id_(FFF^4)),
   ({1},rsort id_(FFF^4)), 
   ({1},sub(transpose matrix {{1,1,1,1},{0,1,2,3},{0,0,2,6},{0,0,0,1}},FFF)),
   ({1},sub(transpose matrix {
       {1,2,3,5}, 
       {0,1,7,11}, 
       {0,0,1,13}, 
       {0,0,0,1}},FFF))
};

S = solveSchubertProblem(Pblm, 2,4)

-- we need to make a column reduction
-- to see if these solutions are real
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
	))

assert all(flatten flatten (Sreduced/entries), isReal)
assert all(S,s->checkIncidenceSolution(s,Pblm))

S2 = solveSchubertProblem(Pblm, 2,4, LinearAlgebra=>false)
assert all(S2,s->checkIncidenceSolution(s,Pblm))

Sreduced = apply(S2, s->(
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
	))
assert all(flatten flatten (Sreduced/entries), isReal)

end

restart
load "4lines_real_test.m2"
