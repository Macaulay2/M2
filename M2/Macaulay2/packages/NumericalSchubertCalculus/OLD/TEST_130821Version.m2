-- NumSchCalcTEST.m2
restart
needsPackage "NumericalSchubertCalculus"
setRandomSeed 2
-- Problem of 4 osculating lines (real solutions)
Pblm = {({1},id_(FFF^4)),
    ({1},rsort id_(FFF^4)), 
    ({1},transpose matrix {{1,1,1,1},{0,1,2,3},{0,0,2,6},{0,0,0,1}}),
    ({1},transpose matrix {{1,2,4,8}, {0,1,4,12}, {0,0,1,6}, {0,0,0,1}})}
solveSchubertProblem(Pblm, 2,4)

-- 1^10 = 42 in G(2,7)
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

SchPblm = {({1},id_(FFF^4)), 
    ({1},random(FFF^4,FFF^4)),
    ({1},random(FFF^4,FFF^4)), 
    ({1},random(FFF^4,FFF^4))};
S=solveSchubertProblem(SchPblm,2,4);
S

SchPblm = {({2,1},random(FFF^6,FFF^6)),
    ({2,1},random(FFF^6,FFF^6)), 
    ({2,1},random(FFF^6,FFF^6))};
S=solveSchubertProblem(SchPblm,3,6);
S

--- Fails here
Pblm={({1},random(FFF^6,FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
     ({2,1},random(FFF^6,FFF^6)),
    ({2,1},random(FFF^6,FFF^6))};
S = solveSchubertProblem(Pblm,3,6);
S
quit

----------------------
