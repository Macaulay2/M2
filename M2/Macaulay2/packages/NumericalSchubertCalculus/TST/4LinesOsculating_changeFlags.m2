debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0

-- TEST the function changeFlags

-- problem of 4 lines w.r.t. random flags

RandomFlags = apply(4,i->random(FFF^4,FFF^4));
SchPblm = {
    ({1},RandomFlags_0), 
    ({1},RandomFlags_1),
    ({1},RandomFlags_2),
    ({1},RandomFlags_3)
    };

time S = solveSchubertProblem(SchPblm, 2,4)
assert all(S,s->checkIncidenceSolution(s,SchPblm))


-- Now we will change Flags to osculating: id, idop, 2 osculating flags
OsculatingFlags = {
    id_(FFF^4),
    rsort id_(FFF^4), 
    sub(transpose matrix {{1,1,1,1},{0,1,2,3},{0,0,2,6},{0,0,0,1}},FFF),
    sub(transpose matrix {{1,2,4,8}, {0,1,4,12}, {0,0,1,6}, {0,0,0,1}},FFF)
    };

--prepare Change of Flags
conditions =toList(4:{1}); 
Sosc = changeFlags(S,(conditions,RandomFlags,OsculatingFlags))
oscPblm = apply(OsculatingFlags, f->({1},f))
assert all(Sosc, s->checkIncidenceSolution(s,oscPblm))

///
brack = partition2bracket({1},2,4)
Sreduced = apply(Sosc,s->clean_0.001 columnReduce(s,brack))
///
    
-- we need to make a column reduction
-- to see if these solutions are real
Sreduced = apply(Sosc, s->(
	M1:= matrix{
	    {s_(0,0)^(-1), -s_(0,1)*s_(0,0)^-1},
	    {0, 1}};
	s1 := s*M1;
	M2 := matrix{
	    {1,0},
	    {-s1_(3,0)*s1_(3,1)^-1 ,s1_(3,1)^-1 }
	    };
	s2 := clean(0.001, s1*M2);
	s2
	))

assert all(flatten flatten (Sreduced/entries), isReal)

assert all(Sreduced, s->checkIncidenceSolution(s,oscPblm))

end

restart
load "NumericalSchubertCalculus/TST/4linesOsculating_changeFlags.m2"
