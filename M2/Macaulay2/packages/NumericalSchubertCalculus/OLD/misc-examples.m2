restart 
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0
-----------------------
-- 4 lines in P^3
SchPblm = {({1},id_(FFF^4)), 
    ({1},random(FFF^4,FFF^4)),
    ({1},random(FFF^4,FFF^4)), 
    ({1},random(FFF^4,FFF^4))};

solveSchubertProblem(SchPblm,2,4)

restart 
debug needsPackage "NumericalSchubertCalculus"
-- setRandomSeed 2

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


-- Problem (2,1)^3=2 in G(3,6)
SchPblm = {({2,1},random(FFF^6,FFF^6)), ({2,1},random(FFF^6,FFF^6)),({2,1},random(FFF^6,FFF^6))}
solveSchubertProblem(SchPblm,3,6)
-- not a simple tree

-- Problem (2,1)*(2)^3 = 2 in G(3,6) 
-- This problem has a non-trivial tree (not like the problem of 4 lines)
Pblm={({2},random(FFF^6,FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6)), 
    ({2,1},random(FFF^6,FFF^6))}
solveSchubertProblem(Pblm,3,6)

-- Problem (1)*(2)*(2,1)^2 = 3 in G(3,6)
Pblm={({1},random(FFF^6,FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
    ({2,1},random(FFF^6,FFF^6))}
solveSchubertProblem(Pblm,3,6)
-- code breaks here
----------------------


root = playCheckers({1},{1},2,4)
resolveNode(root, {({1},random(FFF^4,FFF^4)), ({1},random(FFF^4,FFF^4))})
assert(#root.Solutions==2)

root = playCheckers({2,1,0},{2,1,0},3,6)
time resolveNode(root, {({2,1,0},random(FFF^6,FFF^6))})
assert(#root.Solutions==2)
peek root
-- test code and assertions here
-- may have as many TEST sections as needed

-- Problem (2,1)^2*(1)*(2) = 3 in G(3,6)
root = playCheckers({2,1},{2,1},3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({1},random(FFF^6,FFF^6))})
assert(#root.Solutions==3)
peek root
printTree root
-- Problem (2,1)*(2)^3 = 2 in G(3,6) 
-- This problem has a non-trivial tree (not like the problem of 4 lines)
root = playCheckers({2},{2},3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({2,1},random(FFF^6,FFF^6))})
assert(#root.Solutions == 2)
peek root
printTree root
///

end
check "NumericalSchubertCalculus"

-- EXAMPLES (see TEST section for more simple examples)
restart
setRandomSeed 0
--debug 
needsPackage "NumericalSchubertCalculus";

root = playCheckers({2,1},{2,1},3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({1},random(FFF^6,FFF^6))})
peek root

DEBUG'LEVEL = 1 --check that DebugLevel = 1 should do the black box solving only at the leaves
n=7; K'n=FFF^n; -- takes about 10 minutes!
root = playCheckers({2,1,0},{2,1,0},3,n)
time resolveNode(root, {({2,1,0},random(K'n,K'n)),({2,1,0},random(K'n,K'n))})
peek root
printTree root

root = playCheckers({2,1}, {2}, 3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({2},random(FFF^6,FFF^6))})
peek root
printTree root

---- there is something wrong, this problem gives an error
root = playCheckers({3,2,2},{2}, 3,6)
peek root
resolveNode(root, {})


restart
setRandomSeed 0
debug needsPackage "LRcheckergame";

-- we test if the resolveNode function
-- can just solve the problem when 
-- the Schubert problem consist of two
-- complementary partitions only
root = playCheckers({3,3,1},{2},3,6)
resolveNode(root,{})
peek root

-- this problem should give empty solutions as
-- the two partition are not complementary
root = playCheckers({3,3,1}, {1,1}, 3,6)
resolveNode(root, {({2,1,0},random(FFF^6,FFF^6))})


