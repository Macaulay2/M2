--0) seeding tests

TEST ///
setRandomSeed 2 
T = CC[a_1..a_6][x_1,x_2,lambda]
f_1 = a_1*x_1+a_2*x_2 - x_1*lambda 
f_2 = a_3*x_1+a_4*x_2 - x_2*lambda
f_3 = a_5*x_1+a_6*x_2 + 1
H = {f_1,f_2,f_3}
(x0, p0) = createSeedPair polySystem H -- checks assertion

declareVariable \ {A,B,C, t}
P=gateSystem(matrix{{A,B,C}},matrix{{t}},transpose matrix{{A-t,B-t^2,C-t^3}})
(p0, x0) = createSeedPair P
///


-- 1) static monodromy tests

TEST ///
setRandomSeed 0
R = CC[a,b,c,d,e,f,g,h][A,B,C];
polys = polySystem {
	a*A+b*B+c*C,
	d*A*B+e*B*C+f*C*A,
	g*A*B*C-h*1};
(p0,x0) = createSeedPair polys;
count = 6;

--The first set of tests may not find all solutions, as there is no
--target root count.

(V,npaths) = monodromySolve(polys, NumberOfNodes=>3);
assert( length V.PartialSols == count );

(V,npaths) = monodromySolve(polys, NumberOfNodes=>3, "new tracking routine" => false);
assert( length V.PartialSols == count );

(V,npaths) = monodromySolve(polys,p0,{x0},NumberOfNodes=>3);
assert( length V.PartialSols == count );

setRandomSeed 0
--NumberOfNodes, NumberOfEdges, NumberOfRepeats
(V,npaths) = monodromySolve(polys,p0,{x0},
	NumberOfNodes=>2,
	NumberOfEdges=>5,
	NumberOfRepeats=>11);
assert( length V.PartialSols == count );

--Two options for SelectEdgeAndDirection. If SelectBestEdgeAndDirection, then
--must also provide a Potential function.
(V,npaths) = monodromySolve(polys,p0,{x0},
    	    	NumberOfNodes=>3,
		NumberOfEdges=>5,
		SelectEdgeAndDirection=>selectRandomEdgeAndDirection);
assert( length V.PartialSols == count );

(V,npaths) = monodromySolve(polys,p0,{x0},
	NumberOfNodes=>3,	
	SelectEdgeAndDirection=>selectBestEdgeAndDirection,
	Potential=>potentialLowerBound);
assert( length V.PartialSols == count );

--Two different GraphInitFunctions. Also, BatchSize can be set,
--which will change the number of paths tracked simultaneously.
(V,npaths) = monodromySolve(polys,p0,{x0},
	GraphInitFunction=>flowerGraphInit,
	NumberOfEdges=>5);
assert( length V.PartialSols == count );

setRandomSeed 0
(V,npaths) = monodromySolve(polys,p0,{x0},
	NumberOfEdges=>5,
	GraphInitFunction=>completeGraphInit,
	BatchSize=>1);
assert( length V.PartialSols == count );

--NumberOfEdges=>1 (no randomization)
(V,npaths) = monodromySolve(polys,p0,{x0},
	NumberOfNodes=>10,
	NumberOfEdges=>1);
assert( length V.PartialSols == count );

--The next two tests test booleans: "new tracking routine" (defaults to true)
--and Verbose (defaults to false). We test that both the defaults work
--and that non-default values work.
(V,npaths) = monodromySolve(polys,p0,{x0},
		NumberOfEdges=>4,
		NumberOfNodes=>3,
		"new tracking routine"=>false,
		Verbose=>false);
assert( length V.PartialSols == count );

--The next three tests use strict equality, as they ought to always succeed.
--Can provide TargetSolutionCount
(V,npaths) = monodromySolve(polys,p0,{x0},
	SelectEdgeAndDirection=>selectBestEdgeAndDirection,
	Potential=>potentialE,
	NumberOfNodes=>3,
	NumberOfEdges=>3,
	TargetSolutionCount=>count);
assert( length V.PartialSols == count );
///

///TEST
-- this tests the Equivalencer option and the new (v1.13) quadratic parameter homotopy
setRandomSeed 0
m = 4
n = 2
declareVariable \ {t_1,t_2,u_0,u_1,u_2,u_3}
paramMatrix = gateMatrix{{u_0,u_1,u_2,u_3}}
varMatrix = gateMatrix{{t_1,t_2}}

phi = transpose gateMatrix{{t_1^3, t_1^2*t_2, t_1*t_2^2, t_2^3}}
phiEval = gateSystem(varMatrix, phi)
assert(m==numrows phi)
--distance = sum for i from 0 to 2 list (u_i-phi_(0,i))^2
loss = sum for i from 0 to 3 list (u_i - phi_(i,0))^2
dLoss = diff(varMatrix, gateMatrix{{loss}})
G = gateSystem(paramMatrix,varMatrix,transpose dLoss)
(u0, x0) = createSeedPair G
norm evaluate(G,u0,x0)
(P, p0, x0) = (G, u0, x0)
(p1, x1s) = solveFamily(G, Equivalencer=>(x-> point evaluate(phiEval, x)))
assert(length points x1s == 7)
///

-- 2) dynamic monodromy tests
///TEST
setRandomSeed 0
R = CC[a,b,c,d,e,f,g,h][A,B,C];
polys = polySystem {
	a*A+b*B+c*C,
	d*A*B+e*B*C+f*C*A,
	g*A*B*C-h*1};
(p0,x0) = createSeedPair polys;
count = 6;

--Set dynamic options. Need to provide an AugmentGraphFunction and
--the AugmentEdgeCount and/or AugmentNodeCount should be greater than 0 if
--any augmenting is going to happen. AugmentNumberOfRepeats can be used to
--keep it from running indefinitely.
(V,npaths) = monodromySolve(polys,p0,{x0},
	SelectEdgeAndDirection=>selectBestEdgeAndDirection,
	GraphInitFunction=>completeGraphInit,
	AugmentGraphFunction=>completeGraphAugment,
	AugmentNodeCount=>1,
	AugmentNumberOfRepeats=>10);
assert( length V.PartialSols == count );
(V,npaths) = monodromySolve(polys,p0,{x0},
	SelectEdgeAndDirection=>selectBestEdgeAndDirection,
	GraphInitFunction=>flowerGraphInit,
	AugmentGraphFunction=>flowerGraphAugment,
	AugmentEdgeCount=>1,
	AugmentNumberOfRepeats=>10);
assert( length V.PartialSols == count );

-- test for sparseSolver which sometimes fails: many repeats is there to reduce failure probability, but might slow tests down
S = CC[x,y]
P = polySystem {(x-ii)^2+y^2-1, x+1-y^2}
sols = sparseMonodromySolve(P, NumberOfEdges=>10, NumberOfRepeats=>20)
assert (#sols == 4) 
assert all(sols,s->norm evaluate(P,s) < 0.0001)
///
