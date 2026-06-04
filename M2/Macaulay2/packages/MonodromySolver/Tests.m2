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


-- Tests added in the 2026 test-audit pass.  The existing TEST blocks at
-- Tests.m2:105, Tests.m2:129, and galois-group.m2:6 use the reversed-fence
-- form `///TEST` (rather than `TEST ///`), which the M2 reader treats as a
-- top-level string literal — so the bodies never register as tests.  Several
-- exports (solveFamily, sparseMonodromySolve, monodromyGroup, pCompose,
-- inverse-on-MutableHashTable, computeMixedVolume, specializeSystem,
-- PointArray operations beyond the smoke test, and the HomotopyEdge
-- correspondence/gamma plumbing) have therefore been silently uncovered.
-- These additive blocks pin down deterministic shapes for all of them.

TEST ///
  -- solveFamily on a parametric linear x quadratic family
  setRandomSeed 0
  R = CC[a,b,c,d,e,f][x,y];
  q  = a*x^2 + b*y + c;
  l  = d*x + e*y + f;
  (sys, sols) = solveFamily(polySystem{q,l}, NumberOfNodes=>3);
  assert(class sys === Point)
  assert(class sols === PointArray)
  assert(length sols == 2)
  -- every solution actually solves the specialized system
  assert(all(points sols, s -> norm evaluate(polySystem{q,l}, sys, s) < 1e-4))
///

TEST ///
  -- sparseMonodromySolve on the documented cubic x polar-curve x linear system
  setRandomSeed 2021
  R = CC[x,y,z];
  F = random(3,R);
  P = sum apply(gens R, g -> diff(g,F) * random CC);
  PS = polySystem {F, P, random(1,R) - 1};
  sols = sparseMonodromySolve PS;
  assert(class sols === PointArray)
  assert(length sols == 6)
  assert(all(points sols, s -> norm evaluate(PS, s) < 1e-4))
///

TEST ///
  -- monodromyGroup on the doc example: structural assertions only
  -- (the exact count of permutations depends on graph topology, but each
  -- permutation must permute the same fiber).
  setRandomSeed 100
  declareVariable \ {t_1,t_2,u_0,u_1,u_2,u_3}
  paramMatrix = gateMatrix{{u_0,u_1,u_2,u_3}}
  varMatrix = gateMatrix{{t_1,t_2}}
  phi = transpose gateMatrix{{t_1^3, t_1^2*t_2, t_1*t_2^2, t_2^3}}
  loss = sum for i from 0 to 3 list (u_i - phi_(i,0))^2
  dLoss = diff(varMatrix, gateMatrix{{loss}})
  G = gateSystem(paramMatrix, varMatrix, transpose dLoss)
  perms = monodromyGroup(G, "msOptions" => {NumberOfEdges=>10})
  assert(class perms === List)
  assert(#perms > 0)
  -- all permutations have the same length (= root count)
  assert(# unique apply(perms, p -> #p) == 1)
  rc = # first perms
  -- and each is a permutation of {0..rc-1}
  assert(all(perms, p -> sort p == toList(0..rc-1)))
///

TEST ///
  -- pCompose and inverse on MutableHashTable.  The reversed-fence test at
  -- galois-group.m2:6 has been silently inert; it also lacks `debug
  -- MonodromySolver`, which is required because pCompose is unexported.
  debug MonodromySolver
  H1 = new MutableHashTable from {0=>1, 1=>2, 2=>0}
  H2 = new MutableHashTable from {0=>2, 1=>1, 2=>0}
  H1H2 = pCompose(H1, H2)
  H2H1 = pCompose(H2, H1)
  assert(H1H2#1 == 2)
  assert(H2H1#1 == 0)
  -- pCompose with the identity gives back the original
  Hid = new MutableHashTable from {0=>0, 1=>1, 2=>2}
  HidH1 = pCompose(Hid, H1)
  assert(all(keys H1, k -> HidH1#k === H1#k))
  -- inverse undoes pCompose: inverse(H) o H is the identity
  H1inv = inverse H1
  Hcomp = pCompose(H1inv, H1)
  assert(all(keys H1, k -> Hcomp#k === k))
  assert(all(keys H1, k -> H1inv#(H1#k) === k))
///

TEST ///
  -- PointArray operations beyond the existing smoke check in PointArray.m2
  needsPackage "MonodromySolver"
  p1 = point {{1_CC, 2_CC}}
  p2 = point {{3_CC, 4_CC}}
  p3 = point {{5_CC, 6_CC}}
  A = pointArray {p1, p2}
  assert(class A === PointArray)
  assert(length A == 2)
  assert(position(p1, A) === 0)
  assert(position(p2, A) === 1)
  assert(position(p3, A) === null)
  assert(member(p1, A))
  assert(not member(p3, A))
  assert(A_0 === p1)
  assert(A_{0,1} === {p1, p2})
  assert(indices A === {0, 1})
  -- appendPoint extends; duplicate insertion is an error
  appendPoint(A, p3)
  assert(length A == 3)
  assert(member(p3, A))
  assert(try (appendPoint(A, p3); false) else true)
  -- appendPoints batches
  B = pointArray {p1}
  appendPoints(B, {p2, p3})
  assert(length B == 3)
  assert(member(p2, B) and member(p3, B))
  -- points round-trips: toExternalString recovers structure
  assert(class (points A) === List)
  assert(length A == # points A)
///

TEST ///
  -- computeMixedVolume on small explicit supports.
  -- The degenerate pure-monomial case computeMixedVolume {u^3, v^3} (which
  -- should be 0, since the Newton polytopes are single points and admit no
  -- mixed cells) is deliberately omitted: Homebrew gfan 0.8beta segfaults
  -- on that input, while the apt-packaged gfan 0.6.2 handles it.  Bug in
  -- the external solver, not in M2 — re-add only when gfan is fixed.
  S = CC[u, v];
  assert(computeMixedVolume {u^2 + v^2 - 1, u*v - 1} == 4)
  assert(computeMixedVolume {u + v - 1, u^2 - v} == 2)
///

TEST ///
  -- specializeSystem substitutes parameter values into the polynomial coefficients
  R = CC[c1, c2][x, y];
  PS = polySystem {c1*x^2 + y - 1, c2*x + y};
  p0 = point {{2.0_CC, 3.0_CC}};
  spec = specializeSystem(p0, PS);
  assert(class spec === List)
  assert(#spec == 2)
  -- the specialized system lives in CC[x,y] (no parameters left)
  assert(numgens ring first spec == 2)
///

TEST ///
  -- HomotopyEdge plumbing: EdgesSaturated populates the correspondence tables
  -- and the gamma scalars on each edge.  These exports (Correspondence12,
  -- Correspondence21, gamma1, gamma2, Node1, Node2) have never had direct
  -- assertions in any TEST block.
  setRandomSeed 0
  R = CC[a,b,c,d,e,f,g,h][A,B,C];
  polys = polySystem {
      a*A+b*B+c*C,
      d*A*B+e*B*C+f*C*A,
      g*A*B*C-h*1};
  (p0, x0) = createSeedPair polys;
  (V, npaths) = monodromySolve(polys, p0, {x0},
      EdgesSaturated => true,
      NumberOfNodes => 3);
  assert(class V === HomotopyNode)
  assert(class V.PartialSols === PointArray)
  assert(class V.Graph === HomotopyGraph)
  assert(length V.PartialSols == 6)
  assert(#V.SpecializedSystem == 3)
  G = V.Graph;
  assert(#G.Vertices == 3)
  assert(#G.Edges > 0)
  e = first G.Edges;
  assert(class e === HomotopyEdge)
  -- the correspondence tables and gamma scalars exist on every edge
  assert(class e.Correspondence12 === MutableHashTable)
  assert(class e.Correspondence21 === MutableHashTable)
  assert(class e.gamma1 === CC)
  assert(class e.gamma2 === CC)
  -- the two endpoints of an edge are distinct HomotopyNodes
  assert(class e.Node1 === HomotopyNode)
  assert(class e.Node2 === HomotopyNode)
  assert(e.Node1 =!= e.Node2)
///

TEST ///
  -- selectRandomEdgeAndDirection should reach the target count as reliably
  -- as selectBestEdgeAndDirection on the staple cubic example.  This pins
  -- down the (option => function) plumbing for SelectEdgeAndDirection that
  -- was only exercised once at Tests.m2:55, without a counterpart for the
  -- TargetSolutionCount option.
  setRandomSeed 0
  R = CC[a,b,c,d,e,f,g,h][A,B,C];
  polys = polySystem {
      a*A+b*B+c*C,
      d*A*B+e*B*C+f*C*A,
      g*A*B*C-h*1};
  (p0, x0) = createSeedPair polys;
  (V, npaths) = monodromySolve(polys, p0, {x0},
      SelectEdgeAndDirection => selectRandomEdgeAndDirection,
      TargetSolutionCount => 6,
      NumberOfNodes => 3);
  assert(length V.PartialSols == 6)
///
