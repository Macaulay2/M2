-- to execute look below for "EXECUTE...FROM HERE" -------------------------
needsPackage "MonodromySolver"
needsPackage "Permanents"

FF = CC

-- S: number of players
-- n_i: number of strategies for player i
-- N = n_1 + ... + n_S - S

-- we assume that each player (S players total) has the same number of strategies 
-- compute BKK bound for NxN system
bkkBound = method()
bkkBound (ZZ, ZZ) := (S, n) -> (
    N := S*(n-1);
    -- A is an NxN matrix of 1's and 0's
    FF := ZZ;
    A := matrix flatten toList(apply(0..S-1,i -> toList (
		if i == 0 then (
		    for j from 1 to n-1 list join(toList(n-1: 0_FF), toList(N-((n-1)*(i+1)): 1_FF))
		    ) 
		else if  i == S-1 then (
		    for j from 1 to n-1 list join(toList((n-1)*i: 1_FF), toList(n-1: 0_FF))
		    ) 
		else (
		    for j from 1 to n-1 list 
		    join(toList((n-1)*i: 1_FF), toList(n-1: 0_FF), toList(N-(n-1)*(i+1): 1_FF)))
		))
	);
    permA := glynn A;
    Bound := permA//((n-1)!^S);
    Bound
    )

TEST ///
bkkBound(3,3)
bkkBound(4,2)
bkkBound(5,3)
///

-- For S players, each with n strategies, we construct n-1 equations per player
-- We use p_(1,n) = 1-p_(1,1)-...-p_(1,n-1)
-- Polynomial system of N equations with N unknowns

-- 3 players 3 strategies each
getNashSystem = method()
getNashSystem (ZZ,ZZ) := (n, S) -> (
	-- Set up the ring and create all of the indexed variables.
	R := CC[a_(toSequence((for i from 1 to (n+1) list 1)))..a_(toSequence({n} | (for i from 1 to (n) list S)))][p_(1,1)..p_(n,S-1)];

	-- Create all of the p_(i,n).
	for i from 1 to n do (
		p_(i,S) = 1 - (sum for j from 1 to S-1 list p_(i,j));
	);

	-- This isn't very clean. The "a_" subscripts are in two parts: the first part
	-- is (i,j) while the second half is (S,S,..,S) for n S's. subscriptSuffixes
	-- is a list of all of the possible second halves, so they're easy to spin
	-- through when we're multiplying. fullSubscript joins a suffix with a distinct
	-- (i,j) pair. If anyone has a better idea of how to do this, feel free to
	-- implement it.
	subscriptSuffixes := toSequence(for i from 1 to (n-1) list 1)..toSequence(for i from 1 to (n-1) list S);
	for i from 1 to n do (
		for j from 1 to S do (
			P_(i,j) = 0;
			for subscriptSuffix in subscriptSuffixes do (
				fullSubscript := prepend(i,prepend(j,subscriptSuffix));
				val := a_fullSubscript;
				--Create a list of the indices of all of the polynomials
				--except for the i-th polynomial.
				PolyList := delete(i, for q from 1 to n list (q));
				for k from 2 to #fullSubscript - 1 do (
					val = val*p_(PolyList#(k-2), fullSubscript#k);
				);
				P_(i,j) = P_(i,j) + val;
			);
		);
	);

	-- Create the equations. Could be a one liner, but this is more readable.
	Eqs = {};
	for i from 1 to n list (
		for j from 2 to S list (
			Eqs = append(Eqs, {P_(i,1) - P_(i,j)});
		);
	);
	polySystem (matrix Eqs)
);
--------------------------------------------------------------
end 

--------EXECUTE line-by-line FROM HERE -----------------------------------
restart
load "example-Nash.m2"

-- completeGraph(2,3)
setRandomSeed 0 -- this seed fails with defaults
G = getNashSystem(3,3)
(p0, x0) = createSeedPair G
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},NumberOfEdges=>3,NumberOfNodes=>2)
points V.PartialSols
length V.PartialSols

bkkBound(3,3)

options monodromySolve

-- completeGraph(2,10)
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},
    NumberOfNodes=>2
    NumberOfEdges => 10,TargetSolutionCount => bkkBound(3,3),
    SelectEdgeAndDirection => selectBestEdgeAndDirection,
    Potential=>potentialE, 
    Verbose=>true
    )   
points V.PartialSols
length V.PartialSols

-- completeGraph(2,5)
G = getNashSystem(4,3)
(p0, x0) = createSeedPair G
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0}, NumberOfNodes=>2
    NumberOfEdges => 5,TargetSolutionCount => bkkBound(4,3),
    Verbose=>true)   
getTrackTime(V.Graph)
solsM2 = points V.PartialSols;

-- same with Bertini's blackbox
specPolys = specializeSystem (p0,G)
R = CC[x_1..x_(numgens ring first specPolys)]
toR = map(R,ring first specPolys,vars R)
elapsedTime solsB = solveSystem(specPolys/toR, Software=>BERTINI);
