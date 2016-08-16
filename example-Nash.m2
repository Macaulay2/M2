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
    A := matrix flatten toList(apply(0..S-1,i -> toList (
		if i == 0 then (
		    for j from 1 to n-1 list join(toList(n-1: 0_CC), toList(N-((n-1)*(i+1)): 1_CC))
		    ) 
		else if  i == S-1 then (
		    for j from 1 to n-1 list join(toList((n-1)*i: 1_CC), toList(n-1: 0_CC))
		    ) 
		else (
		    for j from 1 to n-1 list 
		    join(toList((n-1)*i: 1_CC), toList(n-1: 0_CC), toList(N-(n-1)*(i+1): 1_CC)))
		))
	);
    permA := glynn A;
    Bound := permA/((n-1)!^S);
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

restart
load "example-Nash.m2"

G = getNashSystem(3,3)
peek G
R = ring G
L = apply(toList(1..numgens R), i -> random(0.,1.))
SubList := apply(toList(0..numgens R-1), i -> (gens R)#i => L#i)
C = coefficientRing ring G
M = sub(sub(G.PolyMap, SubList), C)
N = numericalIrreducibleDecomposition ideal M
c0 = first (first components N).Points
pre0 = point{apply(SubList, i -> i#1)}

elapsedTime sols = monodromySolve(transpose G.PolyMap,c0,{pre0},NumberOfEdges => 5,TargetSolutionCount => bkkBound(3,3))   

J = first sols
E = J.Edges
for e in E do (
    print peek e.Correspondence12;
    print peek e.Correspondence21;
    )
V = J.Vertices
for v in V do (
    print peek v.PartialSols;
    )




--ignore below

pol1 = "- 1.122525605*p3 - 0.1202590019*p3*p2 + 1.078803537*p2" \
         + " + 0.7696590455*p4 + 0.1628390188*p4*p3*p2 - 0.1163105815*p4*p3" \
         + " - 0.5333928492*p4*p2 - 0.6823118878;"
    pol2 = "- 0.4266067331*p3 + 0.9756757996*p4 + 0.646954510E-01*p4*p3" \
         + "- 0.2142068753*p3*p1 + 2.814522341*p1 + 0.3980853635*p4*p3*p1" \
         + "- 2.546266040*p4*p1 - 1.157918209;"
    pol3 = "2.023260100*p2 - 0.1342402078*p4 - 0.3904810713*p4*p2" \
         + " + 0.4274147938*p1 + 0.5269506245*p4*p1 - 3.069473137*p2*p1" \
         + " + 0.3284270487*p4*p2*p1 - 0.9128973443;"
    pol4 = "- 0.6256820158*p3 - 0.1614530476*p3*p2 + 1.080721123*p2" \
         + " + 1.366746822*p3*p1 - 2.347725424*p1 + 0.5941402017*p2*p1" \
         + " - 2.233884240*p3*p2*p1 + 1.026687422;"


