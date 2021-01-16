newPackage(   
    "SlackIdeals",
    Version => "1.0",
    Date => "March 15, 2020",
    Authors => {{Name => "Amy Wiebe", Email => "w.amy.math@gmail.com"},
	{Name => "Antonio Macchia", Email => "macchia.antonello@gmail.com"}},
    Headline => "Slack ideals of polytopes and matroids",
    Keywords => {"Commutative Algebra", "Matroids"},
    PackageExports => {"Polyhedra", "Matroids", "EdgeIdeals", "LLLBases", "Elimination"}
    )

export {"slackMatrix",
    "getFacetBases",
    "slackFromPlucker",
    "slackFromGalePlucker",
    "slackFromGaleCircuits",
    "specificSlackMatrix",
    "symbolicSlackMatrix",
    "symbolicSlackOfPlucker",
    "slackIdeal",
    "grassmannSectionIdeal",
    "graphFromSlackMatrix",
    "graphicIdeal",
    "cycleIdeal",
    "universalIdeal",
    "toricPolytope",
    "setOnesForest",
    "rehomogenizePolynomial",
    "rehomogenizeIdeal",
    "findFlag",
    "containsFlag",
    "reconstructSlackMatrix",
    "reducedSlackMatrix",
    -- Options
    "Object",
    "Vars",
    "Tolerance",
    "Saturate",
    "FlagElement",
    "FlagIndices"
    }  
    -- INTERNAL
    -- auxiliary functions
--    nonzeroEntries,
--    polytopeSlack,
--    matroidSlack,
--    sgn,
--    reducedRowEchelon
--    toBin,
--    toricIdeal,
--    slackToPluckerMatrix,
--    fromBinomial,
--    varEntriesIndices,
--    distVU,
--    abstractFaceLattice,
--    findAbstractDim,
--    flagFromPosetCoatoms,
--    simplicialColExtend,
--    smallColExtend,
    -- options
--    Strategy
	

--Abstract Incidence structure helpers
abstractFaceLattice := V -> (
	-- INPUT: list V of facet indices of abstract polytope (coatoms of desired poset)
	-- CAVEAT: V is NOT a list of vertex coordinates!!
	-- OUTPUT: poset representing face lattice of polytope
	v := #V;
	atoms := unique flatten V;
	L := for s in subsets(v) list (
		I := set atoms;
		scan(s, e -> I = I*set(V_e));
		{I, s}
	);
	-- get unique sets that occur in the poset without index sets
	elts := unique apply(L, e -> e_0);
	-- collect each unique element from L with all its index sets
	smallL := for i from 0 to #elts-1 list (
		sameElts := select(L, e -> e_0===elts_i);
		{elts_i, apply(sameElts, e -> e_1)}
	);
	-- create poset by comparing first element of each member of smallL
	poset(smallL, (x, y) -> isSubset(x_0, y_0))
)

findAbstractDim := V -> (
	-- INPUT: list V of facet indices of abstract polytope (coatoms of desired poset)
	-- CAVEAT: V is NOT a list of vertex coordinates!!
	-- OUTPUT: dimension of the polytope
	Pos := abstractFaceLattice V;
	M := maximalChains Pos;
	m := max apply(M, e->#e);
	-- dimension is 2 less than elements in longest max chain
	m-2
)

flagFromPosetCoatoms := (i, coatoms) -> (
	-- Helper function for findFlag
	-- INPUT: List coatoms contains all coatoms of some Poset as sets of atom labels, index i of coatom to start flag
	-- OUTPUT:  a list F of indices of coatoms elements that make up a flag in the Poset
	Pos := abstractFaceLattice coatoms;
	M := maximalChains Pos;
	-- get the longest maximal chains (if we have a full face lattice then all are same length
	-- otherwise, some might be shorter due to missing coatoms)
	C := M_0;
	for m in M do (
		if #m > #C_0 then C = {m} else if #m == #C_0 then C = append(C, m);
	);
	d := #C_0-1;
	flags := {};
	-- get correct index set of zero that actually forms a chain
	for c in C do (
		F := {{}};
		for j from 0 to d do (
			F = select(1, (c_(d-j))_1, e -> (isSubset(F_0, e) and #e==j));
		);
		flags = append(flags, F_0);
	);
	(select(1, flags, f -> member(i, f)))_0
)

nonzeroEntries := M -> (
    -- Helper function: symbolicSlackMatrix,
    -- INPUT: matrix M
    -- OUTPUT: nnz=number of nonzero entries of the matrix M
    nnz := 0;
    for i in (0..numgens target M-1) do (
		for j in (0..numgens source M-1) do (
	    	if M_(i, j) != 0 then nnz = nnz+1;
	    );
	);
    nnz
)

polytopeSlack := P -> (
    -- Helper function for slackMatrix
    -- INPUT: bounded Polyhedron P
    -- OUTPUT: slack matrix of P
    if not isCompact(P) then (
		error "Please use a bounded polyhedron"
	) else (-- facets/vertices doesn't work without Polyhedra$ because of shadowed defns
		V := Polyhedra$vertices P;
		n := numgens source V;
		F := Polyhedra$facets P;
		W := matrix(entries transpose F_1 | -entries transpose F_0);
		one := for i to n-1 list {1};
		-- convexHull(V) and V are often ordered differently 
		<< "\nOrder of vertices is " << endl << entries transpose V << endl;
		(matrix one | transpose V)*W
	)
)
matroidSlack := M -> (
    -- Helper function for slackMatrix
    -- INPUT: realization of matroid M with ground set the columns of a matrix
    -- OUTPUT: slack matrix of M
    V := apply(M_*, v -> flatten entries v);
    H := hyperplanes M;
    d := (rank M)-1;
    I := independentSets(M, d);
    S := {};
    for i from 0 to #V-1 do (
		row := {};
		for h in H do (
	    	-- find a maximal independent set in each hyperplane
	    	dSub := subsets(h, d);
	    	j := 0;
	    	J := dSub_j;
	    	while not member(J, I) do (
				j = j+1;
				J = dSub_j;
			);
	    	J = sort(toList J);
	    	-- calculate slack from independent set
	    	entry := det(matrix({V_i} | V_J));
	    	row = append(row, entry);
	    );
		S = append(S, row);
	);
    matrix S
)

slackMatrix = method(Options => {Object => "polytope"})
slackMatrix List := Matrix => opts -> V -> (
    -- INPUT: a list V of points to be treated as vertices of polytope, vectors of a matroid, or generators of a cone
    -- 'matroid' option treats V as affine points and appends 1 to each to put into projective space
    -- OUTPUT: a slack matrix M of V
    VV := matrix V;
    if opts.Object == "matroid" then (
		V1 := for i to #V-1 list {1} | V_i;
		M := matroid(transpose matrix V1);
		matroidSlack M
	) else if opts.Object == "polytope" then (
		P := convexHull(transpose matrix V);
		polytopeSlack P
	) else if opts.Object == "cone" then (
		C := coneFromVData(transpose matrix V);
		slackMatrix C
	) else (
		error "Please choose a valid object"
	)
)
slackMatrix Matroid := Matrix => opts -> M -> (
    -- check that ground set of M could be a realization
    V := M_*;
    if instance(V_0, List) then (
    	r := rank M;
    	if #(V_0) < r then (
        	error "Please give a realization of the matroid"
        ) else (
        	print(" -- warning: Treating ground set as realization");
        	matroidSlack(matroid(transpose matrix(V)))
        )
    ) else if instance(V_0, Matrix) then (
		matroidSlack(M)
	) else (
		error "Please give a realization of the matroid"
	)
)
slackMatrix Polyhedron := Matrix => opts -> P -> (
    if isCompact(P) then (
		polytopeSlack(P)
	) else (
		error "Please give a bounded polyhedron or cone"
	)
)
slackMatrix Cone := Matrix => opts -> C -> (
    -- INPUT: cone C
    -- OUTPUT: a slack matrix of C
    W := Polyhedra$halfspaces C;
    V := rays C;
    -- coneFromVData(V) and V are often ordered differently 
    << "\nOrder of rays is " << endl << entries transpose V <<endl;
    (transpose V)*(transpose W)
)

getAbstractFacetBases := coatoms -> (
	-- Helper function for getFacetBases
	-- INPUT: a list coatoms of codim 1 elements of an abstract "face" lattice listed as the atom indices they contain
	-- OUTPUT: a list B of a basis (size d set of atoms not contained in any other coatom) for each coatom
	d := findAbstractDim coatoms;
	-- find d subsets of each facet not contained in any other facet (i.e., independent)
	B := for f in coatoms list (
		haveBasis := false;
		S := subsets(#f, d);
		i := 0;
		while not haveBasis and i < #S do (
			b := f_(S_i);
			isBasis := true;
			j := 0;
			while isBasis and j < #f-1 do (
				f2 := (delete(f, coatoms))_j;
				if isSubset(b, f2) then (
					isBasis = false;
				);
				j = j+1;				
			);
			if isBasis then haveBasis = true else i = i+1;
		);
		b
	);
	B
)

-- Helper method for (symbolic)SlackFromPlucker functions
getFacetBases = method(Options => {Object => "polytope"})
getFacetBases Matrix := (List, List) => opts -> S -> (
	-- INPUT: (symbolic) slack matrix S of rank d+1
	-- OUTPUT: first list is empty since there are no vertices to reorder, a list B of d spanning elements for each facet 
	coatoms := apply(entries transpose S, r -> positions(r, e -> e==0_(ring S)));
	({},getAbstractFacetBases coatoms)
)
getFacetBases Polyhedron := (List, List) => opts -> P -> (
	-- INPUT: polytope P
	-- OUTPUT: first list is empty since vertices are not reordered, a list B of d spanning elements for each facet 
	coatoms := apply(Polyhedra$faces(1, P), e -> e_0);
	({}, getAbstractFacetBases coatoms)
)
getFacetBases Matroid := (List, List) => opts -> M -> (
	-- INPUT: matroid M
	-- OUTPUT: first list is empty since ground set elements are not reordered, a list B of d spanning elements for each hyperplane 
	coatoms := apply(hyperplanes M, e -> toList e);
	({}, getAbstractFacetBases coatoms)
)
getFacetBases List := (List, List) => opts -> V -> (
	-- INPUT: list V of vertices of d-polytope, vectors of a rank d+1 matroid, or (d+1)-cone generators
 	-- OUTPUT: a list newV of V in the order corresponding to B, and B the list of d spanning elements for each facet
	newV := {};
	if opts.Object == "abstractPolytope" then (
		(sort unique flatten V, getAbstractFacetBases V)
	) else if opts.Object == "polytope" then (
		P := convexHull(transpose matrix V);
		newV = entries transpose Polyhedra$vertices(P);
		(newV, (getFacetBases P)_1)
	) else if opts.Object == "cone" then (
		C := coneFromVData(transpose matrix V);
		newV = entries transpose rays(C);
		(newV, (getFacetBases C)_1)
	) else if opts.Object == "matroid" then (
		newV = for v in V list {1} | v;
		M := matroid(transpose matrix newV);
		(newV, (getFacetBases M)_1)
	) else error "Please choose a valid object"
)

slackFromPlucker = method(Options => {Object =>  "polytope"}) 
slackFromPlucker (List, List) := Matrix => opts -> (V, B) -> (
    -- INPUT: list of coordinates V for polytope vertices, cone generators, or matroid vectors; a set B of hyperplane spanning set indices
    -- OUTPUT: slack matrix filled with Plucker coordinates
    -- CAVEAT: does not check if B actually spans for given V, does not check orientation of simplices in B
    v := #V;
    X := matrix for p in V list join({1}, p);
    if opts.Object == "cone" then (
		X = matrix V;
	) else if opts.Object == "abstractPolytope" then (
		error "Please give a realization"
	) else if (opts.Object != "polytope" and opts.Object != "matroid") then (
		error "Please choose a valid object object"
	);
	-- check we can calculate plucker coordinates from determinants
    d := (numgens source X)-1;
    if #(B_0) != d then (
		error "Please give a realization in the correct dimension"
	);
    -- get matrix of Plucker index sets
    M := for i from 0 to v-1 list for b in B list join(b, {i});
    matrix apply(M, r -> apply(r, e -> det(submatrix(X, e, ))))
)
slackFromPlucker List := Matrix => opts -> V -> (
    -- INPUT: list of coordinates V for polytope vertices, cone generators, or matroid vectors
    -- OUTPUT: slack matrix filled with Plucker coordinates
    (newV, B) := getFacetBases(V, Object => opts.Object);
	if opts.Object == "polytope" or opts.Object == "cone" then (
		<< "\nInput has been reorderd to" << endl << newV << endl;
	);
    slackFromPlucker(newV, B, Object => opts.Object)
)
slackFromPlucker Polyhedron := Matrix => opts -> P -> (
	-- INPUT: polytope P
	-- OUTPUT: slack matrix filled with Plucker coordinates
	V := Polyhedra$vertices P;
	(newV, B) := getFacetBases P;
	slackFromPlucker(entries transpose V, B, Object => "polytope")
)
slackFromPlucker Matroid := Matrix => opts -> M -> (
	-- INPUT: matroid M
	-- OUTPUT: slack matrix filled with Plucker coordinates
	v := #(M_*);
	X := M_0;
	for i from 1 to v-1 do X = X | M_i;
	d := (numgens target X)-1;
	(newV, B) := getFacetBases M;
	if #(B_0) != d then (
		error "Please give a realization in the correct dimension"
	);
	-- get matrix of Plucker index sets
	N := for i from 0 to v-1 list for b in B list join(b, {i});
	matrix apply(N, r -> apply(r, e -> det(submatrix(X, , e))))
)

sgn = (I, J) -> (
    -- Helper function for slackFromGalePlucker and slackToPluckerMatrix
    -- INPUT: list of subset I of [n], list of subset J of [n]
    -- OUTPUT: the sign of the permutation that puts I, J in the correct order, assuming the elements of I, J are already ordered increasingly
    count := 0;
    for j in J do (
		for i in I do (
			if j < i then count = count + 1;
		);
	);
    (-1)^count
)

slackFromGalePlucker = method()
slackFromGalePlucker (List, List) := Matrix => (B, G) -> (
    -- INPUT: a set B of hyperplane spanning set indices; list of Gale vectors G 
    -- OUTPUT: slack matrix filled with Plucker coordinates
    -- CAVEAT: does not check if B actually spans for given V, does not check orientation of simplices in B\
	-- CAVEAT: make sure elements of G are listed in the order correspondong to indices in B 
    v := #G;
    d := v-1-#(G_0);
    if #(B_0) != d then error "Dimensions do not match.";
    -- get matrix of Plucker index sets and sign of the permutation that puts them in the order  
    M := for i from 0 to v-1 list for b in B list {sgn(toList(set(0..v-1)-join(b, {i})), join(b, {i}))*sgn(b, {i}), sort(toList(set(0..v-1) - join(b, {i})))};
    -- account for the fact that [v]-J is incorrect size if J contains repeats
    matrix apply(M, r->apply(r, e -> if #e_1 != v-d-1 then 0 else (e_0)*det(submatrix(matrix G, e_1, ))))
)    
slackFromGalePlucker (List, Matrix) := Matrix => (B, MG) -> (
    -- INPUT: a set B of hyperplane spanning set indices; matrix of Gale vectors MG
    -- OUTPUT: slack matrix filled with Plucker coordinates
    -- CAVEAT: does not check if B actually spans for given V, does not check orientation of simplices in B
    if (numgens source MG)>(numgens target MG) then (
		slackFromGalePlucker(B, entries transpose MG)
	) else slackFromGalePlucker(B, entries MG)
)

reducedRowEchelon = method(Options => {Tolerance => 14})
reducedRowEchelon Matrix := Matrix => opts -> M -> (
    -- Helper function for slackFromGaleCircuits
    -- INPUT: matrix M of the form matrix(RR, {...}), positive integer to set the tolerance to 10^(-tol) 
    -- OUTPUT: reduced row-echelon form (or Gauss-Jordan form) of the matrix M
    rows := numgens target M; -- number of rows
    cols := numgens source M; -- number of columns
    N := mutableMatrix M;
    eps := 10^(-opts.Tolerance);
    lead := 0;
    for r in (0..rows-1) do (
        if lead >= cols then (
            return matrix N;
        );
        i := r;
        while abs(N_(i, lead)) < eps do (
            i = i + 1;
            if i == rows then (
                i = r;
                lead = lead + 1;
                if cols == lead then (
                    return matrix N;
                );
            );
        );
        rowSwap(N, i, r);
        if abs(N_(r, lead)) >= eps then (
            rowMult(N, r, 1/N_(r, lead));
            for h in (0..cols-1) do (
                if abs(N_(r, h)) < eps then N_(r, h) = 0;
            );
        );
        for i in (0..rows-1) do (
            if i != r then (
                rowAdd(N, i, -N_(i, lead), r);
                for h in (0..cols-1) do (
                    if abs(N_(i, h))<eps then N_(i, h) = 0;
                );
            );
        );
        lead = lead + 1;
    );
	matrix(N)
)

slackFromGaleCircuits = method(Options => {Tolerance => 14})
slackFromGaleCircuits Matrix := Matrix => opts -> G -> (
    -- INPUT: matrix of the form matrix(RR, {...}) whose columns are the vectors of a Gale transform,
    -- optional positive integer Tolerance to set entries less than 10^(-Tolerance) to zero
    -- OUTPUT: slack matrix of a polytope P having G as Gale transform
    print("-- warning: experimental computation over inexact field begun, results not reliable");
	-- Put Gale transform into ring with real numbers so computation runs properly
	G = substitute(G,RR);
    v := numgens source G;
    w := numgens target G;
    d := v - w - 1;
    circuits := {};
    slack := {};
    for i in (2..w+1) do (
		L := subsets(v, i);
		for z in L do (
			M := submatrix(G, , z);
			rkMi := false;
			if w >= i then (
				r := subsets(w, w);
				c := subsets(z, w);
				-- check that rank(M)<w by computing all w-minors of M
				mins := {};
				for a in r do (
					for b in c do mins = mins | {determinant(submatrix(G, a, b))};
				);
				rkMi = all(mins, m -> abs(m) < 10^(-opts.Tolerance));
			);
			-- if rank(M) < min(w, i), compute the minimal positive circuits
			if rkMi or w < i then (
				P := reducedRowEchelon(M, Tolerance => opts.Tolerance);
				circ := flatten entries P_(i-1);
				if all(circ_{0..i-2}, c -> c < 0) and not(any(circuits, c->isSubset(c, z))) then (
					slack = slack | {-circ_{0..i-2} | {1}};
					circuits = circuits | {z};
				);
			);
		);
	);
    -- constructing the slack matrix
    numCirc := #circuits;
    SP := mutableMatrix(RR, v, numCirc);
    for b in (0..numCirc-1) do (
		for a in (0..#(circuits_b)-1) do SP_((circuits_b)_a, b) = (slack_b)_a;
	);
    matrix SP
)

specificSlackMatrix = method()
specificSlackMatrix String := Matrix => name -> (
	-- INPUT: string identifying a polytope or matroid in the following list: "perles1", "perles2", "barnette", "toric-non-graphic", "pu-non-mcmullen", "fano-matroid", "complex-matroid", "nonfano-matroid", "perles-matroid", "perles-matroid-QQ"
	-- CAVEAT: "perles1" and "perles2" are the two nonrational polytopes due to Perles (they refer respectively to the polytopes coming from the Gale diagrams in Figure 5.5.1 and Figure 5.5.2 in Grünbaum, Convex Polytopes, Springer GTM, New York, 2003,Second Edition)
	-- CAVEAT: "barnette" is a four-dimensional prism over a square pyramid with a nonprescribable cubical facet (Section 4.2 in Gouveia, Macchia, Thomas, Wiebe, The slack realization space of a polytope, SIAM J. Discrete Math. 33 (3):1637–1653, 2019) 
	-- CAVEAT: "toric-non-graphic" is an example of polytope with toric but not graphic slack ideal (Example 3.7 in Gouveia, Macchia, Thomas, Wiebe, Projectively unique polytopes and toric slack ideals,  J. Pure Appl. Algebra 224 (5):106229, 2020)
	-- CAVEAT: "pu-non-mcmullen" is a projectively unique polytope that does not come from McMullens constructions (Theorem 4.4.1 in  Wiebe, Realization spaces of polytopes and matroids, PhD thesis, University of Washington, Seattle, 2019)
	-- CAVEAT: "fano-matroid", "complex-matroid", "nonfano-matroid", "perles-matroid", "perles-matroid-QQ" are the matroids respectively from Example 4.3, Example 4.4, Example 5.13 and Example 5.14 in Brandt, Wiebe, The slack realization space of a matroid, Algebraic Combinatorics 2 (4):663–681, 2019)
	R := QQ(monoid[vars 0]);
	a := R_0;
	if name == "perles1" then (
		print("dimension", 8);
		matrix(RR, {{0, 0, 0, 2/(3+sqrt(5)), 2*sqrt(5)/(5+sqrt(5)), 2*sqrt(5)/(5*sqrt(5)+5), 0, 0, 0, 0, 0, 0, 0, -(sqrt(5)-5)/(5+sqrt(5)), -(3-sqrt(5))/(sqrt(5)-5), -(5-3*sqrt(5))/(5+sqrt(5)), -(sqrt(5)-1)/(sqrt(5)-5), -(5-3*sqrt(5))/(5+sqrt(5)), -(-sqrt(5)+1)/(sqrt(5)+1), -(-3-sqrt(5))/(10+4*sqrt(5)), -(-sqrt(5)-1)/(3+sqrt(5)), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 2/(sqrt(5)+1), 0, 0, 2*sqrt(5)/(5+sqrt(5)), (sqrt(5)+1)/(2*sqrt(5)+4), 2*sqrt(5)/(5*sqrt(5)+5), 0, 0, 0, 0, -2+sqrt(5), -(sqrt(5)-5)/(-5+5*sqrt(5)), 3/2-(1/2)*sqrt(5), -(5-3*sqrt(5))/(-5+5*sqrt(5)), 0, 0, 0, 0, -(-3-sqrt(5))/(10+4*sqrt(5)), -(5-3*sqrt(5))/(5+sqrt(5)), -(-sqrt(5)+1)/(sqrt(5)+1), 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 3/2-(1/2)*sqrt(5), 0, 0, 3/2-(1/2)*sqrt(5), 1/2-(1/10)*sqrt(5), 0, 0, -(-sqrt(5)+1)/(sqrt(5)+1), 1/2-(1/10)*sqrt(5), 0, 0, 3/2-(1/2)*sqrt(5), -2+sqrt(5), 0, 0, 2/(5+sqrt(5)), 0, 0, 0, -(25-11*sqrt(5))/(-15+5*sqrt(5)), 1, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 3/2-(1/2)*sqrt(5), 0, 0, 0, 0, 0, 0, 1/2-(1/10)*sqrt(5), 3/2-(1/2)*sqrt(5), 0, 0, -(-sqrt(5)+1)/(sqrt(5)+1), 1/2-(1/10)*sqrt(5), 0, 0, 2/(5+sqrt(5)), 0, 0, 3/2-(1/2)*sqrt(5), -2+sqrt(5), 0, 0, 0, 3/2-(1/2)*sqrt(5), (1/2)*sqrt(5)-1/2, -(25-11*sqrt(5))/(-15+5*sqrt(5)), 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 2/(sqrt(5)+1), 0, -(3*sqrt(5)-5)/(sqrt(5)-5), 0, -2/(sqrt(5)-5), 0, 0, 0, 0, 0, 0, 0, 0, 0, -(-3-sqrt(5))/(5+3*sqrt(5)), 2*sqrt(5)/(5+3*sqrt(5)), 0, 0, -(3*sqrt(5)-5)/(-15+5*sqrt(5)), 0, -2*sqrt(5)/(sqrt(5)-5), 1, 0, 2/(5+sqrt(5)), (1/2)*sqrt(5)+1/2, 1/2+(1/10)*sqrt(5), 0}, {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2/(sqrt(5)-5), 0, -(3*sqrt(5)-5)/(sqrt(5)-5), 0, 0, 0, 0, 2*sqrt(5)/(5+3*sqrt(5)), 0, -(-3-sqrt(5))/(5+3*sqrt(5)), 0, 0, 0, 0, 0, 0, -(-3+sqrt(5))/(-4+2*sqrt(5)), 0, 0, -(3*sqrt(5)-5)/(-15+5*sqrt(5)), 0, 0, 0, (1/10)*(sqrt(5)-1)*sqrt(5)}, {0, 1, 0, 0, 0, 0, 0, 0, 1/2+(1/10)*sqrt(5), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2*sqrt(5)/(5+3*sqrt(5)), 1, 0, 0, 0, 0, -(3*sqrt(5)-5)/(-15+5*sqrt(5)), 2/(5+sqrt(5)), 0, 0, -(-sqrt(5)-1)/(5+sqrt(5))}, {0, 0, 1, 0, 0, 1/2+(1/10)*sqrt(5), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2*sqrt(5)/(5+3*sqrt(5)), 0, 1, 0, 0, 0, 0, -(3*sqrt(5)-5)/(-15+5*sqrt(5)), 0, 0, 0, 0, -(-sqrt(5)-1)/(5+sqrt(5)), 1, 1/2+(1/10)*sqrt(5), 2/(5+sqrt(5))}, {1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (1/2)*sqrt(5)-1/2, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, (1/2)*sqrt(5)+1/2, (1/5)*sqrt(5), 0}, {0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, -(sqrt(5)-5)/(3*sqrt(5)-5), 1, -(3*sqrt(5)-5)/(sqrt(5)-5), 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1}})
	) else if name == "perles2" then (
		print("dimension", 8);
		matrix(RR, {{(2836152*sqrt(5) + 5360256)/1459855, (493812*sqrt(5) + 1217700)/187409, (41517828*sqrt(5) + 224340732)/31766699, 0, 0, 0, (35478108*sqrt(5) + 85655952)/16407589, 0, 0, 0, 0, 0, 0, 0, (47784*sqrt(5) + 298320)/50299, 0, 0, 0, 0, 0, 0, 0, 0, 0, (342936*sqrt(5) + 2756160)/579041, 0, 0, 0, 0, 0}, {0, (114312*sqrt(5) + 233904)/187409, (-14207292*sqrt(5) + 78892044)/31766699, 0, 0, (414*sqrt(5) + 906)/311, (-12243186*sqrt(5) + 45046182)/16407589, 0, (-20875068*sqrt(5) + 51719196)/26102989, 0, (2596572*sqrt(5) + 1703064)/1777939, 0, 0, 0, 0, 0, 0, 0, 0, (-7333884*sqrt(5) + 48832152)/20501549, 0, 0, (1584*sqrt(5) + 36696)/22621, 0, (-185328*sqrt(5) + 981816)/579041, (74052*sqrt(5) + 116556)/122321, (-9067770*sqrt(5) + 58107066)/27082049, 0, 0, (411048*sqrt(5) + 8496048)/6199429}, {(-71676*sqrt(5) + 387000)/291971, (-94248*sqrt(5) + 270756)/187409, (-2267892*sqrt(5) + 34746984)/31766699, 0, (1043856*sqrt(5) + 9865908)/12542879, 0, 0, 0, 0, (-198*sqrt(5) + 1494)/779, (1648548*sqrt(5) + 2211966)/1777939, (-1174206*sqrt(5) + 3080622)/1549411, (-70818*sqrt(5) + 577446)/293921, (754776*sqrt(5) + 4718316)/3599671, (-24948*sqrt(5) + 117696)/50299, (-886380*sqrt(5) + 22796112)/19623371, 0, (151056*sqrt(5) + 253332)/193699, (-1983*sqrt(5) + 9921)/4282, 0, (17496*sqrt(5) + 9100)/6589, (-244848*sqrt(5) + 787572)/401599, (-4140*sqrt(5) + 39816)/22621, (-19698*sqrt(5) + 72126)/29431, (-234972*sqrt(5) + 1096344)/579041, (-46278*sqrt(5) + 280386)/122321, 0, (244056*sqrt(5) + 374148)/242429, (-4140*sqrt(5) + 11112)/6061, (-934344*sqrt(5) + 9212364)/6199429}, {0, 0, (25042644*sqrt(5) + 43220628)/31766699, (21*sqrt(5) + 93)/25, 0, 0, (-13526046*sqrt(5) + 43141662)/16407589, 0, (-9275724*sqrt(5) + 86404788)/26102989, 0, (1857834*sqrt(5) + 4060782)/1777939, (2372172*sqrt(5) + 2346036)/1549411, 0, 0, 0, 0, (1770*sqrt(5) + 30294)/10571, 0, 0, (-2679462*sqrt(5) + 48241458)/20501549, 0, 0, 0, 0, 0, 0, (-3876372*sqrt(5) + 57319560)/27082049, 0, 0, 0}, {0, 0, 0, 0, (12359556*sqrt(5) + 40587228)/12542879, (414*sqrt(5) + 2826)/311, 0, 0, 0, 0, 0, 0, (857868*sqrt(5) + 1091244)/293921, (7538388*sqrt(5) + 19558572)/3599671, 0, (7552908*sqrt(5) + 91871076)/19623371, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, (-9708876*sqrt(5) + 23047272)/16407589, (522*sqrt(5) + 234)/109, (2621556*sqrt(5) + 49068828)/26102989, 0, 0, 0, 0, 0, 0, (2065404*sqrt(5) + 28835004)/19623371, (-5442*sqrt(5) + 41490)/10571, 0, 0, (-16181928*sqrt(5) + 39426588)/20501549, (6956*sqrt(5) - 1076)/599, 0, 0, 0, 0, 0, (-14181876*sqrt(5) + 80028576)/27082049, (37284*sqrt(5) + 39564)/22039, 0, (-279972*sqrt(5) + 11557260)/6199429}, {0, 0, 0, 0, 0, 0, 0, 0, (27529236*sqrt(5) + 126043056)/26102989, (594*sqrt(5) + 23562)/3895, (2387286*sqrt(5) + 13359456)/1777939, (5652702*sqrt(5) + 8874426)/1549411, (1048674*sqrt(5) + 868890)/293921, 0, 0, 0, 0, 0, (4389*sqrt(5) + 13101)/4282, 0, 0, 0, 0, (330*sqrt(5) + 144342)/29431, 0, (365310*sqrt(5) + 461142)/122321, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (4454736*sqrt(5) + 4291848)/1549411, (261624*sqrt(5) + 989472)/293921, (10835352*sqrt(5) + 8912112)/3599671, (-22836*sqrt(5) + 187572)/50299, (-8731932*sqrt(5) + 91978260)/19623371, (3672*sqrt(5) + 55068)/10571, 0, (3201*sqrt(5) + 4257)/2141, 0, (10412*sqrt(5) - 580)/599, (45492*sqrt(5) + 84900)/36509, 0, (60324*sqrt(5) + 31548)/29431, 0, 0, 0, (115092*sqrt(5) + 34116)/22039, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (1101456*sqrt(5) + 1110888)/193699, (1998*sqrt(5) + 9924)/2141, (26195274*sqrt(5) + 109518390)/20501549, (208544*sqrt(5) + 88184)/6589, (277584*sqrt(5) + 2554536)/401599, (2556*sqrt(5) + 194940)/22621, 0, 0, 0, 0, 0, 0, 0}, {(-1786752*sqrt(5) + 6201624)/1459855, (285252*sqrt(5) + 526548)/187409, 0, 0, (-4938516*sqrt(5) + 49879500)/12542879, 0, 0, (522*sqrt(5) + 1074)/109, 0, (396*sqrt(5) + 15708)/3895, 0, 0, 0, 0, 0, 0, 0, (53604*sqrt(5) + 40596)/17609, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, (79692*sqrt(5) + 105156)/29431, (77364*sqrt(5) + 2114172)/579041, (337536*sqrt(5) + 609768)/122321, (27126018*sqrt(5) + 129529386)/27082049, (1920192*sqrt(5) + 1724520)/242429, (13512*sqrt(5) + 38784)/6061, (803268*sqrt(5) + 45127476)/6199429}, {(-138204*sqrt(5) + 804276)/291971, 0, 0, (-21*sqrt(5) + 207)/25, (-8464896*sqrt(5) + 50181912)/12542879, 0, 0, 0, 0, 0, 0, 0, 0, (2542188*sqrt(5) + 10007052)/3599671, 0, 0, 0, (32796*sqrt(5) + 46692)/17609, 0, 0, 0, (48468*sqrt(5) + 49380)/36509, 0, 0, 0, 0, 0, 0, (-852*sqrt(5) + 2076)/551, 0}})
	) else if name == "barnette" then (
		print("dimension", 4); 
		matrix(RR, {{1, 0, 0, 0, 1, 1, 0}, {1, 0, 0, 0, 0, 1, 1}, {1, 0, 0, 1, 0, 0, 1}, {1, 0, 0, 1, 1, 0, 0}, {1, 0, 1, 0, 0, 0, 0}, {0, 1, 0, 0, 1, 1, 0}, {0, 1, 0, 0, 0, 1, 1}, {0, 1, 0, 1, 0, 0, 1}, {0, 1, 0, 1, 1, 0, 0}, {0, 1, 1, 0, 0, 0, 0}})
	) else if name == "toric-non-graphic" then ( 
		print("dimension", 5);
		matrix(RR, {{0, 1, 1, 0, 0, 0, 0, 1}, {1, 1, 1, 0, 0, 0, 0, 0}, {0, 0, 1, 1, 0, 0, 0, 0}, {0, 1, 0, 0, 0, 1, 0, 0}, {1, 0, 1, 0, 0, 0, 1, 0}, {0, 0, 1, 0, 0, 0, 1, 1}, {0, 0, 0, 0, 0, 1, 1, 0}, {1, 1, 0, 0, 1, 0, 0, 0}, {0, 1, 0, 0, 1, 0, 0, 1}, {0, 0, 0, 1, 1, 0, 0, 0}, {0, 0, 0, 0, 1, 0, 1, 1}, {1, 0, 0, 0, 1, 0, 1, 0}})
	) else if name == "pu-non-mcmullen" then (
		print("dimension", 5);
		matrix(RR, {{0, 0, 1, 0, 0, 0, 1, 1, 0, 0}, {1, 1, 1, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 1, 1, 0, 1, 0, 0, 0}, {0, 1, 0, 1, 0, 0, 0, 0, 0, 1}, {0, 1, 1, 1, 0, 0, 0, 0, 1, 0}, {1, 0, 0, 0, 1, 1, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 1, 1, 0}, {0, 0, 0, 0, 0, 1, 0, 1, 0, 1}, {0, 0, 0, 1, 1, 1, 0, 0, 1, 0}})
	) else if name == "fano-matroid" then (
		print("dimension", 2);
		matrix(ZZ/2, {{1, 0, 1, 0, 0, 1, 1}, {0, 0, 1, 1, 1, 1, 0}, {0, 1, 1, 0, 1, 0, 1}, {1, 1, 1, 1, 0, 0, 0}, {1, 0, 0, 1, 1, 0, 1}, {1, 1, 0, 0, 1, 1, 0}, {0, 1, 0, 1, 0, 1, 1}})
	) else if name == "complex-matroid" then (
		print("dimension", 2);
		matrix(R/ideal(a^2-a+1), {{0, a, 1, 1, 0, 1, 0, a-1, -a+1, a, 1, 0}, {-a, 0, 1, a, 0, 0, 1, -1, 1, -1, 0, -a+1}, {1, 0, a, 0, -a+1, a, 0, -a, 1, 0, 1, 1}, {1, a, a, 0, 0, 1, a-1, 0, 0, 1, 1, a}, {a, 0, 0, -a+1, -a+1, a, -a, 0, -a+1, a, 1, 0}, {0, 1, 0, 0, a, -a, a, 1, a-1, -a+1, 0, a}, {0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1}, {a, 1, 0, -a+1, 1, 0, 0, 1, 0, 1, 1, a}})
	) else if name == "nonfano-matroid" then (
		print("dimension", 2);
		matrix(RR, {{1, 0, 1, 0, 0, 1, 1, 1, 1}, {0, 0, 1, 1, -1, 1, 2, 0, 0}, {0, 1, -1, 0, 1, 0, -1, -1, 1}, {-1, 1, -1, 1, 0, 0, 0, -2, 0}, {1, 0, 0, -1, 1, 0, -1, 1, 1}, {1, 1, 0, 0, 1, 1, 0, 0, 2}, {0, 1, 0, 1, 0, 1, 1, -1, 1}})
	) else if name == "perles-matroid" then (
		print("dimension", 2);
		matrix(RR, {{0, 1, 3/2-sqrt(5)/2-3, 3-3/2-sqrt(5)/2, 0, 3-3/2-sqrt(5)/2, 0, -1, 1, 3-3/2-sqrt(5)/2, 1, 3/2-sqrt(5)/2-2, 2-3/2-sqrt(5)/2, 2-3/2-sqrt(5)/2, 0}, {2*(3/2-sqrt(5)/2)-5, 1, 0, 0, 3-3/2-sqrt(5)/2, 3-3/2-sqrt(5)/2, 0, 2-3/2-sqrt(5)/2, 3-3/2-sqrt(5)/2, 3/2-sqrt(5)/2-2, 0, 3/2-sqrt(5)/2-2, 0, 2-3/2-sqrt(5)/2, 5-2*(3/2-sqrt(5)/2)}, {1, 3/2-sqrt(5)/2, 1, 0, 3/2-sqrt(5)/2, 3/2-sqrt(5)/2-1, 1, 0, 0, 3/2-sqrt(5)/2, 1-3/2-sqrt(5)/2, 1, 3/2-sqrt(5)/2, 0, 0}, {1, 0, 3/2-sqrt(5)/2-1, 1-3/2-sqrt(5)/2, 3/2-sqrt(5)/2-1, 0, 0, 3/2-sqrt(5)/2-1, -3/2-sqrt(5)/2, 1, 1-2*(3/2-sqrt(5)/2), 0, 3/2-sqrt(5)/2, 0, -1}, {2-3/2-sqrt(5)/2, 0, 2-3/2-sqrt(5)/2, 3/2-sqrt(5)/2-1, 0, 3/2-sqrt(5)/2-2, 1, 0, 3/2-sqrt(5)/2-1, 0, 3/2-sqrt(5)/2, 2-3/2-sqrt(5)/2, 0, 3/2-sqrt(5)/2-1, 3/2-sqrt(5)/2-1}, {2-3/2-sqrt(5)/2, 1-3/2-sqrt(5)/2, 0, 1, 0, 0, 1, 3/2-sqrt(5)/2-1, 0, 2-3/2-sqrt(5)/2, 1, 1-3/2-sqrt(5)/2, 1, 3/2-sqrt(5)/2, 3/2-sqrt(5)/2-1}, {0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1}, {0, 0, 3-3/2-sqrt(5)/2, 3/2-sqrt(5)/2-2, 1, 3/2-sqrt(5)/2-2, 1, 1, 0, 3/2-sqrt(5)/2-2, 0, 2-3/2-sqrt(5)/2, 3/2-sqrt(5)/2-1, 3/2-sqrt(5)/2-1, 1}, {0, 3/2-sqrt(5)/2+1, 1, 0, 1, 0, 1, 3/2-sqrt(5)/2, 1-3/2-sqrt(5)/2, 0, 1-3/2-sqrt(5)/2, 1-3/2-sqrt(5)/2, 3/2-sqrt(5)/2, 3/2-sqrt(5)/2, 1}})
	) else if name == "perles-matroid-QQ" then (
		print("dimension", 2);
		matrix(R/ideal(a^2-3*a+1), {{0, 1, a-3, 3-a, 0, 3-a, 0, -1, 1, 3-a, 1, a-2, 2-a, 2-a, 0}, {2*a-5, 1, 0, 0, 3-a, 3-a, 0, 2-a, 3-a, a-2, 0, a-2, 0, 2-a, 5-2*a}, {1, a, 1, 0, a, a-1, 1, 0, 0, a, 1-a, 1, a, 0, 0}, {1, 0, a-1, 1-a, a-1, 0, 0, a-1, -a, 1, 1-2*a, 0, a, 0, -1}, {2-a, 0, 2-a, a-1, 0, a-2, 1, 0, a-1, 0, a, 2-a, 0, a-1, a-1}, {2-a, 1-a, 0, 1, 0, 0, 1, a-1, 0, 2-a, 1, 1-a, 1, a, a-1}, {0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1}, {0, 0, 3-a, a-2, 1, a-2, 1, 1, 0, a-2, 0, 2-a, a-1, a-1, 1}, {0, a+1, 1, 0, 1, 0, 1, a, 1-a, 0, 1-a, 1-a, a, a, 1}})
	) else error "Please choose one of the specified objects"
)

symbolicSlackMatrix = method(Options => {Object => "polytope", CoefficientRing => QQ, Vars => {}})
symbolicSlackMatrix Matrix := Matrix => opts -> S -> (
    -- INPUT: S = slack matrix
    -- OUTPUT: the symbolic slack matrix with support given by S
    -- also helper function for other symbolicSlackMatrix instances
    nvar := nonzeroEntries(S);
    a := numgens target S; 
    b := numgens source S;
    F := opts.CoefficientRing;
    R := F(monoid[VariableBaseName => "x", Variables => nvar]); 
    -- check if a set of appropriate variables is given as an option
    var := opts.Vars;
    if (#var > 0 and #var < nvar) then (
		print("-- warning: not enough variables given. Creating new ring.");
	) else if #var > 0 then (
		R = F[var];
	);
    N := mutableMatrix(R, a, b);
    -- replace nonzero entries in order by rows with variables 
    ind := 0;
    for i in (0..a-1) do (
		for j in (0..b-1) do (
	    	if (S_(i, j) != 0) then (
				N_(i, j) = R_ind;
				ind = ind + 1;
			);
	    );
	);
    matrix(N)
)
symbolicSlackMatrix Matroid := Matrix => opts -> M -> (
    -- INPUT: matroid M
    -- OUTPUT: the (vertex-hyperplane) symbolic slack matrix of M
    n := #M.groundSet;
    H := hyperplanes M;
    h := #H;
    supp := for i from 0 to n-1
        list for j from 0 to h-1 
        list if member(i, H_j) then 0 else 1;
    symbolicSlackMatrix(matrix supp, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
)
symbolicSlackMatrix List := Matrix => opts -> V -> (
    -- INPUT: V = a list of vertex coordinates, matroid vectors, facet labels, or cone generators
    -- OUTPUT: the symbolic slack matrix of conv(V), or of matroid M[V] if Object => matroid is given
    --         or of the abstract polytope with facets V if Object => abstractPolytope is given
    --         or of cone(V) if Object => cone is given
    if (opts.Object == "polytope" or opts.Object == "cone") then (
		S := slackMatrix(V, Object => opts.Object);
		symbolicSlackMatrix(S, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
	) else if opts.Object == "matroid" then (
		V1 := for i to #V-1 list {1} | V_i;
		M := matroid(transpose matrix V1);
		symbolicSlackMatrix(M, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
	) else if opts.Object == "abstractPolytope" then (
		-- extract ground set elements from V
		VV := sort(unique flatten V);
		supp := for i in VV list for F in V list if member(i, F) then 0 else 1;
		symbolicSlackMatrix(matrix supp, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
	) else error "Please choose a valid object"
)
symbolicSlackMatrix Polyhedron := Matrix => opts -> P -> (
    -- INPUT: polytope P
    -- OUTPUT: the (vertex-facet) symbolic slack matrix of P
    S := slackMatrix P;
    symbolicSlackMatrix(S, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
)
symbolicSlackMatrix Cone := Matrix => opts -> C -> (
    -- INPUT: convex cone C
    -- OUTPUT: the (extreme ray-facet) symbolic slack matrix of C
    S := slackMatrix C;
    symbolicSlackMatrix(S, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
)

symbolicSlackOfPlucker = method(Options => {Object => "polytope", CoefficientRing => QQ})
symbolicSlackOfPlucker (ZZ, List) := Matrix => opts -> (v, B) -> (
    -- INPUT: number v of polytope vertices, cone generators, or matroid vectors; a set B of hyperplane spanning set indices
    -- OUTPUT: slack matrix filled with Plucker variables
    -- CAVEAT: does not adjust for simplex orientation in B
    d := #(B_0);
    F := opts.CoefficientRing;
    R := F[gens ring Grassmannian(d, v-1)];
    inds := subsets(toList(0..v-1), d+1);
    M := for i from 0 to v-1 list for b in B list {sgn(b, {i}), sort(join(b, {i}))};
    matrix apply(M, r -> apply(r, e -> if member(e_1, inds) then (e_0)*R_(position(inds, j -> j==e_1)) else 0))
)
symbolicSlackOfPlucker (List, List) := Matrix => opts -> (V, B) -> (
    -- INPUT: list of coordinates V for polytope vertices, cone generators, or matroid vectors; a set B of hyperplane spanning set indices
    -- OUTPUT: slack matrix filled with Plucker variables
    -- CAVEAT: does not check if B makes sense for given V
    symbolicSlackOfPlucker(#V, B, CoefficientRing => opts.CoefficientRing)
)
symbolicSlackOfPlucker List := Matrix => opts -> V -> (
    -- INPUT: list of coordinates V for polytope vertices, cone generators, or matroid vectors
    -- OUTPUT: slack matrix filled with Plucker variables
    (newV, B) := getFacetBases(V, Object => opts.Object);
	if opts.Object == "polytope" or opts.Object == "cone" then (
		<< "\nInput has been reorderd to" << endl << newV << endl;
	);
    symbolicSlackOfPlucker(#V, B, CoefficientRing => opts.CoefficientRing)
)
symbolicSlackOfPlucker (Matrix, List) := Matrix => opts -> (S, B) -> (
    -- INPUT: (symbolic) slack matrix S; a set B of hyperplane spanning set indices
    -- OUTPUT: slack matrix filled with Plucker variables
    -- CAVEAT: does not check if B makes sense for given S
    symbolicSlackOfPlucker(numgens target S, B, CoefficientRing => opts.CoefficientRing)
)
symbolicSlackOfPlucker Matrix := Matrix => opts -> S -> (
    -- INPUT: slack matrix S
    -- OUTPUT: slack matrix filled with Plucker variables
	if instance(ring S, PolynomialRing) and (flatten entries S != flatten entries substitute(S, coefficientRing ring S)) then (
		error "Please give an actual slack matrix or a list of facet bases"
	);
    (newV, B) := getFacetBases S;
    symbolicSlackOfPlucker(numgens target S, B, CoefficientRing => opts.CoefficientRing)
)
symbolicSlackOfPlucker Polyhedron := Matrix => opts -> P -> (
    -- INPUT: polytope P
    -- OUTPUT: slack matrix filled with Plucker variables
	(newV,B) := getFacetBases P;
	symbolicSlackOfPlucker(nVertices P, B, CoefficientRing => opts.CoefficientRing)
)
symbolicSlackOfPlucker Matroid := Matrix => opts -> M -> (
    -- INPUT: polytope P
    -- OUTPUT: slack matrix filled with Plucker variables
	(newV,B) := getFacetBases M;
	symbolicSlackOfPlucker(#(M_*), B, CoefficientRing => opts.CoefficientRing)
)

slackIdeal = method(Options => {Object => "polytope", Strategy => Eliminate, CoefficientRing => QQ, Vars => {}, Saturate => "all"})
slackIdeal (ZZ, List) := Ideal => opts -> (d, V) -> (
    -- INPUT: list of points in d-space, optional: object type, saturation strategy, ring and variables
    -- OUTPUT: slack ideal of conv(V), M[1 V] if Object => matroid, or abstract polytope with facets indexed by V
    -- if vectors are given check they span correct dimensional space
    if opts.Object == "polytope" or opts.Object == "matroid" or opts.Object == "cone" then (
		M := slackMatrix(V, Object => opts.Object);
		D := rank M;
		if D != d+1 then (
	    	print(" -- warning: Object is not of the correct dimension");
	    );
	);
    S := symbolicSlackMatrix(V, Object => opts.Object, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
    J := minors(d+2, S);
    if opts.Saturate == "all" then (
    	saturate(J, product gens ring S, Strategy => opts.Strategy)
    ) else if opts.Saturate == "each" then (
    	I := J;
    	for i in gens ring S do I = saturate(I, i, Strategy => opts.Strategy);
    I
    ) else error "Please choose a valid saturation mode"
)
slackIdeal Matroid := Ideal => opts -> M -> (
    -- INPUT: matroid M
    -- OUTPUT: slack ideal of M
    d := (rank M)-1;
    S := symbolicSlackMatrix(M, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
    J := minors(d+2, S);
    if opts.Saturate == "all" then (
    	saturate(J, product gens ring S, Strategy => opts.Strategy)
    ) else if opts.Saturate == "each" then (
    	I := J;
    	for i in gens ring S do I = saturate(I, i, Strategy => opts.Strategy);
    I
    ) else error "Please choose a valid saturation mode"
)
slackIdeal Polyhedron := Ideal => opts -> P -> (
    -- INPUT: polytope P
    -- OUTPUT: slack ideal of P
    d := dim P;
    S := symbolicSlackMatrix(P, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
    J := minors(d+2, S);
    if opts.Saturate == "all" then (
    	saturate(J, product gens ring S, Strategy => opts.Strategy)
    ) else if opts.Saturate == "each" then (
    	I := J;
    	for i in gens ring S do I = saturate(I, i, Strategy => opts.Strategy);
    I
    ) else error "Please choose a valid saturation mode"
)
slackIdeal Cone := Ideal => opts -> C -> (
    -- INPUT: cone C
    -- OUTPUT: slack ideal of C
    d := dim(C)-1;
    S := symbolicSlackMatrix(C, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
    J := minors(d+2, S);
    if opts.Saturate == "all" then (
    	saturate(J, product gens ring S, Strategy => opts.Strategy)
    ) else if opts.Saturate == "each" then (
    	I := J;
    	for i in (gens ring S) do I = saturate(I, i, Strategy => opts.Strategy);
    I
    ) else error "Please choose a valid saturation mode"
)
slackIdeal List := Ideal => opts -> V -> (
    -- INPUT: list V of points, optional object type
    -- OUTPUT: slack ideal of conv(V), or M[1 V] if Object => matroid, or abstract polytope with facets V, or cone(V)
    if opts.Object == "polytope" then (
		P := convexHull(transpose matrix V);
		slackIdeal(P, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
	) else if opts.Object == "matroid" then (
		V1 := for i to #V-1 list {1} | V_i;
		M := matroid(transpose matrix V1);
		slackIdeal(M, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
	) else if opts.Object == "abstractPolytope" then (
        slackIdeal(V, findAbstractDim(V), Object => "abstractPolytope", CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
	) else if opts.Object == "cone" then (
		C := coneFromVData(transpose matrix V);
		slackIdeal C
	) else error "Please choose a valid object"
)
slackIdeal (ZZ, Matrix) := Ideal => opts -> (d, S) -> (
    -- INPUT: (symbolic) slack matrix S of d-polytope or rank d+1 matroid
    -- OUTPUT: slack ideal of S
    F := opts.CoefficientRing;
    M := S;
    R := F[gens ring S];
    var := opts.Vars;
    -- check if S has variable entries, if not create symbolic version 
    -- or create new symbolic version with correct variables
    if (numgens R == 0 or #var > 0) then (
		M = symbolicSlackMatrix(S, CoefficientRing => F, Vars => var);
		R = ring M;
	);
    J := substitute(minors(d+2, M), R);
    if opts.Saturate == "all" then (
    	saturate(J, product gens R, Strategy => opts.Strategy)
    ) else if opts.Saturate == "each" then (
    	I := J;
    	for i in gens R do I = saturate(I, i, Strategy => opts.Strategy);
    I
    ) else error "Please choose a valid saturation mode"
)
slackIdeal Matrix := Ideal => opts -> S -> (
    -- INPUT: (symbolic) slack matrix S 
    -- OUTPUT: slack ideal of S
    d := findAbstractDim apply(entries transpose S, r -> positions(r, e -> e==0_(ring S)));
    slackIdeal(d, S, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
)

grassmannSectionIdeal = method(Options => {Object => "polytope", CoefficientRing => QQ, Strategy => Eliminate, Saturate => "all"})
grassmannSectionIdeal (Matrix, List) := Ideal => opts -> (S, B) -> (
    -- INPUT: a slack matrix S; a set B of hyperplane spanning set indices
    -- OUTPUT: the Grassmannian section ideal corresponding to choice B of the object with slack matrix S
    -- CAVEAT: does not check if B makes sense for given S
    d := findAbstractDim apply(entries transpose S, r -> positions(r, e -> e==0_(ring S)));
    v := numgens target S;
    if #(B_0) != d then error "Dimensions do not match.";
    G := Grassmannian(d, v-1);
    F := opts.CoefficientRing;
    R := F[gens ring G];
    G = substitute(G, R);
    Spl := substitute(symbolicSlackOfPlucker(S, B, CoefficientRing => F), R);
    zeroPl := select(apply(flatten entries Spl, flatten entries S, (e, f) -> if f==0_(ring S) and e != 0_(ring Spl) then e else 0_(ring Spl)), j -> j != 0_(ring Spl));
    -- entries of Spl may have negative signs which won't be recognized as generators, so include +/- all entries
    usedPl := delete(0_R, unique join(flatten entries Spl, -flatten entries Spl));
    Z := ideal zeroPl;
    -- zeroPl may also contain -p_J elements which we need to change to p_J to eliminate
    G0 := eliminate(G + Z, flatten entries mingens Z);
    G0used := eliminate(G0, toList(gens R - set(usedPl)));
    if opts.Saturate == "all" then (
    	saturate(G0used, product usedPl, Strategy => opts.Strategy)
    ) else if opts.Saturate == "each" then (
    	G0usedSat := G0used;
    	for i in usedPl do G0usedSat = saturate(G0usedSat, i, Strategy => opts.Strategy);
    	G0usedSat
    ) else error "Please choose a valid saturation mode"
)
grassmannSectionIdeal Matrix := Ideal => opts -> S -> (
    -- INPUT: a slack matrix S
    -- OUTPUT: the Grassmannian section ideal corresponding to choice B of the object with slack matrix S
    (newV, B) := getFacetBases S;
    grassmannSectionIdeal(S, B, CoefficientRing => opts.CoefficientRing, Strategy => opts.Strategy)
)
grassmannSectionIdeal (List, List) := Ideal => opts -> (V, B) -> (
    -- INPUT: a list V of polytope vertex coordinates, cone generators, or matroid vectors; a set B of hyperplane spanning set indices
    -- OUTPUT: the Grassmannian section ideal corresponding to choice B for object V
    -- CAVEAT: does not check if B makes sense for given S
    S := slackMatrix(V, Object => opts.Object);
    grassmannSectionIdeal(S, B, CoefficientRing => opts.CoefficientRing, Strategy => opts.Strategy)
)
grassmannSectionIdeal List := Ideal => opts -> V -> (
    -- INPUT: a list V of polytope vertex coordinates, cone generators, or matroid vectors; a set B of hyperplane spanning set indices
    -- OUTPUT: the Grassmannian section ideal corresponding to choice B for object V
	S := symbolicSlackMatrix(V, Object => opts.Object);
    grassmannSectionIdeal(S, CoefficientRing => opts.CoefficientRing, Strategy => opts.Strategy)
)
grassmannSectionIdeal Polyhedron := Ideal => opts -> P -> (
    -- INPUT: polytope P
    -- OUTPUT: Grassmannian section ideal of P
    S := slackMatrix P;
    grassmannSectionIdeal(S, CoefficientRing => opts.CoefficientRing, Strategy => opts.Strategy)
)
grassmannSectionIdeal Cone := Ideal => opts -> C -> (
    -- INPUT: cone C
    -- OUTPUT: Grassmannian section ideal of C
    S := slackMatrix C;
    grassmannSectionIdeal(S, CoefficientRing => opts.CoefficientRing, Strategy => opts.Strategy)
)
grassmannSectionIdeal Matroid := Ideal => opts -> M -> (
    -- INPUT: matroid M
    -- OUTPUT: Grassmannian section ideal of M 
    --(not technically defined, but should be fine, right??)
    S := slackMatrix M;
    grassmannSectionIdeal(S, CoefficientRing => opts.CoefficientRing, Strategy => opts.Strategy)
)

toBin = method()
toBin (List, List, Ring) := (b, ss, R) -> (
    -- INPUT: integer vector b, list of nonzero slack matrix entries ss, ring R
    -- OUTPUT: binomial in ring ss corresponding to cycle b: x^b+ - \alpha_b x^b-
    r := #(gens R);
    if ss == {0} then ss = for i from 1 to r list 1;
    pos := 1_R;
    neg := 1_R;
    scan(#b, i-> if b_i>0 then (pos = pos*(R_i)^(b_i); neg = ss_i*neg;) else if b_i<0 then (neg = neg*(R_i)^(-b_i); pos = ss_i*pos;));
    pos - neg
)

toricIdeal = method(Options => {Strategy => Eliminate, CoefficientRing => QQ, Vars => {}, Saturate => "all"})
toricIdeal(Matrix, List) := Ideal => opts -> (A, ss) -> (
    -- Helper function for cycleIdeal, graphicIdeal
    -- INPUT: A = matrix whose columns are some vector configuration
    -- OUTPUT: the toric ideal of the vector configuration defined by A
    n := numgens source A;
    F := opts.CoefficientRing;
    R := F(monoid[VariableBaseName => "y", Variables => n]);
    var := opts.Vars;
    if (#var >0 and #var < n) then (
		print(" -- warning: Not enough variables given. Creating new ring.");
	) else if #var > 0 then (
		R = F[var];
	);
    B := transpose LLL syz A;
    -- generate list of coefficients for binomials
	try ss = apply(ss, i->substitute(i, R)) else (
	    print(" -- warning: Slack entries cannot be substituted into chosen ring. Coefficients will be set to 1.");
	    ss = for i from 1 to (#gens R) list 1;
	);
    J := ideal apply(entries B, b-> toBin(b, ss, R));
    if opts.Saturate == "all" then (
    	saturate(J, product gens R, Strategy => opts.Strategy)
    ) else if opts.Saturate == "each" then (
	I := J;
    	for i in gens R do I = saturate(I, i, Strategy => opts.Strategy);
    I
    ) else error "Please choose a valid saturation mode"
)

graphFromSlackMatrix = method()
graphFromSlackMatrix Matrix := Matrix => S -> (
    -- INPUT: S = (symbolic) slack matrix
    -- OUTPUT: the vertex-edge adjacency matrix of the bipartite graph with adjacency matrix given by support of S
    n := nonzeroEntries S;
    -- make new symbolic slack with consecutively numbered variables
    y := symbol y; -- don't need to change symbol assignment here because user never accesses this ring
    -- check if S has variable entries and create symbolic version
    coeff := try coefficientRing(ring S) else ring S;
    M := symbolicSlackMatrix(S, CoefficientRing => coeff, Vars => toList(y_1..y_n));
    R := ring M;
    v := numgens target M;
    f := numgens source M;
    e := numgens R; 
    N := mutableMatrix(ZZ, v+f, e);
    for i in (0..v-1) do (
		for j in (0..f-1) do (
	    	var := M_(i, j);
	    	if var != 0 then (
				N_(i, index var) = 1;
				N_(j+v, index var) = 1;
			);
	    );
	);
    -- Display new (possibly relabeled) symbolic slack used to compute the graph
    << "Graph computed from symbolic adjacency matrix: " << M << endl;
    matrix N
)

-- Graphic ideals are defined ONLY for polytopes: they are unscaled cycleIdeals
graphicIdeal = method(Options => {Object => "polytope", Strategy => Eliminate, CoefficientRing => QQ, Vars => {}, Saturate => "all"})
graphicIdeal List := Ideal => opts -> V -> (
    -- INPUT: list V of vertices or facet sets
    -- OUTPUT: the toric ideal of the non-incidence graph of conv(V), or of abstract type of V if Object => abstractPolytope given
    S := symbolicSlackMatrix(V, Object => opts.Object, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
    A := graphFromSlackMatrix S;
    if (opts.Object == "polytope" or opts.Object == "abstractPolytope") then (
		toricIdeal(A, {0}, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate)
	) else if (opts.Object == "matroid") then (
		print("-- warning: Did you mean to find the cycleIdeal?");
		toricIdeal(A, {0}, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate)
	)
    -- should be caught by symbolicSlackMatrix
    else error "Please choose a valid object."
)
graphicIdeal Polyhedron := Ideal => opts -> P -> (
    -- INPUT: polytope P
    -- OUTPUT: the toric ideal of the non-incidence graph of P
    S := symbolicSlackMatrix(P, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
    A := graphFromSlackMatrix S;
    toricIdeal(A, {0}, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate)
)
graphicIdeal Matrix := Ideal => opts -> S -> (
    -- INPUT: slack matrix S of a polytope
    -- OUTPUT: the ideal of the non-incidence graph (graph with support of S as its adjacency matrix) of the polytope
    A := graphFromSlackMatrix S;
    T := toricIdeal(A, {0}, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate);
    -- Check for symbolic original matrix and substitute variables back
    if flatten entries S != flatten entries substitute(S, QQ) then T else (
		R := ring T;
		substitute(T, for i from 1 to numgens R list R_(i-1) => (delete(0_(ring S), flatten entries S))_(i-1))
	)
)
graphicIdeal Matroid := Ideal => opts -> M -> (
    -- INPUT: matroid M 
    -- OUTPUT: the toric ideal of the non-incidence graph of M
    print("-- warning: Did you mean to find the cycleIdeal?");
    S := symbolicSlackMatrix(M, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
    A := graphFromSlackMatrix S;
    toricIdeal(A, {0}, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate)
)

cycleIdeal = method(Options => {Object => "polytope", Strategy => Eliminate, CoefficientRing => QQ, Vars => {}, Saturate => "all"})
cycleIdeal List := Ideal => opts -> V -> (
    -- INPUT: list of vertices of polytope, or of vectors realization of a matroid if Object => matroid,
    --        or of facets if Object => abstractPolytope
    -- OUTPUT: cycle ideal of the realization, or graphic ideal if Object => abstractPolytope
    B := {};
    if opts.Object == "abstractPolytope" then (
		print("-- warning: No realization given. Returning graphicIdeal instead.");
        B = symbolicSlackMatrix(V, Object => opts.Object, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
		graphicIdeal(B, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
	) else (
		B = slackMatrix(V, Object => opts.Object);
		ss := delete(0_(ring B), flatten entries B);
		A := graphFromSlackMatrix B;
		toricIdeal(A, ss, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate)
	)
)
cycleIdeal Matroid := Ideal => opts -> M -> (
    -- INPUT: matroid M with ground set given by vectors
    -- OUTPUT: cycle ideal of the realization
    --         graphicIdeal is returned if realization of vectors is not given
    Sym := symbolicSlackMatrix(M, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
    A := graphFromSlackMatrix Sym;
    if instance((M_*)_0, Matrix) then (
		-- get entries of slack matrix for binomial coefficients
		S := slackMatrix M;
		ss := delete(0_(ring S), flatten entries S);
		toricIdeal(A, ss, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate)
	) else (
		print("-- warning: No realization given. Returning graphicIdeal instead.");
		toricIdeal(A, {0}, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate)
	)
)
cycleIdeal Polyhedron := Ideal => opts -> P -> (
    -- INPUT: polytope P
    -- OUTPUT: cycle ideal of P
    S := slackMatrix P;   
    ss := delete(0_(ring S), flatten entries S);
    A := graphFromSlackMatrix S;
    toricIdeal(A, ss, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate)
)
cycleIdeal Matrix := Ideal => opts -> S -> (
    -- INPUT: slack matrix S
    -- OUTPUT: cycle ideal of the realization S
    -- check that an actual realization is given
    if instance(ring S, PolynomialRing) and (flatten entries S != flatten entries substitute(S, coefficientRing(ring S))) then (
		error "Please give the slack matrix of a realization."
	);
    ss := delete(0_(ring S), flatten entries S);
    A := graphFromSlackMatrix S;
    toricIdeal(A, ss, Strategy => opts.Strategy, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, Saturate => opts.Saturate)   
)

-- the universal realization ideal is ONLY defined for matroids
universalIdeal = method(Options => {CoefficientRing => QQ, Vars => {}})
universalIdeal Matroid := Ideal => opts -> M -> (
    -- INPUT: matroid M
    -- OUTPUT: universal realization ideal of M
    F := opts.CoefficientRing;
    d := (rank M)-1;
    n := #M.groundSet;
    S := symbolicSlackMatrix(M, CoefficientRing => F, Vars => opts.Vars);
    G := Grassmannian(d, n-1, CoefficientRing => F);
    R := ring G;
    -- gets the index of the sets of non-bases in the list of subsets of {0..n-1} of size d+1
    -- this gives the index of the correspond plucker variable in the list of gens R
    NBind := apply(nonbases M, NB -> position(subsets(toList(0..n-1), d+1), J -> (J==sort(toList NB))));
    P := substitute(G, for i in NBind list R_i => 0);
    T := F[gens ring S, gens ring G];
    U := substitute(P, T);
    H := hyperplanes M;
    for j from 0 to #H-1 do (
		SS := slackToPluckerMatrix(toList(H_j), n, d, R);
		c1 := submatrix(S, sort(toList(set(0..n-1)-H_j)), {j});
		mat := substitute(c1, T) | substitute(matrix SS, T);
		U = U + minors(2, mat);
	);
    U
)
universalIdeal List := Ideal => opts -> V -> (
    -- INPUT: realization of a matroid M[1 V]
    -- OUTPUT: universal realization ideal of M
    V1 := for i to #V-1 list {1} | V_i;
    M := matroid(transpose matrix V1);
    universalIdeal(M, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
)

slackToPluckerMatrix = (H, n, d, R) -> (
    -- INPUT: list of hyperplane elements H, number of elements of the matroid n,
    --        rank of matroid is d+1, ring of the Grassmannian of the matroid R
    -- OUTPUT: the substitution matrix M_H
    cols := subsets(H, d);
    rows := sort(toList(set(0..n-1)-H));
    -- Plucker variable p_({r}|c) has to be retrieved as the generator of R having gets the
    -- index given by the position of {r}|c in the list of subsets of {0..n-1} of size d+1
    for r in rows list for c in cols list sgn({r}, c)*R_(position(subsets(toList(0..n-1), d+1), J -> (J==sort(join({r}, c)))))
)

fromBinomial = b -> (
    -- INPUT: binomial b
    -- OUTPUT: kernel vector of exponents of b
    -- does not check binomiality, if b has >2 terms the exponent vector of the first two will be returned
    E := exponents(b);
    E_0 - E_1
)

toricPolytope = method()
toricPolytope Ideal := Matrix => I -> (
    -- INPUT: toric ideal I
    -- OUTPUT: matrix whose columns represent the vector configuration with toric ideal I
    K := for f in flatten entries gens I list fromBinomial(f);
    transpose LLL syz matrix K
)

setOnesForest = method()
setOnesForest Matrix := (Matrix, Graph) => X -> (
    -- INPUT: symbolic slack matrix X,
    -- OUTPUT: symbolic slack matrix with spanning forest of variables replaced by 1s, spanning forest
    a := numgens target X;
    b := numgens source X;
    -- ring of variables representing nodes of the graph
    S := QQ(monoid[VariableBaseName => "y", Variables => a+b]);
    E := {};
    -- add edges to the graph corresponding to variables in the slack matrix X
    for i in (0..a-1) do (
		for j in (0..b-1) do (
	    	if X_(i, j) != 0_(ring X) then (
				E = E | {{S_(i), S_(j+a)}};
			);
	    );
	);
    G := graph(S, E);
    T := spanningTree G;
    W := X;
    -- index of nodes in edges of tree give position in matrix to set to 1
    for k in edges(T) do (
		W = substitute(W, {X_(index(k_0), index(k_1)-a) => 1});
	);
    (W, T)
)
  
varEntriesIndices = (M, ind, rc) ->(
    -- INPUT: matrix M, index of row or column ind, 'r' for row or 'c' for column
    -- OUTPUT: list of indices of variables in the specified row or column of the matrix M
    L := {};
    if rc=="r" then (
		L = flatten entries M^{ind} - set{0_(ring M), 1_(ring M)};
	) else (
		L = flatten entries M_ind - set{0_(ring M), 1_(ring M)};
	);
    sort(apply(L, v->index(v)+1))
)

distVU = (G, v, u) -> (
    -- INPUT: graph G, vertices v, u of G
    -- OUTPUT: distance from v to u in G
    if not member(v, vertices G) or not member(u, vertices G) then error "The given vertices are not vertices of G.";
    n := #vertices G;
    v = position(vertices G, i -> i == v);
    u = position(vertices G, i -> i == u);
    C := new MutableList from toList(#vertices G:infinity);
    Q := {v};
    C#v = 0;
    while #Q != 0 do (
        y := first Q;
        Q = drop(Q, 1);
        N := select(positions(first entries (adjacencyMatrix G)^{y}, j -> j != 0), x -> C#x == infinity);
        if any(N, x -> x == u) then (
            C#u = C#y + 1;
            break;
        );
        Q = Q | N;
        for z in N do C#z = C#y + 1;
        );
    C#u
)

rehomogenizePolynomial = method()
rehomogenizePolynomial Matrix := X -> (
    -- INPUT: symbolic slack matrix X
    -- OUTPUT: rehomogenization of a polynomial entered by the user, reversing the dehomogenization of the slack matrix
    (Y, T) := setOnesForest X;
    -- remVars is the list of variables that are not set to 1
    remVars := flatten entries Y - set{0_(ring Y), 1_(ring Y)};
    print("Write a polynomial in the variables", toString remVars);
    f := read " ";
    g := value f;
    while not isSubset(apply(support g, v->(ring X)_(index(v))), remVars) do (
		print("Warning: polynomial in the wrong variables");
		print("Write a polynomial in the variables", toString remVars);
		f = read " ";
		g = value f;
	);
    Z := Y;
    a := numgens target Y; -- #rows of Y
    verticesT := vertices T;
    -- dist is a reverse sorted list of couples, where the 1st element is the distance between vertex v and vertex 1 and the 2nd element is vertex v
    connComp := connectedComponents(T);
    for C in connComp do (
		verticesC := C;
		dist := rsort(apply(verticesC, v->{distVU(T, verticesC_0, v), v}));
		newVertices := {};
		for c in (0..#dist-2) do (
			newVert := index((dist_c)_1)+1;
			newVertices = newVertices | {newVert};
			varsInd := {};
			newIndex := -1;
			newVar := 0;
			variablesInd := {};
			-- we identify the row or column containing exactly one 1 entry
			if newVert<=a then (
				-- varsInd is the list of the indices of the variables in the row of index newVert-1 of Z
				varsInd = varEntriesIndices(Z, newVert-1, "r");
				-- newIndex is the column index of the variable wrt which we want to homogenize g
				newIndex = (toList(apply(neighbors(T, verticesT_((newVert)-1)), w->index(w)+1)-set(newVertices)))_0-a-1;
				-- newVar is the variable wrt which we want homogenize g
				newVar = X_(newVert-1, newIndex);
				Z = mutableMatrix Z;
				Z_(newVert-1, newIndex) = X_(newVert-1, newIndex);
			) else (
				-- varsInd is the list of the indices of the variables in the row of index newVert-1 of Z
				varsInd = varEntriesIndices(Z, newVert-a-1, "c");
				-- newIndex is the column index of the variable wrt which we want to homogenize g
				newIndex = (toList(apply(neighbors(T, verticesT_((newVert)-1)), w -> index(w)+1)-set(newVertices)))_0-1;
				-- newVar is the variable wrt which we want homogenize g
				newVar = X_(newIndex, newVert-a-1);
				Z = mutableMatrix Z;
				Z_(newIndex, newVert-a-1) = X_(newIndex, newVert-a-1);
			);
			Z = matrix Z;
			-- newVarIndex is the index of the new variable that we add to the list varsInd
			newVarIndex := index(newVar)+1;
			variablesInd = sort(varsInd | {newVarIndex});
			-- weights is the vector of the weights of the variables (we write 1 for the variables in varsInd, 0 otherwise)
			weights := {};
			for i in (0..dim(ring X)-1) do (
				if member(i+1, variablesInd) then weights = weights | {1} else weights = weights | {0};
			);
			g = homogenize(g, newVar, weights);
		);
	);
    g
)
rehomogenizePolynomial (Matrix, Matrix, Graph, RingElement) := (X, Y, T, g) -> (
    -- INPUT: symbolic slack matrix X, dehomogenized slack matrix Y, spanning forest T, polynomial g
    -- OUTPUT: rehomogenization of g reversing the dehomogenization of the slack matrix
    Z := Y;
    a := numgens target Y; -- #rows of Y
    verticesT := vertices T;
    connComp := connectedComponents(T);
    for C in connComp do (
		verticesC := C;
		dist := rsort(apply(verticesC, v -> {distVU(T, verticesC_0, v), v}));
		newVertices := {};
		for c in (0..#dist-2) do (
			newVert := index((dist_c)_1)+1;
			newVertices = newVertices | {newVert};
			varsInd := {};
			newIndex := -1;
			newVar := 0;
			variablesInd := {};
			if newVert<=a then (
				varsInd = varEntriesIndices(Z, newVert-1, "r");
				newIndex = (toList(apply(neighbors(T, verticesT_((newVert)-1)), w -> index(w)+1)-set(newVertices)))_0-a-1;
				newVar = X_(newVert-1, newIndex);
				Z = mutableMatrix Z;
				Z_(newVert-1, newIndex) = X_(newVert-1, newIndex);
			) else (
				varsInd = varEntriesIndices(Z, newVert-a-1, "c");
				newIndex = (toList(apply(neighbors(T, verticesT_((newVert)-1)), w -> index(w)+1)-set(newVertices)))_0-1;
				newVar = X_(newIndex, newVert-a-1);
				Z = mutableMatrix Z;
				Z_(newIndex, newVert-a-1) = X_(newIndex, newVert-a-1);
			);
			Z = matrix Z;
			newVarIndex := index(newVar)+1;
			variablesInd = sort(varsInd | {newVarIndex});
			weights := {};
			for i in (0..dim(ring Y)-1) do (
				if member(i+1, variablesInd) then weights = weights | {1} else weights = weights | {0};
			);
			g = homogenize(g, newVar, weights);
		);
	);
    g
)

rehomogenizeIdeal = method(Options => {Strategy => Eliminate, Saturate => "all"})
rehomogenizeIdeal (ZZ, Matrix) := Ideal => opts -> (d, X) -> (
    -- INPUT: symbolic slack matrix X of d-polytope
    -- OUTPUT: rehomogenization of the dehomogenized slack ideal
    (Y, T) := setOnesForest(X);
    varsY := flatten entries Y - set{0_(ring Y), 1_(ring Y)};
    J := minors(d+2, Y);
    dehomIdeal := J;
    if opts.Saturate == "all" then (
    	dehomIdeal = saturate(dehomIdeal, product(varsY), Strategy => opts.Strategy)
    ) else if opts.Saturate == "each" then (
    	for i in varsY do dehomIdeal = saturate(dehomIdeal, i, Strategy => opts.Strategy);
    ) else error "Please choose a valid saturation mode";
    K := ideal(apply(flatten entries gens dehomIdeal, g -> rehomogenizePolynomial(X, Y, T, g)));
    satIdeal := K;
    for a in (gens ring X - set(varsY)) do satIdeal = saturate(satIdeal, a, Strategy => opts.Strategy);
    satIdeal
)
rehomogenizeIdeal (ZZ, Matrix, Graph) := Ideal => opts -> (d, Y, T) -> (
    -- INPUT: dehomogenized symbolic slack matrix Y of d-polytope, spanning forest T whose edges match the ones in Y
    -- OUTPUT: rehomogenization of the dehomogenized slack ideal
    a := numgens target Y;
    b := numgens source Y;
    ones := positions(flatten entries transpose Y, y -> y == 1);
    onesCoord := apply(ones, k -> {k%a, k//a + a});
    indicesEdges := apply(edges T, e->{index e_0, index e_1});
    if onesCoord != indicesEdges then error "The edges of the forest do not match the position of the ones in Y.";
    X := substitute(symbolicSlackMatrix Y, ring Y);
    varsY := flatten entries Y - set{0_(ring Y), 1_(ring Y)};
    -- varsY := apply(flatten entries Y - set{0_(ring Y), 1_(ring Y)}, v -> (ring X)_(index v));
    J := minors(d+2, Y);
    dehomIdeal := J;
    if opts.Saturate == "all" then (
    	dehomIdeal = saturate(dehomIdeal, product(varsY), Strategy => opts.Strategy)
    ) else if opts.Saturate == "each" then (
    	for i in varsY do dehomIdeal = saturate(dehomIdeal, i, Strategy => opts.Strategy);
    ) else error "Please choose a valid saturation mode";
    K := ideal(apply(flatten entries gens dehomIdeal, g -> rehomogenizePolynomial(X, Y, T, g)));
    satIdeal := K;
    for a in (gens ring X - set(varsY)) do satIdeal = saturate(satIdeal, a, Strategy => opts.Strategy);
    satIdeal
)

findFlag = method(Options => {Object => "polytope", FlagElement => 0})
findFlag Polyhedron := List => opts -> P -> (
    -- INPUT: Polytope P
    -- OUTPUT: a list F of facet labels that make up a flag in P, through facet FlagElement if option given
    V := entries transpose Polyhedra$vertices P;  
    -- get list of vertices that make up each facet
    facetList := Polyhedra$facesAsPolyhedra(1, P);
    facetList = apply(facetList, f -> entries transpose Polyhedra$vertices f);
    coatoms := apply(facetList, f -> apply(f, v -> position(V, e -> e==v)));
    << "\nOrder of vertices is " << endl << V << endl;
    << "Order of facets is " << endl << coatoms << endl;
    flagFromPosetCoatoms(opts.FlagElement, coatoms)
)
findFlag Matroid := List => opts -> M -> (
    -- INPUT: a matroid M
    -- OUTPUT: a list F of hyperplane labels that make up a flag in M through hyperplane FlagElement if option given
    H := apply(hyperplanes M, h -> sort(toList h));
    << "\nOrder of hyperplanes is " << endl << H << endl;
    flagFromPosetCoatoms(opts.FlagElement, H)
)
findFlag Matrix := List => opts -> S -> (
    -- INPUT: (symbolic) slack matrix S,
    -- OUTPUT: a list F of column labels that make up a flag in S through column FlagElement if option given
    R := ring S;
    facetList := apply(entries transpose S, r -> positions(r, e -> e==0_R));
    flagFromPosetCoatoms(opts.FlagElement, facetList)
)    
findFlag List := List => opts -> V -> (
    -- INPUT: V = a list of vertex coordinates, matroid vectors, facet labels, or cone generators
    -- OUTPUT: a list F of labels that make up a flag in object given by V through FlagElement if option given
    obj := opts.Object;
    S := symbolicSlackMatrix(V, Object => obj);
    R := ring S;
    coatoms := apply(entries transpose S, r -> positions(r, e -> e==0_R));
    if obj == "matroid" then (
        << "\nOrder of hyperplanes is " << endl;
    ) else (
        << "Order of facets is " << endl;
    ); 
    << coatoms << endl;
    flagFromPosetCoatoms(opts.FlagElement, coatoms)
)

containsFlag = method(Options => {Object => "polytope"})
containsFlag (List, Polyhedron) := Boolean => opts -> (fl, P) -> (
	-- INPUT: list of facets fl, polytope P
	-- OUTPUT: true/false if fl contains a flag of facets for P
	S := slackMatrix P;
	d := dim P;
	if rank(S_fl) == d+1 then true else false
)
containsFlag (List, Matroid) := Boolean => opts -> (fl, M) -> (
	-- INPUT: list of facets fl, matroid M
	-- OUTPUT: true/false if fl contains a flag of facets for M
	S := slackMatrix M;
	d := (rank M) - 1;
	if rank(S_fl) == d+1 then true else false
)	
containsFlag (List, Matrix) := Boolean => opts -> (fl, S) -> (
	-- INPUT: list of facets fl, (symbolic) slack matrix S
	-- OUTPUT: true/false if fl contains a flag of facets for S
	d := findAbstractDim apply(entries transpose S, r -> positions(r, e -> e==0_(ring S)));
	D := findAbstractDim apply(entries transpose S_fl, r -> positions(r, e -> e==0_(ring S)));
	if D == d then true else false
)
containsFlag (List, List) := Boolean => opts -> (fl, V) -> (
	-- INPUT: list of facets fl, V = a list of vertex coordinates, matroid vectors, facet labels, or cone generators
	-- OUTPUT: true/false if fl contains a flag of facets for object represented by V
	S := symbolicSlackMatrix(V, Object=>opts.Object);
	containsFlag(fl, S)
)  

simplicialColExtend := (S, fl, fa) -> (
    -- helper function for reconstructSlackMatrix
    -- INPUT: S a (column) reduced slack matrix, fl a flag of columns, fa the vertices in a simplicial facet column we want to refill
    -- OUTPUT: The column of facet fa as a matrix
    -- check facet is really simplicial
    if #fl != (#fa + 1) then error "Facet is not simplicial";
    v := numgens target S;
    newC := for i from 0 to v-1 list det submatrix(S, join(fa, {i}), fl);
    if positions(newC, e->e==0) != fa then error "Cannot extend matrix";
    transpose matrix {newC}
)
smallColExtend := (S, fl, fa, R, kvars) -> (
    -- helper function for reconstructSlackMatrix
    -- INPUT: S a (column) reduced slack matrix, fl a flag of columns, fa the vertices in a column with less than a simplicial facet many (d) zeros, R ring in which to construct new column with extra free variables kvars
    -- OUTPUT: the column of facet fa as a matrix
    -- check for correct number of elements in facet
    if #fa >= #fl then error "Facet has too many elements";
    K := kernel(submatrix(S, fa, fl));
    M := substitute(K.generators, R);
    -- get a symbolic element of the kernel of the submatrix of S 
    -- corresponding to the elements of column to fill
    v := M*substitute(matrix(for i from 1 to numgens source M list {kvars_(i-1)}), R);
    newC := substitute(S, R)*v;
    if positions(flatten entries newC, e -> e==0_R) != fa then error "Cannot extend matrix";
    newC
)

reconstructSlackMatrix = method(Options => {CoefficientRing => QQ, Vars => {}})
reconstructSlackMatrix (Matrix, List, List) := Matrix => opts -> (S, fl, F) -> (
    -- INPUT: S a (column) reduced slack matrix, fl a flag of columns, F list of facet columns we want to refill
    -- OUTPUT: S extended by the columns of F
    -- If Options are given, new slack matrix is built in ring CoefficientRing[Vars]
    if not containsFlag(fl, S) then error "Given columns don't contain a flag";
    d := #fl-1;
    simpF := select(F, f -> #f==d);
    smallF := select(F, f -> not member(f, simpF));
    -- check smallF is really small facets
    scan(smallF, f -> if d-#f < 0 then error "Cannot reconstruct nonsimplicial facets");
    nVars := sum apply(smallF, f -> d-#f);
    oVars := nonzeroEntries S;
    -- make ring for output matrix
    newR := ring S;
    newS := S;
    -- get coefficient ring of the ring of S, or ring itself if not symbolic 
    coeff := try coefficientRing S else ring S;
    -- if Option Vars are given then assume symbolic matrix to be constructed
    X := monoid[VariableBaseName => "x", Variables => oVars]; -- reduced slack matrix variables
    Y := monoid[VariableBaseName => "y", Variables => nVars]; -- free kernel variables
    if #(opts.Vars) > 0 then (
        newR = opts.CoefficientRing[opts.Vars];
        -- override options if not enough variables are given
        if #(opts.Vars) < nVars+oVars then (
            <<"\nNot enough variables given. Making new ring for reconstructed matrix" << endl;
            newR = coeff(X**Y);
        );
        newS = symbolicSlackMatrix(S, CoefficientRing => coefficientRing newR, Vars => gens newR);
    ) else if nVars > 0 then (
        --even if no variables are given, create ones for underdetermined columns 
        X = monoid[gens ring S];
        newR = coeff(X**Y);
        oVars = #gens(ring S);
    );
    newS = substitute(newS, newR);
    newCols := for f in F list if member(f, simpF) then simplicialColExtend(newS, fl, f) else smallColExtend(newS, fl, f, newR, drop(gens newR, oVars)); 
    scan(newCols, c->(newS = newS | c));
    matrix(for i in (0..numgens target newS-1) list(flatten entries newS^{i}))
)
reconstructSlackMatrix (Matrix, List) := Matrix => opts -> (S, F) -> (
    -- INPUT: S a (column) reduced slack matrix, F list of facet columns we want to refill
    -- OUTPUT: S extended by the columns of F
    -- If Options are given, new slack matrix is built in ring CoefficientRing[Vars]
    fl := findFlag S;
    reconstructSlackMatrix(S, fl, F, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars)
)

reducedSlackMatrix = method(Options => {Object => "polytope", CoefficientRing => QQ, Vars => {}, FlagIndices => {}})
reducedSlackMatrix (ZZ, Matrix) := Matrix => opts -> (d, S) -> (
    -- INPUT: (symbolic) slack matrix S of rank d+1
    -- OUTPUT: reduced slack matrix
    -- find nonsimplicial facet columns
    fl := opts.FlagIndices;
    v := numgens target S;
    nonSimpF := positions(entries transpose S, r -> (v - nonzeroEntries(matrix {r}) > d));
    i := 0;
    -- check if non-simplicial facets contain a flag and the optional given flag
    if containsFlag(nonSimpF, S) then (
        if not isSubset(fl, nonSimpF) then (
            print("-- warning: You've chosen a flag containing simplicial facets");
        ) else (
            if #opts.Vars > 0 then (
		return symbolicSlackMatrix(S_nonSimpF, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars);
		) else return S_nonSimpF;
        );
    );
    -- find flag through some nonSimplicial facet if it exists
    if #nonSimpF > 0 then i = nonSimpF_0;
    nsFl := findFlag(S, FlagElement => i);
    -- check for optional given flag
    if #fl > 0 then (
        if not containsFlag(fl, S) then (
            <<"-- warning: Given facets don't contain a flag. Using new flag" << endl << nsFl << endl;
            fl = nsFl;
        );
    ) else fl = nsFl;
    if #fl != d+1 then (
        print(" -- warning: Flag and matrix ranks don't match");
    );
    cols := sort(unique join(fl, nonSimpF));
    -- put into new ring if Options are given
    if #opts.Vars > 0 then symbolicSlackMatrix(S_cols, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars) else S_cols
)
reducedSlackMatrix Matrix := Matrix => opts -> S -> (
    -- INPUT: (symbolic) slack matrix S
    -- OUTPUT: reduced slack matrix
    fl := findFlag S;
    d := #fl - 1;
    reducedSlackMatrix(d, S, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, FlagIndices=>opts.FlagIndices)
)
reducedSlackMatrix List := Matrix => opts -> V -> (   
    -- INPUT: V = a list of vertex coordinates, matroid vectors, facet labels, or cone generators
    -- OUTPUT: reduced slack matrix of object represented by V
    S := try slackMatrix(V, Object=>opts.Object) else symbolicSlackMatrix(V, Object=>opts.Object);
    d := findAbstractDim apply(entries transpose S, r -> positions(r, e -> e==0_(ring S)));
    reducedSlackMatrix(d, S, CoefficientRing => opts.CoefficientRing, Vars => opts.Vars, FlagIndices => opts.FlagIndices)
)


beginDocumentation()

doc ///
    Key
    	SlackIdeals
    Headline
    	a package for slack ideals of polytopes and matroids
    Description
    	Text
	    
	    @{EM "SlackIdeals"}@ is a package which allows the user to create slack realizations and 
	    the slack ideal of a polytope or a matroid. Polytopes and matroids may be entered as a 
	    list of vertices of a specific realization or as a pre-created @{TT "Polyhedron"}@ or
	    @{TT "Matroid"}@ object (using the packages @TO Polyhedra@ and @TO Matroids@). 
	    
	    @{BOLD "References."}@
	    
	    @UL {
	  LI {"[GMTW19] ", EM "The slack realization space of a polytope, ", "(J. Gouveia, A. Macchia, R.R. Thomas, A. Wiebe, SIAM J. Discrete Math. 33 (2019), 3, 1637–1653.)"},
	  LI {"[BW19] ", EM "The slack realization space of a matroid, ", "(M. Brandt, A. Wiebe, Algebraic Combinatorics, 2 (2019), 4, 663–681, 2019.)"},
	  LI {"[BMTW20] ", EM "Projectively unique polytopes and toric slack ideals, ", "(J. Gouveia, A. Macchia, R.R. Thomas, A. Wiebe, J. Pure Appl. Algebra 224 (2020), 5, paper 106229.)"},
	  LI {"[BMW20] ", EM "Combining realization space models of polytopes, ", "(J. Gouveia, A. Macchia, A. Wiebe, preprint (2020), arXiv:2001.11999v1.)"}}@

///

doc ///
    Key
    	slackMatrix
	(slackMatrix, List)
	(slackMatrix, Matroid)
	(slackMatrix, Polyhedron)
	(slackMatrix, Cone)
    Headline
    	computes the slack matrix of a given realization
    Usage
    	S = slackMatrix V
	S = slackMatrix M
	S = slackMatrix P
	S = slackMatrix C
    Inputs
    	V:List
	    a list of vertices of a polytope, or the vectors of a linear matroid
	M:Matroid
	    a matroid
	P:Polyhedron
	    a polytope
	C:Cone
	    a cone
    Outputs
    	S:Matrix
	    a slack matrix of the realization
    Description
    	Text
	    A slack matrix depends on the given representation. Its rows are indexed by 
	    vertices/ground set elements and its columns are indexed by facets/hyperplanes.
	    The @{TT "(i, j)"}@-entry is the @{TT "j"}@-th inequality evaluated on element 
	    @{TT "i"}@.
	Example
	    V = {{0, 0}, {0, 1}, {1, 1}, {1, 0}};
	    SP = slackMatrix V
	    SM = slackMatrix(V, Object => "matroid")
	Example
	    C = posHull(matrix{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}});
	    S = slackMatrix C
    Caveat
    	When giving a @{TT "Matroid"}@ as input, the ground set must be a set of vectors giving a 
	representation of the matroid. 
    SeeAlso
    	symbolicSlackMatrix
	Object
///

doc ///
    Key
    	getFacetBases
	(getFacetBases, Matrix)
	(getFacetBases, Polyhedron)
	(getFacetBases, Matroid)
	(getFacetBases, List)
    Headline
        get a list of d-spanning elements for each facet
    Usage
    	(newV, B) = getFacetBases S
	(newV, B) = getFacetBases P
	(newV, B) = getFacetBases M
	(newV, B) = getFacetBases V
    Inputs
    	S:Matrix
	    a slack matrix S of rank d+1
	P:Polyhedron
	    a polytope
	M:Matroid
	    a matroid
	V:List
	    a list of vertices of d-polytope, vectors of a rank d+1 matroid, or (d+1)-cone generators
    Outputs
    	newV:List
		a list of vertices (empty if a matrix is given as input) in the order corresponding to B
	B:List
		list of d spanning elements for each facet
    Description
        Text
	    This function produces a list B of d spanning elements for each facet in a given d-polytope,
	    rank d+1 matroid, or (d+1)-cone generators. If a slack matrix or a list of vertices is given as input, it also
	    creates a sorted list of vertices (empty if a matrix is given as input) from which B is computed in the order corresponding to B.
	Example
	    V = {{0, 0}, {1, 0}, {2, 1}, {1, 2}, {0, 1}};
	    (newV, B) = getFacetBases V
	Example
	    V = {{0, 0}, {1, 0}, {1, 1}, {0, 1}};
	    S = slackMatrix V;
	    (newV, B) = getFacetBases S
    SeeAlso
    	slackFromPlucker
	symbolicSlackOfPlucker
	grassmannSectionIdeal
///

doc ///
    Key
    	slackFromPlucker
	(slackFromPlucker, List, List)
	(slackFromPlucker, List)
	(slackFromPlucker, Polyhedron)
	(slackFromPlucker, Matroid)
    Headline
        fill the slack matrix of a given polytope, cone or matroid with Plucker coordinates
    Usage
    	fillPl = slackFromPlucker(V, B)
	fillPl = slackFromPlucker V
	fillPl = slackFromPlucker P
	fillPl = slackFromPlucker M
    Inputs
    	V:List
	    list of coordinates for polytope vertices, cone generators, or matroid vectors
	B:List
	    set of hyperplane spanning set indices
	P:Polyhedron
	    a polytope
	M:Matroid
	    a matroid
    Outputs
    	fillPl:Matrix
		slack matrix filled with Plucker coordinates
    Description
    	Text
	    Given a slack matrix or a list of vertices of d-polytope or a rank d+1 matroid, or (d+1)-cone generators,
	    it fills the corresponding slack matrix with Plucker coordinates.
	Example
	    V = {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 0, 1}, {1, 1, 0}};
	    B = {{1, 2, 4}, {0, 2, 3}, {0, 1, 4}, {3, 4, 5}, {0, 1, 2}};
	    fillPl = slackFromPlucker(V, B)
	Example
	    V = {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 0, 1}, {1, 1, 0}};
	    fillPl = slackFromPlucker V
    Caveat
    	Does not check if B actually spans for given V, does not check orientation of simplices in B, so that the resulting slack matrix may differ by signs of each column.
    SeeAlso
    	getFacetBases
///

doc ///
    Key
    	slackFromGalePlucker
	(slackFromGalePlucker, List, List)
	(slackFromGalePlucker, List, Matrix)
    Headline
	fill the slack matrix with Plucker coordinates of the Gale transform
    Usage
    	fillPl = slackFromGalePlucker(B, G)
	fillPl = slackFromGalePlucker(B, MG)
    Inputs
    	B:List
	    set of hyperplane spanning set indices
	G:List
	    list of Gale vectors of a polytope
	MG:Matrix
	    matrix of Gale vectors of a polytope
    Outputs
    	fillPl:Matrix
		slack matrix filled with Plucker coordinates
    Description
        Text
	    Given a set of vectors of a Gale transform or a matrix whose columns form a Gale transform of a polytope,
	    it fills the slack matrix of the polytope with Plucker coordinates of the Gale transform.
	Example
	    G = {{0, 1}, {1, 0}, {-1, -1}, {0, -1}, {-1, 0}, {1, 1}};
	    B = {{1, 2, 4}, {0, 2, 3}, {0, 1, 4}, {3, 4, 5}, {0, 1, 2}};
	    slackFromGalePlucker(B, G)
	Example
	    MG = matrix(RR, {{0, 1, -1, 0, -1, 1}, {1, 0, -1, -1, 0, 1}});
	    B = {{1, 2, 4}, {0, 2, 3}, {0, 1, 4}, {3, 4, 5}, {0, 1, 2}};
	    slackFromGalePlucker(B, MG)
    Caveat
    	Does not check if B actually spans for given V, does not check orientation of simplices in B, so that the resulting slack matrix may differ by signs of each column.
    SeeAlso
    	getFacetBases
	slackFromPlucker
///

doc ///
    Key
    	slackFromGaleCircuits
    	(slackFromGaleCircuits, Matrix)
    Headline
        computes the slack matrix of a polytope from a Gale transform of the polytope
    Usage
    	M = slackFromGaleCircuits(G, Tolerance => 12)
    	M = slackFromGaleCircuits G
    Inputs
        G:Matrix
	    matrix of the form matrix(RR, {...}) whose columns are the vectors of a Gale transform
    Outputs
    	SP:Matrix
	    the slack matrix of the polytope whose Gale transform is G
    Description
        Text
	    This function computes the slack matrix of a polytope starting from a matrix whose columns correspond to the vertices of a Gale transform of the polytope. The second argument Tolerance, which is optional, is a positive integer that expresses the tolerance used in the computations, i.e. if an entry of a matrix is less than 10^(-Tolerance), then it will be treated as zero. The default value for the tolerance is 14.
        Example
	    -- Let G be the Gale transform of the triangular prism
	    G = matrix(RR, {{0, 1, -1, 0, -1, 1}, {1, 0, -1, -1, 0, 1}});
	    M = slackFromGaleCircuits G
        Example
	    -- Let G be the Gale transform of the octahedron
	    G = matrix(RR, {{1, 1, -1, -1, 0, 0}, {1, 1, 0, 0, -1, -1}});
	    M = slackFromGaleCircuits G
        Example
	    -- Let G be the Gale transform of the bipyramid over a square pyramid
	    G = matrix(RR, {{1, 0, 1, 0, 2, -2, -2}, {0, 1, 0, 1, 2, -2, -2}});
	    M = slackFromGaleCircuits G
        Example
	    -- Let G be the Gale transform of the cyclic polytope in R^3 with seven vertices
	    G = matrix(RR, {{1, -4, 6, -4, 1, 0, 0}, {4, -15, 20, -10, 0, 1, 0}, {10, -36, 45, -20, 0, 0, 1}});
	    M = slackFromGaleCircuits G
        Example
	    -- Let G be the Gale transform of the Perles polytope
	    G = matrix(RR, {{-sqrt(10+2*sqrt(5))*(sqrt(5)+1)/(4*(sqrt(5)-1)), sqrt(10+2*sqrt(5))*(sqrt(5)+1)/(4*(sqrt(5)-1)), -4*sqrt(5)/(sqrt(10+2*sqrt(5))*(sqrt(5)+1)+(sqrt(5)-5)*sqrt(10-2*sqrt(5))), 4*sqrt(5)/(sqrt(10+2*sqrt(5))*(sqrt(5)+1)+(sqrt(5)-5)*sqrt(10-2*sqrt(5))), -sqrt(10-2*sqrt(5))/4, sqrt(10-2*sqrt(5))/4, -sqrt(10+2*sqrt(5))/4, sqrt(10+2*sqrt(5))/4, -sqrt(10-2*sqrt(5))/4, sqrt(10+2*sqrt(5))/4, -sqrt(10+2*sqrt(5))/4, 0}, {(sqrt(5)+1)/4, (sqrt(5)+1)/4, (-5-sqrt(5))/(6*sqrt(5)-10), (-5-sqrt(5))/(6*sqrt(5)-10), (sqrt(5)+1)/4, (sqrt(5)+1)/4, -(sqrt(5)-1)/4, -(sqrt(5)-1)/4, -(sqrt(5)+1)/4, (sqrt(5)-1)/4, (sqrt(5)-1)/4, 0}, {1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1}});
	    M = slackFromGaleCircuits G
    SeeAlso
    	slackMatrix
	symbolicSlackMatrix
///

doc ///
	Key
		specificSlackMatrix
		(specificSlackMatrix, String)
	Headline
		creates built-in slack matrices of some polytopes and matroids
	Usage
		specificSlackMatrix name
	Inputs
		name:String
			the name of the polytope or of matroids
	Outputs
		:Matrix
	Description
		Text
			Returns a slack matrix of one of the named polytopes and matroids below.
		Example
			peek specificSlackMatrix "perles1"
			peek specificSlackMatrix "perles2"
			peek specificSlackMatrix "barnette"
			peek specificSlackMatrix "toric-non-graphic"
			peek specificSlackMatrix "pu-non-mcmullen"
			peek specificSlackMatrix "fano-matroid"
			peek specificSlackMatrix "complex-matroid"
			peek specificSlackMatrix "nonfano-matroid"
			peek specificSlackMatrix "perles-matroid"
			peek specificSlackMatrix "perles-matroid-QQ"
///

doc ///
    Key
    	symbolicSlackMatrix
	(symbolicSlackMatrix, List)
	(symbolicSlackMatrix, Matrix)
	(symbolicSlackMatrix, Matroid)
	(symbolicSlackMatrix, Polyhedron)
	(symbolicSlackMatrix, Cone)
    Headline
    	computes the symbolic slack matrix
    Usage
    	S = symbolicSlackMatrix V
	S = symbolicSlackMatrix X
	S = symbolicSlackMatrix M
	S = symbolicSlackMatrix P
	S = symbolicSlackMatrix C
    Inputs
    	V:List
	    a list of points treated as the vertices of polytope; or vectors of a linear matroid 
	    if matroid is given as @{TO Object}@; or a list of indices comprising the facets of an 
	    abstract polytope if abstractPolytope is given as @{TO Object}@.
	X:Matrix
	    a slack matrix
	M:Matroid
	    a matroid
	P:Polyhedron
	    a polytope
	C:Cone
	    a cone
    Outputs
    	S:Matrix
	    the symbolic slack matrix with indexed variables in symbol x
    Description 
    	Text
	    The symbolic slack matrix records the combinatorial structure of the given object. Its 
	    @{TT "(i, j)"}@-entry is 0 if element @{TT "i"}@ is in hyperplane @{TT "j"}@ and it is 
	    a variable otherwise. Variables are indexed left to right by rows.
	Example
	    V = {{0, 0}, {0, 1}, {1, 1}, {1, 0}};
	    S = symbolicSlackMatrix V
	Example
	    M = matroid({0, 1, 2, 3, 4, 5}, {{1, 2, 3}, {0, 2, 4}, {0, 3, 5}, {1, 4, 5}}, EntryMode => "nonbases");
	    S = symbolicSlackMatrix M
	Example
	    V = {{1, 2, 3}, {4, 5, 6}, {1, 2, 4, 5}, {1, 3, 4, 6}, {2, 3, 5, 6}};
	    S = symbolicSlackMatrix(V, Object => "abstractPolytope")
    SeeAlso
    	slackMatrix
	Object
///

doc ///
    Key
    	symbolicSlackOfPlucker
	(symbolicSlackOfPlucker, ZZ, List)
	(symbolicSlackOfPlucker, List, List)
	(symbolicSlackOfPlucker, List)	
	(symbolicSlackOfPlucker, Matrix, List)
	(symbolicSlackOfPlucker, Matrix)
	(symbolicSlackOfPlucker, Polyhedron)
	(symbolicSlackOfPlucker, Matroid)
    Headline
        fill the slack matrix with Plucker variables
    Usage
    	fillPl = symbolicSlackOfPlucker(v, B)
	fillPl = symbolicSlackOfPlucker(V, B)
	fillPl = symbolicSlackOfPlucker V
	fillPl = symbolicSlackOfPlucker(S, B)
	fillPl = symbolicSlackOfPlucker S
	fillPl = symbolicSlackOfPlucker P
	fillPl = symbolicSlackOfPlucker M
    Inputs
    	v:ZZ
	    number of polytope vertices, cone generators, or matroid vectors
	B:List
	    set of hyperplane spanning set indices
	V:List
	    list of coordinates for polytope vertices, cone generators, or matroid vectors
	S:Matrix
	    (symbolic) slack matrix
	P:Polyhedron
	    a polytope
	M:Matroid
	    a matroid
    Outputs
    	fillPl:Matrix
		slack matrix filled with Plucker variables
    Description
    	Text
	    Given the number of polytope vertices, cone generators, or matroid vectors, or a set of polytope vertices,
	    cone generators, or matroid vectors, or a slack matrix and a set of set of hyperplane spanning set indices,
	    it fills the slack matrix with Plucker variables.
	Example
	    v = 6;
	    B = {{1, 2, 4}, {0, 2, 3}, {0, 1, 4}, {3, 4, 5}, {0, 1, 2}};
	    fillPl = symbolicSlackOfPlucker(v, B)
	Example
	    V = {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 0, 1}, {1, 1, 0}};
	    B = {{1, 2, 4}, {0, 2, 3}, {0, 1, 4}, {3, 4, 5}, {0, 1, 2}};
	    fillPl = symbolicSlackOfPlucker(V, B)
	Example
	    V = {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 0, 1}, {1, 1, 0}};
	    fillPl = symbolicSlackOfPlucker V
	Example
	    V = {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 0, 1}, {1, 1, 0}};
	    B = {{1, 2, 4}, {0, 2, 3}, {0, 1, 4}, {3, 4, 5}, {0, 1, 2}};
	    fillPl = symbolicSlackOfPlucker(slackMatrix V, B)
	Example
	    V = {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 0, 1}, {1, 1, 0}};
	    fillPl = symbolicSlackOfPlucker(slackMatrix V)
    SeeAlso
    	getFacetBases
	slackFromPlucker
	slackFromGalePlucker
///

doc ///
    Key
    	slackIdeal
	(slackIdeal, ZZ, List)
	(slackIdeal, Matroid)
	(slackIdeal, Polyhedron)
	(slackIdeal, Cone)    
	(slackIdeal, List)
	(slackIdeal, ZZ, Matrix)
	(slackIdeal, Matrix)    
    Headline
    	computes the slack ideal
    Usage
    	I = slackIdeal(d, V)
	I = slackIdeal M
	I = slackIdeal P
	I = slackIdeal C	
	I = slackIdeal V
	I = slackIdeal(d, S)
	I = slackIdeal S
    Inputs
    	d:ZZ
	    the dimension of the polytope or 1 less than the rank of the matroid
	V:List
	    a list of vertices of a polytope, the vectors of a linear matroid, or facet sets of an abstract polytope
	M:Matroid
	    a matroid
	P:Polyhedron
	    a polytope
	C:Cone
	    a cone	    
	S:Matrix
	    a (symbolic) slack matrix 
    Outputs
    	I:Ideal
	    the slack ideal - the saturated ideal of (d+2)-minors of the symbolic slack matrix
    Description
    	Text
	    The slack ideal of a d-polytope or rank d+1 matroid is the ideal of (d+2)-minors
	    of its symbolic slack matrix, saturated by the product of the variables in the matrix. 
	Example
	    V = {{0, 0}, {1, 0}, {1, 1}, {0, 1}};
	    I = slackIdeal V
	Text
	    If a list of points is given it can be treated as the vertices of a polytope,
	    the ground set of a matroid or the facets of an abstract polytope by specifying 
	    the option @TO{Object}@. The default is as a polytope.
	Example
	    V = {{0, 0}, {1, 0}, {1, 1}, {0, 1}};
	    IP = slackIdeal V
	    IM = slackIdeal(V, Object => "matroid")
    SeeAlso
    	slackMatrix
	symbolicSlackMatrix
///

doc ///
    Key
    	grassmannSectionIdeal
	(grassmannSectionIdeal, Matrix, List)
	(grassmannSectionIdeal, Matrix)
	(grassmannSectionIdeal, List, List)
	(grassmannSectionIdeal, List)
	(grassmannSectionIdeal, Polyhedron)
	(grassmannSectionIdeal, Cone)
	(grassmannSectionIdeal, Matroid)
    Headline
        compute the Grassmannian section ideal corresponding to a slack matrix
    Usage
    	I = grassmannSectionIdeal(S, B)
	I = grassmannSectionIdeal S
	I = grassmannSectionIdeal(V, B)
	I = grassmannSectionIdeal V
	I = grassmannSectionIdeal P
	I = grassmannSectionIdeal C
	I = grassmannSectionIdeal M
    Inputs
    	S:Matrix
	    slack matrix
	B:List
	    set of hyperplane spanning set indices
	V:List
	    list of polytope vertex coordinates, cone generators, or matroid vectors
	P:Polyhedron
	    a polytope
	C:Cone
	    a cone	  
	M:Matroid
	    a matroid
    Outputs
    	I:Ideal
		the Grassmannian section ideal corresponding to choice B of the object with slack matrix S
    Description
    	Text
	    Given a slack matrix of a polytope, a cone or a matroid, or a set of polytope vertices, cone generators,
	    or matroid vectors, and a set of set of hyperplane spanning set indices, it computes the Grassmannian section
	    ideal corresponding to choice B of the object with slack matrix S.
	Example
	    V = {{0, 0}, {1, 0}, {2, 1}, {1, 2}, {0, 1}};
	    (VV, B) = getFacetBases V;
	    I = grassmannSectionIdeal(VV, B)
	Example
	    V = {{0, 0}, {1, 0}, {2, 1}, {1, 2}, {0, 1}};
	    (VV, B) = getFacetBases V;
	    I = grassmannSectionIdeal(slackMatrix(VV), B)
    SeeAlso
    	getFacetBases
	slackFromPlucker
	slackFromGalePlucker
	symbolicSlackOfPlucker
///

doc ///
    Key
    	graphFromSlackMatrix
	(graphFromSlackMatrix, Matrix)
    Headline
    	creates the vertex-edge incidence matrix for the bipartite non-incidence graph with adjacency matrix the given slack matrix
    Usage
    	A = graphFromSlackMatrix S
    Inputs
    	S:Matrix
	    a (symbolic) slack matrix
    Outputs
    	A:Matrix
	    0-1 vertex-edge incidence matrix
    Description
    	Text
	    This function creates a matrix whose columns correspond to the edges of the 
	    non-incidence graph of a slack matrix. They are ordered by variable index.
	    The first n rows correspond to rows of the slack matrix (vertices/ground
	    set elements) and the remaining rows correspond to columns of the slack matrix 
	    (facets/hyperplanes).
	Example
	    V = {{0, 0}, {1, 0}, {1, 1}, {0, 1}};
	    S = symbolicSlackMatrix V
	    A = graphFromSlackMatrix S
    Caveat
    	If S is not a symbolic slack matrix, the columns of A will be ordered assuming
	edges are listed in order by rows of S (as in @TO{symbolicSlackMatrix}@).
    SeeAlso
    	graphicIdeal
	symbolicSlackMatrix
///

doc ///
    Key
    	graphicIdeal
	(graphicIdeal, List)
	(graphicIdeal, Polyhedron)
	(graphicIdeal, Matrix)
	(graphicIdeal, Matroid)

    Headline
    	creates the toric ideal of the non-incidence graph of a polytope
    Usage
    	T = graphicIdeal V
	T = graphicIdeal P
	T = graphicIdeal S
	T = graphicIdeal M
    Inputs
    	V:List
	    a list of vertices or of facet sets
	P:Polyhedron
	    a polytope
	S:Matrix
	    a (symbolic) slack matrix of a polytope
	M:Matroid
	    a matroid
    Outputs
    	T:Ideal
	    the toric ideal of the non-incidence graph 
    Description
    	Text
	    The graphic ideal associated to a polytope is the toric ideal of the vector
	    configuration consisting of the columns of the vertex-edge incidence matrix of the
	    non-incidence (of vertices and facets) graph of the polytope.  
	Example
	    P = convexHull(matrix{{0, 0, 1, 1}, {0, 1, 0, 1}});
	    T = graphicIdeal P
    Caveat
    	If S is not a symbolic slack matrix, the ideal will have variables indexed as in 
	@TO{symbolicSlackMatrix}@ (from left to right in order by rows of S).
    SeeAlso
    	graphFromSlackMatrix
	cycleIdeal
	symbolicSlackMatrix
///

doc ///
    Key
    	cycleIdeal
	(cycleIdeal, List)
	(cycleIdeal, Matroid)
	(cycleIdeal, Polyhedron)
	(cycleIdeal, Matrix)
    Headline
    	constructs the cycle ideal of a realization
    Usage
    	C = cycleIdeal V
	C = cycleIdeal M
	C = cycleIdeal P
	C = cycleIdeal S
    Inputs
    	V:List
	    a list of points of a realization
	M:Matroid
	    a matroid
	P:Polyhedron
	    a polytope
	S:Matrix
	    a slack matrix of some realization
    Outputs
	C:Ideal
		the cycle ideal with coefficients from given realization
    Description
        Text
	    A cycle ideal is the toric ideal generated by the cycles of the bipartite non-incidence 
	    graph on vertices/elements and facets/hyperplanes. Each cycle binomial has coefficients
	    coming from the entries of a slack matrix of a realization of the given object. 
	    This method computes the cycle ideal of the given realization of a polytope or matroid.
    	Example
	    V = {{0, 0}, {1, 0}, {1, 1}, {0, 1}};
	    C = cycleIdeal V
	Text
	    You can create the cycle ideal of any matrix, even if it is not the realization
	    of some matroid or polytope.
	Example
	    M = matrix{{1, 1, 0}, {0, 1, 2}, {1, 0, 3}};
	    C = cycleIdeal M
    Caveat
    	Variables in the cycle ideal C will be labelled according to the labeling assigned by
	@TO{symbolicSlackMatrix}@, which is in order by rows, so to test equality of C with some precomputed
	ideal care needs to be taken that variable labels match.
	
	If @TO{Object}@ => abstractPolytope is chosen, the graphic ideal will be returned instead. 
    SeeAlso
    	graphicIdeal
	slackMatrix
///

doc ///
    Key
    	universalIdeal
	(universalIdeal, Matroid)
	(universalIdeal, List)
    Headline
    	computes the universal realization ideal of a matroid
    Usage
    	U = universalIdeal M
	U = universalIdeal V
    Inputs
    	M:Matroid
	    a matroid
	V:List
	    a list of points of a representation of a matroid
    Outputs
    	U:Ideal
	    the universal realization ideal in Pluecker and slack variables
    Description
    	Text
	    The universal realization ideal defines a variety containing points (p, S) which
	    are pairs of a Pluecker vector and a slack matrix coming from the same realization
	    of the matroid. 
	Example
	    M = matroid({0, 1, 2, 3, 4, 5}, {{1, 2, 3}, {0, 2, 4}, {0, 3, 5}, {1, 4, 5}}, EntryMode => "nonbases");
	    U = universalIdeal M
    Caveat
    	A list of points representing a matroid are always assumed to be in affine space and are 
	projectivized by appending 1 to vector of coordinates. 
    SeeAlso
    	slackMatrix
///

doc ///
    Key
    	toricPolytope
	(toricPolytope, Ideal)
    Headline
    	computes the polytope whose toric ideal is the given ideal
    Usage
    	Q = toricPolytope I
    Inputs
    	I:Ideal
	    a toric ideal
    Outputs
    	Q:Matrix
	    a matrix whose columns are the vectors of a configuration whose toric ideal is I
    Description
    	Text
    	    Every toric ideal comes as the ideal of some vector configuration. This function 
	    gives one such vector configuration whose ideal is the given ideal. 
	Example
	    V = {{0, 0}, {0, 1}, {1, 1}, {1, 0}};
	    I = slackIdeal V
	    Q = toricPolytope I
    Caveat
    	This function does not test for binomiality. If it is given an ideal which is not toric,
	it will return the polytope of the ideal whose generators are the first two terms of 
	each generator of the input ideal. If the input toric ideal is scaled (lattice ideal with
	    not all coefficients one) the toric polytope of the unscaled ideal is returned. 
    SeeAlso
    	graphicIdeal
	cycleIdeal
///

doc ///
    Key
    	setOnesForest
	(setOnesForest, Matrix)
    Headline
    	sets to 1 variables in a symbolic slack matrix which corresponding to edges of a spanning forest  
    Usage
    	(Y, F) = setOnesForest X
    Inputs
    	X:Matrix
	    a symbolic slack matrix
    Outputs
    	Y:Matrix
	    the matrix X with the variables corresponding to the edges of a spanning forest set to 1
	F:Graph
	    spanning forest of the bipartite (non-incidence) graph whose vertices are the vertices and 
	    facets of a polytope P, and whose edges are the vertex-facet pairs of P such that the vertex 
	    is not on the facet
    Description
    	Text
	    Since row and column scaling operations preserve the projective equivalence class of a slack matrix, 
	    it is sometimes advantageous to choose a certain equivalence class representative by setting some 
	    variables in a slack matrix to 1. This reduces the computational time of calculating the slack ideal, 
	    for example. 
       
            This function automates the selection of a spanning forest in the bipartite graph corresponding 
            to S and sets the associated variables to 1. 
	Example
	    V = {{0, 0}, {0, 1}, {1, 1}, {1, 0}};
	    X = symbolicSlackMatrix V
	    (Y, F) = setOnesForest X
    SeeAlso
    	graphFromSlackMatrix
	slackIdeal
	symbolicSlackMatrix
///

doc ///
    Key
    	rehomogenizePolynomial
	(rehomogenizePolynomial, Matrix)
	(rehomogenizePolynomial, Matrix, Matrix, Graph, RingElement)
    Headline
    	rehomogenization of a polynomial reversing the dehomogenization of the slack matrix
    Usage
    	h = rehomogenizePolynomial X
	h = rehomogenizePolynomial(X, Y, T, g)
    Inputs
    	X:Matrix
	    a symbolic slack matrix X 
	Y:Matrix
	    dehomogenized slack matrix Y
	T:Graph
	    a spanning forest of the bipartite graph associated to X 
	g:RingElement
	    a polynomial
    Outputs
    	h:RingElement
	    the rehomogenization of g reversing the dehomogenization of the slack matrix
    Description
    	Text
	    Given the symbolic slack matrix X of a polytope, we may set to 1 the variables corresponding
	    of a spanning forest of the bipartite graph associated with X. It can be useful to rehomogenize 
	    a polynomial in the variables that are left, reversing the dehomogenization process.

            This function produces the rehomogenization of a polynomial following the spanning forest 
	    backwards. It is useful in the computation of the rehomogenization of the generators of an 
	    ideal, for example of the slack ideal.
	Example
	    R = QQ[x_1..x_12];
	    X = matrix {{0, x_1, 0, 0, x_2}, {x_3, 0, 0, 0, x_4}, {0, x_5, x_6, 0, 0}, {x_7, 0, x_8, 0, 0}, {0, x_9, 0, x_10, 0}, {x_11, 0, 0, x_12, 0}};
	    (Y, T) = setOnesForest X;
	    remVars := flatten entries Y - set{0_(ring Y), 1_(ring Y)};
	    h = rehomogenizePolynomial(X, Y, T, remVars_0^2+remVars_0*remVars_1-1)
    SeeAlso
    	setOnesForest
	slackIdeal
	symbolicSlackMatrix
///

doc ///
    Key
    	rehomogenizeIdeal
	(rehomogenizeIdeal, ZZ, Matrix)
	(rehomogenizeIdeal, ZZ, Matrix, Graph)
    Headline
    	rehomogenization of a the dehomogenized slack ideal
    Usage
    	H = rehomogenizeIdeal(d, X)
	H = rehomogenizeIdeal(d, Y, T)
    Inputs
    	d:ZZ
	    dimension of the polytope
	X:Matrix
	    a symbolic slack matrix X of a d-polytope
	Y:Matrix
	    dehomogenized symbolic slack matrix Y of d-polytope
	T:Graph
	    spanning forest whose edges match the ones in Y
    Outputs
    	H:Ideal
	    the rehomogenization of the dehomogenized slack ideal
    Description
    	Text
	    It computes the rehomogenization of the dehomogenized slack ideal, applying the 
	    rehomogenize function to its generators.
	Example
	    V = {{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}, {0, 0, 1}, {0, 1, 1}};
	    X = symbolicSlackMatrix V
	    H = rehomogenizeIdeal(3, X)
	Example
	    R = QQ[x_0..x_11];
	    Y = matrix {{0, 0, 1, 0, 1}, {1, 0, 1, 0, 0}, {0, x_4, 0, 0, 1}, {1, 1, 0, 0, 0}, {0, 0, 1, 1, 0}, {0, x_10, 0, 1, 0}};
	    T = graph(QQ[y_0, y_1, y_2, y_3, y_4, y_5, y_6, y_7, y_8, y_9, y_10], {{y_1, y_6}, {y_3, y_6}, {y_3, y_7}, {y_0, y_8}, {y_1, y_8}, {y_4, y_8}, {y_4, y_9}, {y_5, y_9}, {y_0, y_10}, {y_2, y_10}});
	    rehomogenizeIdeal(3, Y, T)
    SeeAlso
    	rehomogenizePolynomial
	setOnesForest
	slackIdeal
	symbolicSlackMatrix
///

doc ///
    Key
    	findFlag
	(findFlag, Polyhedron)
	(findFlag, Matroid)
	(findFlag, Matrix)
	(findFlag, List)
    Headline
    	computes a list of facet labels that make up a flag in a polytope
    Usage
    	F = findFlag P
	F = findFlag M
	F = findFlag S
	F = findFlag V
    Inputs
    	P:Polyhedron
	    a polytope 
	M:Matroid
	    a matroid
	S:Matrix
	    (symbolic) slack matrix
	V:List
	    a list of vertex coordinates, matroid vectors, facet labels, or cone generators 
    Outputs
    	F:List
	    a list of facet labels or hyperplanes or column labels or labels that make up a flag in the given polytope or matroid or matrix or object given by V
    Description
    	Text
	    Given a polytope, a matroid, a cone, it finds a flag of facets or hyperplanes or cone generators that can be given as input to obtain a reduced slack matrix.
	Example
	    V = {{0, 0, 0}, {0, 0, 1}, {1, 0, 0}, {1, 0, 1}, {0, 1, 0}, {0, 1, 1}};
	    findFlag V
	    findFlag(V, Object => "matroid")
	    findFlag({{0, 2, 3, 5}, {0, 1, 3, 4}, {1, 2, 4, 5}, {0, 1, 2}, {3, 4, 5}}, Object => "abstractPolytope")
    SeeAlso
    	reconstructSlackMatrix
	reducedSlackMatrix
///

doc ///
    Key
    	containsFlag
	(containsFlag, List, Polyhedron)
	(containsFlag, List, Matroid)
	(containsFlag, List, Matrix)
	(containsFlag, List, List)
    Headline
    	establishes whether or not a list of facet labels contains a flag in a polytope or matroid
    Usage
    	containsFlag(fl, P)
	containsFlag(fl, M)
	containsFlag(fl, S)
	containsFlag(fl, V)
    Inputs
    	fl:List
	    a list of facet labels 
	P:Polyhedron
	    a polytope 
	M:Matroid
	    a matroid
	S:Matrix
	    (symbolic) slack matrix
	V:List
	    a list of vertex coordinates, matroid vectors, facet labels, or cone generators 
    Outputs
    	:Boolean
	    whether or not a list of facet labels contains a flag in a polytope or matroid
    Description
    	Text
	    Given a list of facet labels fl and a polytope, a matroid, a (symbolic) slack matrix or a list of vertex coordinates, matroid vectors, facet labels, establishes whether or not fl contains a flag in the given object.
	Example
	    V = {{0, 0, 0}, {0, 0, 1}, {1, 0, 0}, {1, 0, 1}, {0, 1, 0}, {0, 1, 1}};
	    containsFlag({0, 1, 2, 4}, V)
	    containsFlag({0, 1, 2, 8}, V, Object => "matroid")
	    containsFlag({0, 1, 2, 3}, {{0, 2, 3, 5}, {0, 1, 3, 4}, {1, 2, 4, 5}, {0, 1, 2}, {3, 4, 5}}, Object => "abstractPolytope")
    SeeAlso
    	findFlag
	reducedSlackMatrix
///

doc ///
    Key
    	reconstructSlackMatrix
	(reconstructSlackMatrix, Matrix, List, List)
	(reconstructSlackMatrix, Matrix, List)
    Headline
    	a list of facet labels that make up a flag in a polytope
    Usage
    	ExtS = reconstructSlackMatrix(S, fl, F)
	ExtS = reconstructSlackMatrix(S, F)
    Inputs
    	S:Matrix
	    a (column) reduced slack matrix 
	fl:List
	    a flag of columns
	F:List
	    list of facet columns we want to refill 
    Outputs
    	ExtS:Matrix
	    S extended by the columns of F
    Description
    	Text
	    Given a (column) reduced slack matrix S, a flag of columns and a list of facet columns F to refill, it extends S by the columns of F.
	Example
	    V = {{0,0,0},{0,0,1},{1,0,0},{1,0,1},{0,1,0},{0,1,1}};
	    S = slackMatrix V;
	    redS = reducedSlackMatrix(3, S);
	    reconstructSlackMatrix(reducedSlackMatrix(S, Vars => {a,b,c,d,e,f,g,h,j,k,m,p}), {{0,1,2}})
	    reconstructSlackMatrix(redS, {{0,1,2}})
	    reconstructSlackMatrix(redS, {{0,1,2}}, Vars => {x})
    SeeAlso
    	findFlag
	reducedSlackMatrix
///

doc ///
    Key
    	reducedSlackMatrix
	(reducedSlackMatrix, ZZ, Matrix)
	(reducedSlackMatrix, Matrix)
	(reducedSlackMatrix, List)
    Headline
    	a reduced slack matrix of a polytope
    Usage
    	RedS = reducedSlackMatrix(d, S)
	RedS = reducedSlackMatrix S
	RedS = reducedSlackMatrix V
    Inputs
    	d:ZZ
	    the dimension of the polytope or 1 less than the rank of the matroid
	S:Matrix
	    a (symbolic) slack matrix of a polytope or of a matroid
	V:List
	    list of vertex coordinates, matroid vectors, facet labels, or cone generators
    Outputs
    	RedS:Matrix
	    a reduced slack matrix
    Description
    	Text
	    Given a (symbolic) slack matrix of a polytope or of a matroid, or a list of vertex coordinates, matroid vectors, facet labels, or cone generators, it reduces it by removing the columns corresponding to simplicial facets outside of a flag.
	Example
	    -- Let P be a triangular prism
	    V = {{0,0,0},{0,0,1},{1,0,0},{1,0,1},{0,1,0},{0,1,1}};
	    S = slackMatrix V;
	    reducedSlackMatrix V
	    reducedSlackMatrix(3, S)
	    -- Let P be a 3-polytope with 8 vertices and facets {1357, 1458, 2367, 2468, 5678, 134, 234}
	    R = QQ[x_1..x_30];
	    S = matrix {{0, 0, x_1, x_2, x_3, 0, x_4}, {x_5, x_6, 0, 0, x_7, x_8, 0}, {0, x_9, 0, x_10, x_11, 0, 0}, {x_12, 0, x_13, 0, x_14, 0, 0}, {0, 0, x_15, x_16, 0, x_17, x_18}, {x_19, x_20, 0, 0, 0, x_21, x_22}, {0, x_23, 0, x_24, 0, x_25, x_26}, {x_27, 0, x_28, 0, 0, x_29, x_30}};
	    redS = reducedSlackMatrix(3, S)
    SeeAlso
    	findFlag
	reconstructSlackMatrix
///

doc ///
    Key
    	Object
    Headline
    	select the combinatorial object which the input should be interpreted as
    Usage
    	slackMatrix(V, Object => "matroid")
	slackIdeal(V, Object => "abstractPolytope")
	symbolicSlackMatrix(V, Object => "polytope")
	cycleIdeal(V, Object => "matroid")
	graphicIdeal(V, Object => "abstractPolytope")
	getFacetBases(V, Object => "cone")
	slackFromPlucker(V, Object => "polytope")
	symbolicSlackOfPlucker(V, Object => "matroid")
	grassmannSectionIdeal(V, Object => "cone")
	findFlag(V, Object => "matroid")
	containsFlag(fl, V, Object => "matroid")
	reducedSlackMatrix(V, Object => "polytope")
    Description
    	Text
	    The default value is polytope.  	
    SeeAlso
    	slackMatrix
	slackIdeal
	symbolicSlackMatrix
	cycleIdeal
	graphicIdeal
	getFacetBases
	slackFromPlucker
	symbolicSlackOfPlucker
	grassmannSectionIdeal
	findFlag
	containsFlag
	reducedSlackMatrix
///

doc ///
	Key
		[slackMatrix, Object]
	Headline
		specify combinatorial object 
	Usage
		slackMatrix(..., Object => "polytope")
		slackMatrix(..., Object => "matroid")
	Description
		Text
		    Indicates whether an input list should be treated as the vertices of a polytope
		    or the ground set of a matroid. 
///

doc ///
	Key
		[graphicIdeal, Object]
	Headline
		specify combinatorial object 
	Usage
		slackMatrix(..., Object => "polytope")
		slackMatrix(..., Object => "abstractPolytope")
	Description
		Text
		    Indicates whether an input list should be treated as the vertices of a polytope
		    or the facets of an abstract polytope. 
///

doc ///
	Key
		[slackIdeal, Object]
	Headline
		specify combinatorial object 
	Usage
		slackIdeal(..., Object => "polytope")
		slackIdeal(..., Object => "matroid")
		slackIdeal(..., Object => "abstractPolytope")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid or the facets of an abstract polytope. 

///

doc ///
	Key
		[symbolicSlackMatrix, Object]
	Headline
		specify combinatorial object 
	Usage
		symbolicSlackMatrix(..., Object => "polytope")
		symbolicSlackMatrix(..., Object => "matroid")
		symbolicSlackMatrix(..., Object => "abstractPolytope")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid or the facets of an abstract polytope.
///

doc ///
	Key
		[symbolicSlackOfPlucker, Object]
	Headline
		specify combinatorial object 
	Usage
		symbolicSlackOfPlucker(..., Object => "polytope")
		symbolicSlackOfPlucker(..., Object => "cone")
		symbolicSlackOfPlucker(..., Object => "matroid")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid or the facets of an abstract polytope.
///

doc ///
	Key
		[grassmannSectionIdeal, Object]
	Headline
		specify combinatorial object 
	Usage
		grassmannSectionIdeal(..., Object => "cone")
		grassmannSectionIdeal(..., Object => "matroid")
		grassmannSectionIdeal(..., Object => "polytope")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid or the facets of an abstract polytope.
///

doc ///
	Key
		[cycleIdeal, Object]
	Headline
		specify combinatorial object 
	Usage
		cycleIdeal(..., Object => "polytope")
		cycleIdeal(..., Object => "matroid")
		cycleIdeal(..., Object => "abstractPolytope")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid or the facets of an abstract polytope.
///

doc ///
	Key
		[getFacetBases, Object]
	Headline
		specify combinatorial object 
	Usage
		getFacetBases(..., Object => "polytope")
		getFacetBases(..., Object => "cone")
		getFacetBases(..., Object => "matroid")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid or the facets of an abstract polytope.
///

doc ///
	Key
		[slackFromPlucker, Object]
	Headline
		specify combinatorial object 
	Usage
		slackFromPlucker(..., Object => "cone")
		slackFromPlucker(..., Object => "abstractPolytope")
		slackFromPlucker(..., Object => "polytope")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid or the facets of an abstract polytope.
///

doc ///
	Key
		[findFlag, Object]
	Headline
		specify combinatorial object 
	Usage
		findFlag(..., Object => "polytope")
		findFlag(..., Object => "matroid")
		findFlag(..., Object => "abstractPolytope")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid or the facets of an abstract polytope.
///

doc ///
	Key
		[containsFlag, Object]
	Headline
		specify combinatorial object 
	Usage
		containsFlag(..., Object => "polytope")
		containsFlag(..., Object => "matroid")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid or the facets of an abstract polytope.
///

doc ///
	Key
		[reducedSlackMatrix, Object]
	Headline
		specify combinatorial object 
	Usage
		reducedSlackMatrix(..., Object => "polytope")
		reducedSlackMatrix(..., Object => "matroid")
	Description
		Text
 		    Indicates whether an input list should be treated as the vertices of a polytope,
		    the ground set of a matroid.
///

doc ///
	Key
		[slackIdeal, Strategy]
	Headline
		specifies saturation strategy to be used 
	Usage
		slackIdeal(..., Strategy => Iterate)
		slackIdeal(..., Strategy => Bayer)
		slackIdeal(..., Strategy => Linear)
		slackIdeal(..., Strategy => Eliminate)
	SeeAlso
	    Strategy
///

doc ///
	Key
		[grassmannSectionIdeal, Strategy]
	Headline
		specifies saturation strategy to be used 
	Usage
		grassmannSectionIdeal(..., Strategy => Iterate)
		grassmannSectionIdeal(..., Strategy => Bayer)
		grassmannSectionIdeal(..., Strategy => Linear)
		grassmannSectionIdeal(..., Strategy => Eliminate)
	SeeAlso
	    Strategy
///

doc ///
	Key
		[cycleIdeal, Strategy]
	Headline
		specifies saturation strategy to be used 
	Usage
		cycleIdeal(..., Strategy => Iterate)
		cycleIdeal(..., Strategy => Bayer)
		cycleIdeal(..., Strategy => Linear)
		cycleIdeal(..., Strategy => Eliminate)
	SeeAlso
	    Strategy
///

doc ///
	Key
		[graphicIdeal, Strategy]
	Headline
		specifies saturation strategy to be used 
	Usage
		cycleIdeal(..., Strategy => Iterate)
		cycleIdeal(..., Strategy => Bayer)
		cycleIdeal(..., Strategy => Linear)
		cycleIdeal(..., Strategy => Eliminate)
	SeeAlso
	    Strategy
///

doc ///
	Key
		[rehomogenizeIdeal, Strategy]
	Headline
		specifies saturation strategy to be used 
	Usage
		rehomogenizeIdeal(..., Strategy => Iterate)
		rehomogenizeIdeal(..., Strategy => Bayer)
		rehomogenizeIdeal(..., Strategy => Linear)
		rehomogenizeIdeal(..., Strategy => Eliminate)
	SeeAlso
	    Strategy
///

doc ///
    Key
    	Saturate
    Headline
    	choose whether to saturate with respect to the product of all variables at the same time or variable by variable.
    Usage
    	slackIdeal(V, Saturate => "all")
	rehomogenizeIdeal(d, X, Saturate => "each")
	grassmannSectionIdeal(S, B, Saturate => "all")
	graphicIdeal(V, Saturate => "each")
	cycleIdeal(S, Saturate => "all")
    Description
    	Text
	    The default value is "all". 
    SeeAlso
    	Strategy
	slackIdeal
	rehomogenizeIdeal
	grassmannSectionIdeal
	graphicIdeal
	cycleIdeal
///

doc ///
	Key
		[slackIdeal, Saturate]
	Headline
		specifies saturation strategy to be used 
	Usage
		slackIdeal(..., Saturate => "all")
		slackIdeal(..., Saturate => "each")
	SeeAlso
	    Strategy
///

doc ///
	Key
		[rehomogenizeIdeal, Saturate]
	Headline
		specifies saturation strategy to be used 
	Usage
		rehomogenizeIdeal(..., Saturate => "all")
		rehomogenizeIdeal(..., Saturate => "each")
	SeeAlso
	    Strategy
///

doc ///
	Key
		[grassmannSectionIdeal, Saturate]
	Headline
		specifies saturation strategy to be used 
	Usage
		grassmannSectionIdeal(..., Saturate => "all")
		grassmannSectionIdeal(..., Saturate => "each")
	SeeAlso
	    Strategy
///

doc ///
	Key
		[graphicIdeal, Saturate]
	Headline
		specifies saturation strategy to be used 
	Usage
		graphicIdeal(..., Saturate => "all")
		graphicIdeal(..., Saturate => "each")
	SeeAlso
	    Strategy
///

doc ///
	Key
		[cycleIdeal, Saturate]
	Headline
		specifies saturation strategy to be used 
	Usage
		cycleIdeal(..., Saturate => "all")
		cycleIdeal(..., Saturate => "each")
	SeeAlso
	    Strategy
///

doc ///
	Key
		[symbolicSlackMatrix, CoefficientRing]
	Headline
		specifies the coefficient ring of the underlying ring of the matrix
	Usage
		symbolicSlackMatrix(..., CoefficientRing => QQ)
		symbolicSlackMatrix(..., CoefficientRing => ZZ/7)
		symbolicSlackMatrix(..., CoefficientRing => GF(9))
	SeeAlso
	    coefficientRing
///

doc ///
	Key
		[symbolicSlackOfPlucker, CoefficientRing]
	Headline
		specifies the coefficient ring of the underlying ring of the matrix
	Usage
		symbolicSlackOfPlucker(..., CoefficientRing => QQ)
		symbolicSlackOfPlucker(..., CoefficientRing => ZZ/7)
		symbolicSlackOfPlucker(..., CoefficientRing => GF(9))
	SeeAlso
	    coefficientRing
///

doc ///
	Key
		[slackIdeal, CoefficientRing]
	Headline
		specifies the coefficient ring of the underlying ring of the ideal
	Usage
		slackIdeal(..., CoefficientRing => QQ)
		slackIdeal(..., CoefficientRing => ZZ/7)
		slackIdeal(..., CoefficientRing => GF(9))
	SeeAlso
	    coefficientRing
///

doc ///
	Key
		[grassmannSectionIdeal, CoefficientRing]
	Headline
		specifies the coefficient ring of the underlying ring of the ideal
	Usage
		grassmannSectionIdeal(..., CoefficientRing => QQ)
		grassmannSectionIdeal(..., CoefficientRing => ZZ/7)
		grassmannSectionIdeal(..., CoefficientRing => GF(9))
	SeeAlso
	    coefficientRing
///

doc ///
	Key
		[cycleIdeal, CoefficientRing]
	Headline
		specifies the coefficient ring of the underlying ring of the ideal
	Usage
		cycleIdeal(..., CoefficientRing => QQ)
		cycleIdeal(..., CoefficientRing => ZZ/7)
		cycleIdeal(..., CoefficientRing => GF(9))
	SeeAlso
	    coefficientRing
///

doc ///
	Key
		[graphicIdeal, CoefficientRing]
	Headline
		specifies the coefficient ring of the underlying ring of the ideal
	Usage
		graphicIdeal(..., CoefficientRing => QQ)
		graphicIdeal(..., CoefficientRing => ZZ/7)
		graphicIdeal(..., CoefficientRing => GF(9))
	SeeAlso
	    coefficientRing
///

doc ///
	Key
		[universalIdeal, CoefficientRing]
	Headline
		specifies the coefficient ring of the underlying ring of the ideal
	Usage
		universalIdeal(..., CoefficientRing => QQ)
		universalIdeal(..., CoefficientRing => ZZ/7)
		universalIdeal(..., CoefficientRing => GF(9))
	SeeAlso
	    coefficientRing
///

doc ///
	Key
		[reconstructSlackMatrix, CoefficientRing]
	Headline
		specifies the coefficient ring of the underlying ring of the ideal
	Usage
		reconstructSlackMatrix(..., CoefficientRing => QQ)
		reconstructSlackMatrix(..., CoefficientRing => ZZ/7)
		reconstructSlackMatrix(..., CoefficientRing => GF(9))
	SeeAlso
	    coefficientRing
///

doc ///
	Key
		[reducedSlackMatrix, CoefficientRing]
	Headline
		specifies the coefficient ring of the underlying ring of the ideal
	Usage
		reducedSlackMatrix(..., CoefficientRing => QQ)
		reducedSlackMatrix(..., CoefficientRing => ZZ/7)
		reducedSlackMatrix(..., CoefficientRing => GF(9))
	SeeAlso
	    coefficientRing
///

doc ///
    Key
    	Vars
    Headline
    	give a set of variables for the polynomial ring where the object created will live
    Usage
    	symbolicSlackMatrix(S, Vars => {x_1..x_10})
    	slackIdeal(V, Vars => {a..z})
	cycleIdeal(M, Vars => {A, B, S, T})
	graphicIdeal(P, Vars => {y_(1, 1)..y_(5, 4)})
	universalIdeal(M, Vars => vars{3, 5, 7})
	reconstructSlackMatrix(S, {{0,1,2}}, Vars => {x})
	reducedSlackMatrix(S, Vars => {a,b,c,d,e,f,g,h,j,k,m,p})
    Description
    	Text
	    This option is useful if you want to compare the object you will create with one in an existing ring. The default value is @{TT "{}"}@ which means the method will create the necessary variables (in a new ring) for the constructed object. 
    SeeAlso
    	symbolicSlackMatrix
    	slackIdeal
	cycleIdeal
	graphicIdeal
	universalIdeal
	reconstructSlackMatrix
	reducedSlackMatrix
	PolynomialRing
	vars	
///

doc ///
	Key
		[symbolicSlackMatrix, Vars]
	Headline
		specifies the variables to use to create the underlying ring of the matrix
	Usage
		symbolicSlackMatrix(..., Vars => {x_1..x_10})
		symbolicSlackMatrix(..., Vars => {a..z})
		symbolicSlackMatrix(..., Vars => {y_(1, 1)..y_(3, 4)})
	SeeAlso
	    PolynomialRing
	    vars
///

doc ///
	Key
		[slackIdeal, Vars]
	Headline
		specifies the variables to use to create the underlying ring of the ideal
	Usage
		slackIdeal(..., Vars => {a..r})
		slackIdeal(..., Vars => {y_1..y_6})
		slackIdeal(..., Vars => {A, B, S, T})
	SeeAlso
	    PolynomialRing
	    vars
///

doc ///
	Key
		[cycleIdeal, Vars]
	Headline
		specifies the variables to use to create the underlying ring of the ideal
	Usage
		cycleIdeal(..., Vars => vars(13..54))
		cycleIdeal(..., Vars => {x_(1, 1)..x_(4, 5)})
		cycleIdeal(..., Vars => {y_3..y_9})
	SeeAlso
	    PolynomialRing
	    vars
///

doc ///
	Key
		[graphicIdeal, Vars]
	Headline
		specifies the variables to use to create the underlying ring of the ideal
	Usage
		graphicIdeal(..., Vars => {x_0..x_19})
		graphicIdeal(..., Vars => {y_(1, 1)..y_(3, 2)})
		graphicIdeal(..., Vars => {a..k})
	SeeAlso
	    PolynomialRing
	    vars
///

doc ///
	Key
		[universalIdeal, Vars]
	Headline
		specifies the variables to use to create the underlying ring of the ideal
	Usage
		universalIdeal(..., Vars => {P, Q, R, S})
		universalIdeal(..., Vars => {a_0..a_12})
		universalIdeal(..., Vars => vars(-10..-3))
	SeeAlso
	    PolynomialRing
	    vars
///

doc ///
	Key
		[reconstructSlackMatrix, Vars]
	Headline
		specifies the variables to use to create the underlying ring of the ideal
	Usage
		reconstructSlackMatrix(..., Vars => {x})
		reconstructSlackMatrix(..., Vars => {a,b,c,d,e,f,g,h,j})
	SeeAlso
	    PolynomialRing
	    vars
///

doc ///
	Key
		[reducedSlackMatrix, Vars]
	Headline
		specifies the variables to use to create the underlying ring of the ideal
	Usage
		reducedSlackMatrix(..., Vars => {x})
		reducedSlackMatrix(..., Vars => {a,b,c,d,e,f,g,h,j,k,m,p})
	SeeAlso
	    PolynomialRing
	    vars
///


doc ///
    Key
        Tolerance
    Headline
        choose the tolerance to approximate computations over the field RR
    Usage
        slackFromGaleCircuits(G, Tolerance => 12)
    Description
        Text
	    The default value is 14. 
    SeeAlso
    	Strategy
    	slackFromGaleCircuits       
///

doc ///
	Key
		[slackFromGaleCircuits, Tolerance]
	Headline
		specifies the tolerance to compute the slack matrix of a polytope from a Gale transform of a polytope 
	Usage
		slackFromGaleCircuits(..., Tolerance => 12)
	SeeAlso
		Strategy
///


doc ///
    Key
        FlagElement
    Headline
        a facet label that will be contained in a flag of facets of given polytope or matroid
    Usage
        findFlag(V, FlagElement => 3)
    Description
        Text
	    The default value is 0. 
    SeeAlso
    	Object
    	findFlag
///

doc ///
	Key
		[findFlag, FlagElement]
	Headline
		a facet label that will be contained in a flag of facets of given polytope or matroid
	Usage
		findFlag(..., FlagElement => 3)
	SeeAlso
		Object
///


doc ///
    Key
        FlagIndices
    Headline
        a list of facet labels that form a flag of facets of given polytope or matroid
    Usage
        containsFlag(V, FlagIndices => {0, 1, 2, 3})
    Description
        Text
	    The default value is 0. 
    SeeAlso
    	Object
    	findFlag
///

doc ///
	Key
		[reducedSlackMatrix, FlagIndices]
	Headline
		a list of facet labels that form a flag of facets of given polytope or matroid
	Usage
		reducedSlackMatrix(..., FlagIndices => {0, 1, 2, 3})
	SeeAlso
		Object
///

------ TESTS ------

TEST ///
V = {{0, 0}, {0, 1}, {1, 1}, {1, 0}}
assert(slackMatrix V == matrix(QQ, {{0, 1, 0, 1}, {1, 0, 0, 1}, {0, 1, 1, 0}, {1, 0, 1, 0}}))
assert(slackMatrix(V, Object => "matroid") == matrix {{-1, -1, 0, -1, 0, 0}, {-1, 0, 1, 0, 1, 0}, {0, 1, 1, 0, 0, -1}, {0, 0, 0, -1, -1, -1}})
C = posHull(matrix{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}})
assert(slackMatrix C == matrix {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}})
///

TEST ///
V = {{0, 0}, {1, 0}, {2, 1}, {1, 2}, {0, 1}}
assert(getFacetBases V == ({{0, 0}, {1, 0}, {0, 1}, {2, 1}, {1, 2}}, {{0, 2}, {0, 1}, {1, 3}, {2, 4}, {3, 4}}))
///

TEST ///
V = {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 0, 1}, {1, 1, 0}}
B = {{1, 2, 4}, {0, 2, 3}, {0, 1, 4}, {3, 4, 5}, {0, 1, 2}}
assert(slackFromPlucker(V, B) == matrix {{-1, 0, 0, -1, 0}, {0, 1, 0, -1, 0}, {0, 0, -1, 0, 0}, {-1, 0, 0, 0, 1}, {0, 1, 0, 0, 1}, {1, 1, -1, 0, 0}})
///

TEST ///
MG = matrix(RR, {{0, 1, -1, 0, -1, 1}, {1, 0, -1, -1, 0, 1}})
B = {{1, 2, 4}, {0, 2, 3}, {0, 1, 4}, {3, 4, 5}, {0, 1, 2}}
assert(slackFromGalePlucker(B, MG) == matrix(RR, {{1, 0, 0, 1, 0}, {0, -1, 0, 1, 0}, {0, 0, 1, 1, 0}, {1, 0, 0, 0, -1}, {0, -1, 0, 0, -1}, {0, 0, 1, 0, -1}}))
///

TEST ///
G = matrix(RR, {{0, 1, -1, 0, -1, 1}, {1, 0, -1, -1, 0, 1}})
assert(slackFromGaleCircuits G == matrix(RR, {{1, 0, 0, 1, 0}, {0, 1, 0, 1, 0}, {0, 0, 1, 1, 0}, {1, 0, 0, 0, 1}, {0, 1, 0, 0, 1}, {0, 0, 1, 0, 1}}))
///

TEST ///
assert(specificSlackMatrix "barnette" == matrix(RR, {{1, 0, 0, 0, 1, 1, 0}, {1, 0, 0, 0, 0, 1, 1}, {1, 0, 0, 1, 0, 0, 1}, {1, 0, 0, 1, 1, 0, 0}, {1, 0, 1, 0, 0, 0, 0}, {0, 1, 0, 0, 1, 1, 0}, {0, 1, 0, 0, 0, 1, 1}, {0, 1, 0, 1, 0, 0, 1}, {0, 1, 0, 1, 1, 0, 0}, {0, 1, 1, 0, 0, 0, 0}}))
///

TEST ///
V = {{0, 0}, {0, 1}, {1, 1}, {1, 0}}
R = QQ[x_0..x_11]
assert(substitute(symbolicSlackMatrix V, R) == matrix {{0, x_0, 0, x_1}, {x_2, 0, 0, x_3}, {0, x_4, x_5, 0}, {x_6, 0, x_7, 0}})
assert(substitute(symbolicSlackMatrix(V, Object => "matroid"), R) == matrix {{x_0, x_1, 0, x_2, 0, 0}, {x_3, 0, x_4, 0, x_5, 0}, {0, x_6, x_7, 0, 0, x_8}, {0, 0, 0, x_9, x_10, x_11}})
V = {{1, 2, 3}, {4, 5, 6}, {1, 2, 4, 5}, {1, 3, 4, 6}, {2, 3, 5, 6}}
assert(substitute(symbolicSlackMatrix(V, Object => "abstractPolytope"), R) == matrix {{0, x_0, 0, 0, x_1}, {0, x_2, 0, x_3, 0}, {0, x_4, x_5, 0, 0}, {x_6, 0, 0, 0, x_7}, {x_8, 0, 0, x_9, 0}, {x_10, 0, x_11, 0, 0}})
///

TEST ///
V = {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {1, 0, 1}, {1, 1, 0}}
B = {{1, 2, 4}, {0, 2, 3}, {0, 1, 4}, {3, 4, 5}, {0, 1, 2}}
assert(symbolicSlackOfPlucker(V, B) == matrix {{-p_(0,1,2,4), 0, 0, -p_(0,3,4,5), 0}, {0, p_(0,1,2,3), 0, -p_(1,3,4,5), 0}, {0, 0, -p_(0,1,2,4), -p_(2,3,4,5), 0}, {-p_(1,2,3,4), 0, -p_(0,1,3,4), 0, p_(0,1,2,3)}, {0, p_(0,2,3,4), 0, 0, p_(0,1,2,4)}, {p_(1,2,4,5), p_(0,2,3,5), p_(0,1,4,5), 0, p_(0,1,2,5)}})
///

TEST ///
V = {{0, 0}, {1, 0}, {1, 1}, {0, 1}}
R = QQ[x_0..x_11]
assert(substitute(slackIdeal V, R) == ideal(x_0*x_3*x_5*x_6-x_1*x_2*x_4*x_7))
assert(substitute(slackIdeal(V, Object => "matroid"), R) == ideal(x_4*x_8*x_10+x_5*x_7*x_11, x_1*x_8*x_9+x_2*x_6*x_11, x_0*x_5*x_9+x_2*x_3*x_10, x_0*x_4*x_6+x_1*x_3*x_7, x_1*x_3*x_8*x_10-x_0*x_5*x_6*x_11, x_0*x_4*x_8*x_9-x_2*x_3*x_7*x_11, x_1*x_5*x_7*x_9-x_2*x_4*x_6*x_10))
///

TEST ///
V = {{0, 0}, {1, 0}, {2, 1}, {1, 2}, {0, 1}}
(VV, B) = getFacetBases V
R = QQ[p_(0,1,2), p_(0,1,3), p_(0,2,3), p_(1,2,3), p_(0,1,4), p_(0,2,4), p_(1,2,4), p_(0,3,4), p_(1,3,4), p_(2,3,4)]
assert(substitute(grassmannSectionIdeal(VV, B), R) == substitute(ideal(p_(1,2,4)*p_(0,3,4)-p_(0,2,4)*p_(1,3,4)+p_(0,1,4)*p_(2,3,4), p_(1,2,3)*p_(0,3,4)-p_(0,2,3)*p_(1,3,4)+p_(0,1,3)*p_(2,3,4), p_(1,2,3)*p_(0,2,4)-p_(0,2,3)*p_(1,2,4)+p_(0,1,2)*p_(2,3,4), p_(1,2,3)*p_(0,1,4)-p_(0,1,3)*p_(1,2,4)+p_(0,1,2)*p_(1,3,4), p_(0,2,3)*p_(0,1,4)-p_(0,1,3)*p_(0,2,4)+p_(0,1,2)*p_(0,3,4)), R))
///

TEST ///
V = {{0, 0}, {1, 0}, {1, 1}, {0, 1}}
assert(graphFromSlackMatrix(symbolicSlackMatrix V) == matrix {{1, 1, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 1, 0, 0, 0, 0}, {0, 0, 0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 1, 1}, {0, 0, 1, 0, 0, 0, 1, 0}, {1, 0, 0, 0, 1, 0, 0, 0}, {0, 0, 0, 0, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 0, 0, 0}})
///

TEST ///
P = convexHull(matrix{{0, 0, 1, 1}, {0, 1, 0, 1}})
R = QQ[y_0..y_7]
assert(substitute(graphicIdeal P, R) == ideal(y_0*y_3*y_5*y_6-y_1*y_2*y_4*y_7))
///

TEST ///
V = {{0, 0}, {1, 0}, {1, 1}, {0, 1}}
R = QQ[y_0..y_11]
assert(substitute(cycleIdeal V, R) == ideal(y_0*y_3*y_5*y_6-y_1*y_2*y_4*y_7))
assert(substitute(cycleIdeal(V, Object => "matroid"), R) == ideal(y_4*y_8*y_10+y_5*y_7*y_11, y_1*y_8*y_9+y_2*y_6*y_11, y_0*y_5*y_9+y_2*y_3*y_10, y_0*y_4*y_6+y_1*y_3*y_7, y_1*y_3*y_8*y_10-y_0*y_5*y_6*y_11, y_0*y_4*y_8*y_9-y_2*y_3*y_7*y_11, y_1*y_5*y_7*y_9-y_2*y_4*y_6*y_10))
S = matrix{{1, 1, 0}, {0, 1, 2}, {1, 0, 3}}
assert(substitute(cycleIdeal S, R) == ideal(3*y_1*y_3*y_4-2*y_0*y_2*y_5))
///

TEST ///
V = {{0, 0}, {0, 1}, {1, 1}, {1, 0}}
assert(toricPolytope(slackIdeal V) == matrix {{1, 0, 0, 0, 1, 0, 0, 0}, {0, 1, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 1, 0, 0}, {0, 0, 0, 0, 1, 0, 1, 0}, {0, 0, 1, 0, -1, 0, 0, 0}, {0, 0, 0, 0, 0, 1, 0, 1}, {0, 0, 0, 1, 0, -1, 0, 0}})
///

TEST ///
R = QQ[x_1..x_12]
X = matrix {{0, x_1, 0, 0, x_2}, {x_3, 0, 0, 0, x_4}, {0, x_5, x_6, 0, 0}, {x_7, 0, x_8, 0, 0}, {0, x_9, 0, x_10, 0}, {x_11, 0, 0, x_12, 0}}
assert(rehomogenizePolynomial(X, matrix {{0, 1, 0, 0, 1}, {1, 0, 0, 0, x_4}, {0, 1, 1, 0, 0}, {1, 0, x_8, 0, 0}, {0, 1, 0, 1, 0}, {1, 0, 0, 1, 0}}, graph(QQ[y_1..y_11], {{y_2, y_7}, {y_4, y_7}, {y_6, y_7}, {y_1, y_8}, {y_3, y_8}, {y_5, y_8}, {y_3, y_9}, {y_5, y_10}, {y_6, y_10}, {y_1, y_11}}), x_4^2+x_4*x_8-1) == x_1^2*x_4^2*x_6*x_7*x_10^2*x_11^2 + x_1*x_2*x_3*x_4*x_5*x_8*x_10^2*x_11^2 - x_2^2*x_3^2*x_6*x_7*x_9^2*x_12^2)
///

TEST ///
V = {{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, {0, 1, 0}, {0, 0, 1}, {0, 1, 1}}
R = QQ[x_0..x_11]
assert(substitute(rehomogenizeIdeal(3, symbolicSlackMatrix V), R) == ideal(x_3*x_6*x_9*x_10-x_2*x_7*x_8*x_11, x_0*x_5*x_9*x_10-x_1*x_4*x_8*x_11, x_1*x_3*x_4*x_6-x_0*x_2*x_5*x_7))
Y = matrix {{0, 0, 1, 0, 1}, {1, 0, 1, 0, 0}, {0, x_4, 0, 0, 1}, {1, 1, 0, 0, 0}, {0, 0, 1, 1, 0}, {0, x_10, 0, 1, 0}}
T = graph(QQ[y_0, y_1, y_2, y_3, y_4, y_5, y_6, y_7, y_8, y_9, y_10], {{y_1, y_6}, {y_3, y_6}, {y_3, y_7}, {y_0, y_8}, {y_1, y_8}, {y_4, y_8}, {y_4, y_9}, {y_5, y_9}, {y_0, y_10}, {y_2, y_10}})
assert(rehomogenizeIdeal(3, Y, T) == ideal(x_3*x_6*x_9*x_10-x_2*x_7*x_8*x_11, x_0*x_5*x_9*x_10-x_1*x_4*x_8*x_11, x_1*x_3*x_4*x_6-x_0*x_2*x_5*x_7))
///

TEST ///
V = {{0,0,0},{0,0,1},{1,0,0},{1,0,1},{0,1,0},{0,1,1}}
S = slackMatrix V
assert(findFlag S == {0, 1, 2, 4})
assert(findFlag(V, Object => "matroid") == {0, 1, 5, 7})
assert(findFlag({{0, 2, 3, 5}, {0, 1, 3, 4}, {1, 2, 4, 5}, {0, 1, 2}, {3, 4, 5}}, Object => "abstractPolytope") == {0, 1, 2, 4})
///

TEST ///
V = {{0, 0, 0}, {0, 0, 1}, {1, 0, 0}, {1, 0, 1}, {0, 1, 0}, {0, 1, 1}};
assert(containsFlag({0, 1, 2, 4}, V) == true)
assert(containsFlag({0, 1, 2, 8}, V, Object => "matroid") == true)
assert(containsFlag({0, 1, 2, 8}, V, Object => "matroid") == true)
assert(containsFlag({0, 1, 2, 3}, {{0, 2, 3, 5}, {0, 1, 3, 4}, {1, 2, 4, 5}, {0, 1, 2}, {3, 4, 5}}, Object => "abstractPolytope") == true)
///

TEST ///
V = {{0,0,0},{0,0,1},{1,0,0},{1,0,1},{0,1,0},{0,1,1}}
S = slackMatrix V
redS = reducedSlackMatrix(S,Vars => {a,b,c,d,e,f,g,h,j,k,m,p})
R = ring redS
assert(reconstructSlackMatrix(redS, {{0,1,2}}) ==  substitute(matrix {{0, 0, a, b, 0}, {c, 0, 0, d, 0}, {0, e, 0, f, 0}, {0, 0, g, 0, -b*c*e*g}, {h, 0, 0, 0, -a*d*e*h}, {0, j, 0, 0, -a*c*f*j}}, R))
assert(reconstructSlackMatrix(reducedSlackMatrix(3, S), {{0,1,2}}) == matrix(QQ, {{0, 0, 1, 1, 0}, {1, 0, 0, 1, 0}, {0, 1, 0, 1, 0}, {0, 0, 1, 0, -1}, {1, 0, 0, 0, -1}, {0, 1, 0, 0, -1}}))
redS1 = reducedSlackMatrix(3, S)
extS1 = reconstructSlackMatrix(redS1, {{0,1,2}}, Vars => {x})
T = ring extS1
assert(extS1 == substitute(matrix {{0, 0, x_0, x_1, 0}, {x_2, 0, 0, x_3, 0}, {0, x_4, 0, x_5, 0}, {0, 0, x_6, 0, -x_1*x_2*x_4*x_6}, {x_7, 0, 0, 0, -x_0*x_3*x_4*x_7}, {0, x_8, 0, 0, -x_0*x_2*x_5*x_8}}, T))
///

TEST ///
V = {{0,0,0},{0,0,1},{1,0,0},{1,0,1},{0,1,0},{0,1,1}}
S = slackMatrix V
assert(reducedSlackMatrix V == matrix(QQ, {{0, 0, 1, 1}, {1, 0, 0, 1}, {0, 1, 0, 1}, {0, 0, 1, 0}, {1, 0, 0, 0}, {0, 1, 0, 0}}))
R = QQ[x_0..x_8]
assert(substitute(reducedSlackMatrix(S, Vars => {x}), R) == matrix {{0, 0, x_0, x_1}, {x_2, 0, 0, x_3}, {0, x_4, 0, x_5}, {0, 0, x_6, 0}, {x_7, 0, 0, 0}, {0, x_8, 0, 0}})
///

end

restart
loadPackage("SlackIdeals", Reload => true)
uninstallPackage "SlackIdeals"
installPackage "SlackIdeals"
installPackage("SlackIdeals", RemakeAllDocumentation => true)
viewHelp "SlackIdeals"
check "SlackIdeals"
