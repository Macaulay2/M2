--- -*- coding: utf-8 -*-
------------------------------------------------------------------------------
-- Copyright 2025 Sasha Zotine
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------------
newPackage(
    "ToricHigherDirectImages",
    Version => "1.0", 
    Date => "2025 April",
    Authors => {
	{Name     => "Sasha Zotine", 
	 Email    => "zotinea@mcmaster.ca",
	 HomePage => "https://sites.google.com/view/szotine/home" }
	},
    Headline => "computations involving pushforwards and higher direct images of toric maps",
    Keywords => {"Toric Geometry"},
    DebuggingMode => false,
    PackageExports => {"NormalToricVarieties","Complexes"},
    PackageImports => {"FourierMotzkin"}
    )

export {
    -- types
    -- methods
    "frobeniusDirectImage",
    "nefContraction",
    "nefRayContractions",
    "allNefContractions",
    "computeEigencharacters",
    "HDI"
    }

importFrom(NormalToricVarieties, {"outerNormals", "rawHHOO"})
importFrom(Core, {"sortBy"})

------------------------------------------------------------------------------
-- CODE
------------------------------------------------------------------------------

-- method for computing push forwards of Frobenius maps.
-- A Frobenius map is just given by multiplication (of the N lattice) by p.
-- INPUT: p - an integer
--        M - a module over a multigraded polynomial ring.
-- OUTPUT:  the module (F_p)_* M, i.e. the pushforward of M under the p-th toric
--          Frobenius map.
frobeniusDirectImage = method();
frobeniusDirectImage (ZZ, Module) := Module => (p, M) -> (
    if M == 0 then return M;
    -- If it isn't free
    if not isFreeModule M then coker frobeniusDirectImage_p presentation M else ( 
    	S := ring M;
    	X := variety S;
    	A := matrix rays X;
	-- It will split as a direct sum of line bundles, indexed by characters
	-- in M/pM. This is a primitive generating set of that, the unit hypercube.
    	pts := toList (toList (dim X:{0})..toList (dim X:{p-1}));
	-- Pick representative Cartier divisors for each summand.
    	D := (inverse fromCDivToPic X) * transpose matrix degrees M;
	-- This round-down formula computes the bundles appearing in the pushforward.
    	B := flatten apply(numcols D, d -> apply(pts, pt -> entries transpose (
	    	    fromCDivToPic X * matrix (entries (A * (matrix pt) - D_{d}) // p)
	    	    )
	    	)
	    );
    	dSum := directSum apply(B, b -> S^b);
	-- We should remember our choice of Cartier divisors.
    	dSum.cache.frobeniusDirectImage = D;
    	dSum
	)
    )

-- the direct image is functorial. this method computes the direct image
-- of the source and target of a map, and the corresponding morphism between them.
-- INPUT: p - an integer
--        f - a map between multigraded modules f : M -> N.
-- OUTPUT:  the map between modules (F_p)_* f : (F_p)_* M -> (F_p)_* N.
frobeniusDirectImage (ZZ, Matrix) := (p, f) -> (
    S := ring f;
    -- easy way to access the variety which these modules are over.
    X := variety S;
    -- these index the characters splitting the source and target.
    pts := toList (toList (dim X:{0})..toList (dim X:{p-1}));
    -- our map will be a block matrix
    blocksize := #pts;
    src := frobeniusDirectImage_p source f;
    tgt := frobeniusDirectImage_p target f;
    A := matrix rays X;
    -- this is the main reason to keep track of the divisors.
    -- we need them to make sure that the map is sending terms to the correct place.
    -- e.g. the trivial summand should map to the trivial summand.
    Dsrc := src.cache.frobeniusDirectImage;
    Dtgt := tgt.cache.frobeniusDirectImage;
    -- to make the direct image of the map
    matrix table(numrows f, numcols f, (r,c) -> (
	    -- we want to convert the entries of the input map
	    -- to block matrices.
	    f0 := f_(r,c);
	    blocksrc := directSum (components src)_{c*blocksize..(c+1)*blocksize-1};
	    blocktgt := directSum (components tgt)_{r*blocksize..(r+1)*blocksize-1};
	    if f0 == 0 then 0_S else (
		-- we'll multiply by the coefficient at the end.
	    	c0 := (coefficients f0)_1_0_0;
		-- the important data of the map comes from the exponents
		-- of the monomials defining f.
	    	d0 := transpose matrix exponents f0;
		M := matrix table(blocksize, blocksize, (mr, mc) -> (
			-- the exponents + Dsrc tell us which summand we should be mapping to.
		    	tgtindex := (entries solve(A, A * (matrix pts_mc) + d0 - Dsrc_{c} + Dtgt_{r})) % p;
			-- if there's no such summand, then the map is zero.
	    	    	if pts_mr != tgtindex then 0 else (
			    -- otherwise, we figure out what the degree of the source and target terms
			    -- are, and the monomial map between them is the difference of their degrees.
			    srcexp := matrix (entries (A * (matrix pts_mc) - Dsrc_{c}) // p);
		    	    tgtexp := matrix (entries (A * (matrix pts_mc) - Dsrc_{c} + d0) // p);
		    	    exponent := flatten entries (tgtexp - srcexp);
		    	    c0 * S_exponent
		    	    )
		    	)
		    );
		map(blocktgt,blocksrc,M)
		)
	    )
	)
    )

-- the direct image method can be applied to each map in a complex.
-- INPUT: p - an integer
--        K - a complex of multigraded modules.
-- OUTPUT:  the pushforward of the complex C, by applying the functor to each differential.
-- NOTE: since the Frobenius is a finite map, the pushforward is an exact functor, i.e.
--       there are no higher direct images.
frobeniusDirectImage (ZZ, Complex) := Complex => (p, K) -> (
    complex (
    	if length K == 0 then map(frobeniusDirectImage_p K_0, 0, 0)
    	else apply(length K, i -> frobeniusDirectImage(p, K.dd_(i+1)))
	)
    )

-- Internal method. Modification of isRelativeInterior in NormalToricVarieties.
-- This checks if a vector lies in the cone, not necessarily the interior.
isContainedInCone = method();
isContainedInCone (NormalToricVariety, List, Matrix) := Boolean => (X, sigma, v) -> (
    H := outerNormals(X, sigma);
    H0 := transpose H#0;
    H1 := transpose H#1;
    all (flatten entries (H0 * v), i -> i <= 0) and 
	all (flatten entries(H1 * v), i -> i === 0)
    )

-- Internal method. Computes the preimages of all codimension-k cones via a toric map. currently
-- only used for k = 0, but some old code used k = 1 as well for gluing complexes.
conePreimages = method();
conePreimages (ToricMap, ZZ) := (phi, k) -> (
    X := source phi;
    n := dim X;
    Y := target phi;
    M := matrix phi;
    hashTable ( 
	-- indexed by codimension-k cones in the target.
	for tau in orbits(Y,k) list (
	    tau => flatten (
		-- start searching through the lowest codimension cones
		-- first. if we find one of lower codimension, we don't
		-- need to check the higher codimension cones.
		i := -1;
		flag := true;
		while(i < k and flag) list (
		    i = i + 1;
		    -- for each cone in the source, check if its interior
		    -- maps into the target cone.
		    preimages := for sigma in orbits(X,i) list (
			sigmainterior := ((transpose matrix rays X)_sigma) *
			(transpose matrix {for j to #sigma - 1 list 1});
			if isContainedInCone(Y,tau,M*sigmainterior) then (
			    sigma
			    )
			else continue
			);
		    if preimages == {} then continue
		    else (
			flag = false;
			preimages
			)
		    )
		)
	    )
	)
    )

-- Internal method. this method computes the exponent vectors for the generators of the affine semigroup
-- corresponding to w. if w is not maximal, then we only return the non-invertible generators.
affineSemigroupGenerators = method();
affineSemigroupGenerators (NormalToricVariety, List) := Matrix => (X, w) -> (
    if w == {} then return matrix toList (dim X:{0});
    if #max X == 1 then return id_(ZZ^(#rays X));
    R := ring X;
    wC := for i to numgens R - 1 list if isSubset({i},w) then continue else i;
    M := transpose matrix (degrees R)_wC;
    N := transpose matrix (degrees R)_w;
    result := solve(M,-N);
    matrix for i to numgens R - 1 list (
	for j to numcols result - 1 list (
    	    	if member(i,wC) then result_j_(position(wC, k-> k == i))
		else if i == w_j then 1 else 0
	    )
	)
    )

-- computes a surjection to the projective variety associated to
-- a non-zero element of the nef cone of X.
-- INPUT: X - a normal toric variety
--        v - a vector presumed to be in the nef cone of X.
-- OUTPUT:  the toric morphism (contraction) f : X -> Y corresponding to the
--	    face of Nef(X) which v lies in the interior of.
-- WARNING: Y may not be a smooth toric variety, in which case we can't use the HDI
--	    method for computing higher direct images.
nefContraction = method();
nefContraction (NormalToricVariety, Vector) := ToricMap => (X, v) -> (
    nefContraction(X, entries v))
nefContraction (NormalToricVariety, Matrix) := ToricMap => (X, v) -> (
    nefContraction(X, flatten entries v))
nefContraction (NormalToricVariety, List) := ToricMap => (X,v) -> (
    R := ring X;
    nefgens := nefGenerators X;
    if not contains(coneFromVData nefgens, matrix vector v) then error "--v needs to be in the nef cone";
    if matrix {v} == 0 then error "--v should be non-zero";
    Xrays := rays X;
    mons := entries transpose matrix flatten ((flatten entries basis(v,R))/exponents);
    quotrays := {};
    zerocount := for i to #mons-1 list (
	count := sum apply(mons_i, j -> if j == 0 then 1 else 0);
	if count == #mons_0 then quotrays = append(quotrays, (Xrays)_i);
	count
        );
    proj := if quotrays != {} then (
	quot := prune coker transpose matrix quotrays;
	inverse quot.cache.pruningMap
	) else id_(ZZ^(dim X));
    newmons := {};
    rayinds := for i to #mons-1 list (
	if zerocount_i == #mons_0 then continue;
	if zerocount_i < numrows proj then continue else (
	    if member(mons_i,newmons) then continue else (
		newmons = append(newmons,mons_i); 
		i
		)
	    )
	);
    M := (matrix mons)^rayinds;
    Yrays := entries ((matrix Xrays)^rayinds * transpose matrix proj);
    Ymax := for i to numcols M - 1 list (
        maxcone := for j to numrows M - 1 list (
	    if M_i_j == 0 then j else continue
	    );
	if #maxcone >= numrows proj then maxcone else continue
	);
    Y := normalToricVariety(Yrays, Ymax);
    map(Y,X,matrix proj)
    )

-- applies nefConeContraction to all of the rays of the nef cone for convenience.
-- INPUT: X - a normal toric variety.
-- OUTPUT:  all contractions corresponding to the rays of the nef cone of X.
nefRayContractions = method();
nefRayContractions NormalToricVariety := List => X -> (
    apply(entries transpose nefGenerators X, v -> nefContraction(X,v))
    )

-- applies nefConeContraction to all nontrivial faces of the nef cone.
-- INPUT: X - a normal toric variety.
-- OUTPUT:  all contractions corresponding to the all faces of the nef cone of X.
-- WARNING: exponential in the number of rays of the nef cone.
allNefContractions = method();
allNefContractions NormalToricVariety := List => X -> (
    nefgens := nefGenerators X;
    if nefgens == 0 then {} else (
    	enefgens := entries transpose nefgens;
    	vs := for s in drop(subsets (#enefgens), 1) list sum enefgens_s; 
    	apply(vs, v -> nefContraction(X,v))
	)
    )

-- computes the eigencharacters of R^i phi_* O_X(D). See Algorithm A.5.
-- INPUT: phi - a surjective toric morphism
--        i   - an integer
--	  D   - a list indexing a toric divisor on source phi.
-- OUTPUT:  a hashtable whose keys are the eigencharacters, and the values
--          are the pairs (D,E) from Theorem 5.6.
-- NOTE:    the eigencharacters are embedded in M_X as opposed to M_K from
--          the paper--this is for convenience for the computations.
computeEigencharacters = method();
computeEigencharacters (ToricMap, ZZ, ToricDivisor) := (phi, i, D) -> computeEigencharacters (phi, i, entries D)
computeEigencharacters (ToricMap, ZZ, List) := (phi, i, D) -> (
    X := source phi;
    Y := target phi;
    if not isSmooth X then error("-- source is not smooth.");
    if not isSmooth Y then error("-- target is not smooth.");
    if not isSurjective matrix phi then error("-- not implemented for nonsurjective maps");
    if #D != numgens ring X then error("-- D needs to index a toric divisor on source");
    -- here are some obvious bounds where the answer is trivial.
    if i < 0 or i > dim X then return hashTable {};
    preimages := conePreimages(phi,0);
    -- cache the negset data.
    if not(X.cache#?rawHHOO) then HH^0(X,OO_X);
    cohomdata := X.cache.rawHHOO;
    negsets := for datum in cohomdata#i list (
	datum_2
	);
    phiCDiv := cartierDivisorGroup phi; 
    -- choose a basis for the character lattice of the kernel torus.
    MK := gens ker matrix phi;
    chardata := flatten for sigma in max Y list (
	-- local map from M_Y to CDiv(Y) sending chi -> \sum_{rho in w} <chi, rho> D_rho.
	iota := affineSemigroupGenerators(Y, sigma);
	-- local embedding of M_Y into M_X using iota.
    	localTorusEmbedding := solve(matrix rays X, phiCDiv * iota);    
    	suppset := unique flatten preimages#sigma;
	-- these are the restricted negsets.
    	rnegsets := for u in negsets list (
	    if isSubset(u,suppset) then u else continue
	    );
	-- Compute Gamma_sigma as described in the algorithm.
	-- First we compute all of the polyhedra.
    	Ps := for neg in rnegsets list (
	    (I, v) := toSequence transpose for j in suppset list (
	    	if member(j,neg) then {(rays X)_j, {-D_j - 1}}
	    	else {-(rays X)_j, {D_j}}
	    	);
	    P := polyhedronFromHData(matrix I, matrix v);
	    if not isEmpty P then P else continue
	    );
        if Ps == {} then {} else (
	    -- take the bounded part of the polyhedron, then compute
	    -- the lattice points of this. this gives Gamma_sigma.
	    pts := flatten (apply(Ps/vertices, P -> convexHull P)/latticePoints);
	    -- here is the set of eigencharacters C(L,i).
    	    CLi := unique for p in pts list MK * (transpose MK) * p;
	    -- for each character, compute this minimizing divisor D_sigma.
	    for mu in CLi list (
		-- these are all of the divisors to minimize over.
		slicepts := select(pts, p -> MK * (transpose MK) * p == mu);
		newpts := for p in slicepts list (
	    	    newp := p - MK * (transpose MK) * p;
	    	    solve(localTorusEmbedding, newp)
	    	    );
    	    	mmin := for j to dim Y - 1 list min apply(newpts, k -> k_0_j);
	    	Dsigma := iota * transpose matrix {mmin};
        	{mu, Dsigma}
		)
	    )
	);
    -- now compute the global (D,E) pairs for each eigencharacter.
    if chardata == {} then hashTable {} else (
    	CLi := unique (transpose chardata)_0;
    	hashTable for mu in CLi list mu => (
	    allDs := apply(select(chardata, x -> x_0 == mu), y -> y_1);
	    -- initialize a choice of Dsigma
	    Dtau := allDs_0;
	    -- if all the local Dsigmas are the same, then E = 0.
	    if same apply(allDs, x -> phiCDiv * x) then (
		(matrix rays X * mu) + (matrix vector D) + phiCDiv * Dtau, {toList (#rays Y - dim Y:0)}
		) 
	    else (
		-- translate by Dtau.
	    	differences := for j to #allDs - 2 list allDs_(j+1) - Dtau;
	        Emu := -matrix for e in entries matrix {differences} list {min prepend(0,e)};
		-- since we just need to twist at the end, might as well compute it here.
	    	twist := transpose entries (fromCDivToPic Y * Emu);
	    	((matrix rays X * mu) + (matrix vector D) + phiCDiv * (Dtau - Emu), twist)
	    	)
	    )
	)
    )

-- CURRENTLY UNEXPORTED. Doesn't seem particularly useful to the user.
-- the multiple input options are a holdover from when I did export it.
-- constructs the complex \v{C}(I_P(D)); see Algorithm A.8.
-- INPUT: phi - a surjective toric morphism
--        i   - an integer
--	  D   - a list indexing a toric divisor on source phi.
-- OUTPUT:  a complex supported in degrees -1, 0, and 1 whose cohomology is equal to R^i phi_* O_X(D).
HDIComplex = method();
HDIComplex (ToricMap, ZZ, ToricDivisor) := (phi, i, D) -> HDIComplex(phi, i, entries D)
HDIComplex (ToricMap, ZZ, Module) := (phi, i, D) -> (
    if ring D != ring source phi then error("-- must be a module over the source of the map");
    if not isFreeModule D then error("-- module is not free");
    if rank D != 1 then error("-- module is not rank one");
    -- this chooses a divisor to represent the twist.
    inp := flatten entries( (inverse fromCDivToPic source phi) * transpose matrix degrees D );
    HDIComplex(phi,i,inp)
    )
--HDIComplex (ToricMap, ZZ, CoherentSheaf) := (phi, i, D) -> sheaf(target phi, HDIComplex(phi, i, module D))
HDIComplex (ToricMap, ZZ, List) := (phi, i, D) -> (
    eigenchars := computeEigencharacters(phi, i, D);
    X := source phi;
    Y := target phi;
    R := ring X;
    S := ring Y;
    -- here is the construction of the reduced Cech complex by resolving the irrelevant ideal.
    R' := QQ (monoid [gens R, Degrees => entries id_(ZZ^(#rays X)), MonomialOrder => Lex]);
    XtoR' := map(R', R, gens R');
    C := (map(ZZ,R', toList (#rays X:1))) Hom(complex resolution XtoR' ideal X, R'^1);
    C = naiveTruncation(C,-length C,-1);
    A := cartierDivisorGroup phi;
    -- the terms of the complex are indexed by torus orbits, so we record those.
    allorbits := for j from -1 to 1 list (
	if i+j < 0 or i+j > dim X then {} else orbits(X,i+j)
	);
    if keys eigenchars == {} then return complex S^0;
    -- now we'll construct a new complex by replacing the terms of C with ideals in S.
    -- NOTE: since we're going to take cohomology in the i-th position, we only need
    -- the terms in degrees i-1, i, and i+1, which saves a lot of work.
    -- the parameter j is tracking these terms.
    directSum for mu in (sortBy entries) keys eigenchars list (
    	(Dmu,Emu) := eigenchars#mu;
	-- this hashtable keeps track of the non-zero terms of the complex.
        nonzerow := new MutableHashTable from hashTable for j from -1 to 1 list j => {};
	-- here are the terms of the complex.
	complexterms := for j from -1 to 1 list (
	    if i+j < 0 or i+j > dim X then {S^0} else (
		for w in allorbits_(j+1) list (
		    -- for each cone w, we form an inequality for each ray in w.
		    -- the inequality says that the fine degree of a variable
		    -- shifted by -Dmu needs to be >= 0.
		    I := matrix {{-A^w},{-id_(ZZ^(#rays Y))}};
		    v := matrix {{Dmu^w}, {matrix toList (#rays Y:{0})}};
		    P := polyhedronFromHData(I,v);
		    -- if the polyhedron defined by these inequalities is non-zero
		    -- then the vertices give generators of an ideal in S_Y.
		    if isEmpty P then S^0 else (
			-- record that the w term is non-zero
			nonzerow#j = append(nonzerow#j, w);
			-- compute the vertices and output the ideal.
			idealgens := latticePoints convexHull vertices P;
			module ideal for gen in idealgens list S_(flatten entries gen)
			)
		    )
		)
	    );
	-- now we arrange the terms into a complex. since the maps are given by C
	-- we just need to appropriately replace the terms, and so the following
	-- code is a bunch of index fiddling.
	-- Note the twist of the complex occurs here.
	S^Emu ** complex reverse for j to 1 list (
	    -- no incoming or outgoing terms means the map is zero.
	    mat := if allorbits_j == {} or nonzerow#(j-1) == {} then 0 else (
		K := #allorbits_j - 1;
		matrix transpose for k to K list (
		    if not member(allorbits_j_k, nonzerow#(j-1)) then continue
		    else (
			L := #allorbits_(j+1) - 1;
		    	for l to L list (
		            if not member(allorbits_(j+1)_l, nonzerow#j) then continue
		    	    else (
				-- this picks out the appropriate sign in the complex C, and multiplies
				-- it by the natural inclusion map between ideals.
			        C.dd_(-i-j)_(K-k)_(L-l) * (gens complexterms_j_k // gens complexterms_(j+1)_l)
			    	)
			    )
			)
		    )		
		);
	    -- once we have the maps and the terms, we can put them into a complex.
	    map(directSum complexterms_(j+1), directSum complexterms_j, mat)
	    )
	)
    )

-- method for computing higher direct images.
-- INPUT: phi - a surjective toric morphism
--        i   - an integer
--	  D   - a list indexing a toric divisor on source phi
-- OUTPUT:  the i-th cohomology of HDIComplex(phi,i,D).
HDI = method();
HDI (ToricMap, ZZ, List) := Module => (phi, i, D) -> (
    -- check if we've computed this for i and D already.
    phi.cache.HDI ??= new MutableHashTable;
    phi.cache.HDI#{i, D} ??= HH_1 HDIComplex(phi, i, D)
    )
HDI (ToricMap, ZZ, ToricDivisor) := Module => (phi, i, D) -> HDI(phi, i, entries D)
HDI (ToricMap, ZZ, Module) := Module => (phi, i, D) -> (
    if not (ring D === ring source phi) then error("-- must be a module over the source of the map");
    if not isFreeModule D then error("-- module is not free");
    if rank D != 1 then error("-- module is not rank one");
    -- this chooses a divisor to represent the twist.
    inp := -flatten entries( (inverse fromCDivToPic source phi) * transpose matrix degrees D );
    HDI(phi, i, inp)
    )
HDI (ToricMap, ZZ, CoherentSheaf) := Module => (phi, i, D) -> sheaf(target phi, HDI(phi, i, module D))

-- This allows the notation "phi_*^i D" to produce higher direct images.
ToricMap_* := phi -> new ScriptedFunctor from {
    superscript => (
	i -> new ScriptedFunctor from {
	    argument => D -> HDI(phi, i, D)
	    }
	)
    }

beginDocumentation()

doc ///
    Key
        ToricHigherDirectImages
    Headline
        computations involving pushforwards and higher direct images of toric maps
    Description
        Text
	    Given a morphism of varieties $\varphi \colon X \rightarrow Y$, we have
	    the pushforward functor $\varphi_*$ from the category of coherent sheaves
	    on $X$ to coherent sheaves on $Y$. This functor is not right-exact, and so
	    there is a right-derived functor $R^i \varphi_*$ from the bounded derived
	    category $D^b(X)$ to $D^b(Y)$, which we call the higher direct image functor.
	    When both $X$ and $Y$ are toric varieties, coherent sheaves arise from
	    finitely generated multigraded modules over their Cox rings. Thus, for a
	    given coherent sheaf $\mathcal{F}$, there are finitely generated graded
	    modules over the Cox ring of $Y$ which sheafifies to $R^i \varphi_* \mathcal{F}$.
        Text
	    The purpose of this package is to compute (higher) direct images of
	    toric morphisms $\varphi \colon X \rightarrow Y$ between smooth
	    projective toric varieties. This is currently implemented in two
	    situations:
	Text
	    1) When $\varphi$ is a toric fibration, i.e. a surjective
	    toric morphism with $\varphi_* \mathcal{O}_X = \mathcal{O}_Y$,
	    then the method \texttt{HDI} allows one to compute the higher direct
	    images of a line bundle $\mathcal{O}_X(D)$ on $X$. In "Reduced \v{C}ech complexes and computing higher direct 
	    images under toric maps", M. Roth provides a constructive method for producing
	    this module and S. Zotine adapts that method into an algorithm. This package
	    provides the implementation of this algorithm. 
	Text
	    For instance, if $X$ is a blowup of $\mathbb{P}^2$, then the first higher direct
	    image of $O_X(3E)$ is nontrivial, where $E$ is the exceptional divisor.
	Example
	    X = hirzebruchSurface 1;
	    Y = toricProjectiveSpace 2;
	    phi = map(Y, X, matrix{{0,-1},{1,0}});
	    D = {0,3,0,0};
	    prune HDI(phi, 1, D)
	Text
	    2) When $\varphi \colon Y \rightarrow Y$ is a toric Frobenius map, i.e.
	    induced from the natural morphism on the dense torus given by raising all
	    of the coordinates to the same power, then the method \texttt{frobeniusDirectImage}
	    allows one to compute pushforwards of any coherent sheaf. For instance,
	    the pushforward of the trivial bundle on $\mathbb{P}^2$ by the second Frobenius map
	    splits into 4 line bundles.
	Example
	    S = ring Y;
	    frobeniusDirectImage(2,S^1)
	Text
	    @SUBSECTION "References"@
	Text
	    M. Roth and S. Zotine, \textit{Reduced \v{C}ech complexes and computing higher direct images under toric maps}, to appear on arXiv
	Text
            @SUBSECTION "Contributors"@
	Text
            The following people have generously contributed code, improved existing code, or 
	    enhanced the documentation:
	    @HREF("https://www-users.cse.umn.edu/~mahrud/", "Mahrud Sayrafi")@, and
	    @HREF("https://mast.queensu.ca/~ggsmith/", "Gregory G. Smith")@.
    Caveat
	In case 1), we have only implemented the computation for line bundles, and if the
	target is simplicial. The only finite maps for which the pushforward is
	implemented are the Frobenius maps.
///

doc ///
    Key
	frobeniusDirectImage
	(frobeniusDirectImage, ZZ, Module)
    Headline
        compute the pushforward of a module under the $p$th toric Frobenius map
    Usage
        frobeniusDirectImage(p, M)
        frobeniusDirectImage_p M
    Inputs
	p : ZZ
	M : Module
    Outputs
	  : Module
	    which is the pushforward of M under the $p$th toric Frobenius.
    Description
        Text
	    The $p$th toric Frobenius is a toric morphism $F_p \colon X \rightarrow X$
	    which is the extension of the natural group homomorphism $T_X \rightarrow T_X$
	    given by raising all coordinates to the $p$th power. This allows one to view the
	    Cox ring $R$ of $X$ as a module over itself, with the module action being
	    $r \cdot m :\!= r^p m$. The extension of this action to modules also allows one
	    to compute the pushforward by $F_p$. Note that $p$ need not be prime, nor related
	    to the characteristic of the ground field in any way.
	    Here is the 4th pushforward of the Cox ring of the first Hirzebruch surface.
	Example
	    X = hirzebruchSurface 1;
	    R = ring X;
	    frobeniusDirectImage(4, R^1)
	Text
	    We can pushforward many kinds of modules. Here is the pushforward of an ideal.
	Example
	    I = module ideal {R_0, R_2};
	    prune frobeniusDirectImage(2, I)
	Text
	    Here is the pushforward of a free module.
	Example
	    F = R^{{1,0},{0,1}};
	    frobeniusDirectImage(2, F)
	Text
	    Here is the pushforward of a torsion module.
	Example
	    M = R^1/(module I)
	    prune frobeniusDirectImage(2, M)
	Text
	    As mentioned, $p$ is not related to the characteristic of the field, and the
	    outputs will be the same modules over a different coefficient ring.
	Example
	    Y = hirzebruchSurface(1, CoefficientRing => ZZ/2);
	    S = ring Y;
	    I' = module ideal {S_0, S_2};
	    prune frobeniusDirectImage(2, I')
    SeeAlso
	(frobeniusDirectImage, ZZ, Matrix)
	(frobeniusDirectImage, ZZ, Complex)
///

doc ///
    Key
	(frobeniusDirectImage, ZZ, Matrix)
    Headline
        compute the pushforward of map of modules under the $p$th toric Frobenius map
    Usage
        frobeniusDirectImage(p, M)
        frobeniusDirectImage_p M
    Inputs
	p : ZZ
	f : Matrix
	    which gives a map between modules
    Outputs
	  : Matrix
	    which is the pushforward of f under the $p$th toric Frobenius.
    Description
        Text
	    The $p$th toric Frobenius is a toric morphism $F_p \colon X \rightarrow X$
	    which is the extension of the natural group homomorphism $T_X \rightarrow T_X$
	    given by raising all coordinates to the $p$th power. This allows one to view the
	    Cox ring $R$ of $X$ as a module over itself, with the module action being
	    $r \cdot m :\!= r^p m$. The extension of this action to modules also allows one
	    to compute the pushforward by $F_p$. Note that $p$ need not be prime, nor related
	    to the characteristic of the ground field in any way.
	    The pushforward is an endofunctor on the category of $R$-modules, so we may
	    apply it to maps between modules.
	Example
	    X = hirzebruchSurface 1;
	    R = ring X;
	    M = koszul_1 vars R
	    frobeniusDirectImage_2 M
    SeeAlso
	(frobeniusDirectImage, ZZ, Module)
	(frobeniusDirectImage, ZZ, Complex)
///

doc ///
    Key
	(frobeniusDirectImage, ZZ, Complex)
    Headline
        compute the pushforward of a complex of modules under the $p$th toric Frobenius map
    Usage
        frobeniusDirectImage(p, C)
        frobeniusDirectImage_p C
    Inputs
	p : ZZ
	C : Complex
	    of modules
    Outputs
	  : Complex
	    which is the pushforward of C under the $p$th toric Frobenius.
    Description
        Text
	    The $p$th toric Frobenius is a toric morphism $F_p \colon X \rightarrow X$
	    which is the extension of the natural group homomorphism $T_X \rightarrow T_X$
	    given by raising all coordinates to the $p$th power. This allows one to view the
	    Cox ring $R$ of $X$ as a module over itself, with the module action being
	    $r \cdot m :\!= r^p m$. The extension of this action to modules also allows one
	    to compute the pushforward by $F_p$. Note that $p$ need not be prime, nor related
	    to the characteristic of the ground field in any way.
	    The pushforward is an endofunctor on the category of $R$-modules, so we may
	    apply it to complexes of $R$-modules.
	Example
	    X = hirzebruchSurface 1;
	    R = ring X;
	    C = complex koszul vars R
	    frobeniusDirectImage_2 C
    SeeAlso
	(frobeniusDirectImage, ZZ, Module)
	(frobeniusDirectImage, ZZ, Matrix)
///

doc ///
    Key
	nefContraction
	(nefContraction, NormalToricVariety, List)
	(nefContraction, NormalToricVariety, Vector)
	(nefContraction, NormalToricVariety, Matrix)
    Headline
        produce a contraction from a vector in the nef cone
    Usage
	nefContraction(X, v)
    Inputs
	X : NormalToricVariety
	v : {List, Vector, Matrix}
	    a vector presumed to be in the nef cone of $X$
    Outputs
	  : ToricMap
    Description
        Text
	    A vector in the nef cone of a toric variety determines an embedding
	    of the toric variety via GIT quotients. When the vector lies on the boundary
	    of the nef cone, the resulting embedding is typically a "smaller" toric variety
	    on which the original variety can surject. These types of maps are commonly
	    used in the minimal model program (MMP)
	Text
	    More precisely, see Lemma 14.4.6 and Theorem 15.1.10 in HREF("https://dacox.people.amherst.edu/toric.html", 
	    "Toric varieties"). The nef cone of a toric variety is a GKZ chamber and each
	    face of this cone is also a GKZ chamber, hence the nef cone of another toric variety.
	    Since both of these are GIT quotients, there is a natural map induced by the projection
	    of vector spaces between the quotients, which this method produces.
	Text
	    The first Hirzebruch surface $\mathbb{F}_1$ comes with two natural maps to
	    $\mathbb{P}^1$ (projection to the base) and $\mathbb{P}^2$ (the blowdown map).
	    Both of these can be realized as maps obtained from the MMP.
	Example
	    X = hirzebruchSurface 1;
	    pi1 = nefContraction(X, vector {1,0})
	    assert(isWellDefined pi1);
	    assert(isProjective target pi1 and dim target pi1 == 1);
	    pi2 = nefContraction(X, matrix {{0},{1}})
	    assert(isProjective target pi1 and dim target pi2 == 2);
	    assert(rank picardGroup target pi2 == 1);
	Text
	    If the vector is in the interior of the nef cone, then the resulting toric variety
	    is the same as the original, and so the resulting surjection is just the identity.
	Example
	    idty = nefContraction(X,{1,1})
	    assert(rays target idty == rays X);
	    assert(max target idty == max X);
    SeeAlso
	(nefRayContractions, NormalToricVariety)
	(allNefContractions, NormalToricVariety)
///

doc ///
    Key
	nefRayContractions
	(nefRayContractions, NormalToricVariety)
    Headline
        produces all contractions corresponding to rays in the nef cone
    Usage
	nefRayContractions X
    Inputs
	X : NormalToricVariety
    Outputs
	  : List
	    of ToricMaps obtained by each ray of the nef cone
    Description
        Text
	    This method is a convenient way to produce all of the nef contractions
	    corresponding to the rays of the nef cone, without the user needing to
	    know those rays.
	Example
	    X = hirzebruchSurface 1;
	    L = nefRayContractions X
	    assert(isProjective target L_0 and dim target L_0 == 1);
	    assert(isProjective target L_1 and dim target L_1 == 2);
	    assert(rank picardGroup target L_1 == 1);
    SeeAlso
	(nefContraction, NormalToricVariety, List)
	(nefContraction, NormalToricVariety, Vector)
	(nefContraction, NormalToricVariety, Matrix)
	(nefRayContractions, NormalToricVariety)
///

doc ///
    Key
	allNefContractions
	(allNefContractions, NormalToricVariety)
    Headline
        produces all contractions corresponding to each face in the nef cone
    Usage
	nefRayContractions X
    Inputs
	X : NormalToricVariety
    Outputs
	  : List
	    of ToricMaps obtained by contracting a face of the nef cone
    Description
        Text
	    This method chooses a vector in the relative interior of
	    every face of the nef cone and produces the corresponding map.
	Example
	    X = hirzebruchSurface 1;
	    L = allNefContractions X
	    assert(all(L, phi -> isWellDefined phi))
    Caveat
	This method is exponential in the number of rays of the fan, so the runtime
	may be quite long!
    SeeAlso
	(nefContraction, NormalToricVariety, List)
	(nefContraction, NormalToricVariety, Vector)
	(nefContraction, NormalToricVariety, Matrix)
	(nefRayContractions, NormalToricVariety)
///

doc ///
    Key
	computeEigencharacters
	(computeEigencharacters, ToricMap, ZZ, List)
	(computeEigencharacters, ToricMap, ZZ, ToricDivisor)
    Headline
        compute the eigencharacters
    Usage
	computeEigencharacters(phi, i, D)
    Inputs
	phi : ToricMap
	      a surjective toric morphism
	i   : ZZ
	D   : {List, ToricDivisor}
	      a list indexing a Cartier divisor on the source of phi
    Outputs
	  : HashTable
	    whose keys are the eigencharacters and values are the (D,E) pairs
    Description
        Text
	    This method is the implementation of Algorithm A.5 in 
	    "Reduced \v{C}ech complexes and computing higher direct 
	    images under toric maps". More precisely, the higher direct image
	    sheaf inherits a trivial torus action by the kernel torus
	    $T_K :\!= \ker \phi\vert_{T_X}$, and so splits into a direct sum
	    of sheaves indexed by eigencharacters. This method computes these
	    characters, as well as a pair of divisors $(D,E)$ for use in
	    Algorithm A.8 which computes the higher direct image sheaf.
	Example
	    X = hirzebruchSurface 1;
	    Y = toricProjectiveSpace 1;
	    phi = map(Y,X,matrix{{1,0}});
	    D = {0,-3,0,0};
	    computeEigencharacters(phi,1,D)
	Text
	    One might hope that the set of eigencharacters is a convex set,
	    but this is not always the case. For instance, when $X$
	    is the blowup of $\mathbb{P}^1 \times \mathbb{P}^1$ at the four
	    torus fixed points and phi is the projection to $\mathbb{P}^1$,
	    we can find an example of a line bundle whose eigencharacters are not convex.
	Example
	    Z = normalToricVariety({{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},
		    {0,-1},{1,-1}},{{0,1},{1,2},{2,3},{3,4},{4,5},{5,6},{6,7},{0,7}});
	    phi = map(Y,Z,matrix{{1,0}});
	    D = toricDivisor({0,-2,0,0,0,-2,1,0},Z);
	    keys computeEigencharacters(phi,1,D)
    SeeAlso
	(HDI, ToricMap, ZZ, List)
	(HDI, ToricMap, ZZ, ToricDivisor)
	(HDI, ToricMap, ZZ, Module)
	(HDI, ToricMap, ZZ, CoherentSheaf)
///

doc ///
    Key
	HDI
	(HDI, ToricMap, ZZ, List)
	(HDI, ToricMap, ZZ, ToricDivisor)
	(HDI, ToricMap, ZZ, Module)
	(HDI, ToricMap, ZZ, CoherentSheaf)
	(symbol_*, ToricMap)
    Headline
        compute the $i$th higher direct image
    Usage
	HDI(phi, i, D)
	phi_*^i D
    Inputs
	phi : ToricMap
	      a surjective toric morphism
	i   : ZZ
	D   : {List, ToricDivisor, Module, CoherentSheaf}
	      a Cartier divisor or rank one free module or line bundle on the source of phi.
    Outputs
	  : {Module, CoherentSheaf}
	    which is the $i$th higher direct image of $\mathcal{O}_X(D)$. 
    Description
        Text
	    This method implements Algorithm A.8 in "Reduced \v{C}ech complexes and computing higher direct 
	    images under toric maps".
	Text
	    The first Hirzebruch surface $\mathbb{F}_1$ has two geometric interpretations: 1) as the projective
	    bundle $\mathbb{P}(\mathcal{O}_{\mathbb{P}^1} \oplus \mathcal{O}_{\mathbb{P}^1})$
	    or as the blowup of $\mathbb{P}^2$ at a torus-fixed point. In both cases we get a
	    toric morphism, the projection and blowdown map. In the first case, the (higher) direct images
	    of a line bundle are themselves vector bundles, and hence split.
	Example
	    X = hirzebruchSurface 1;
	    Y = toricProjectiveSpace(1, Variable => y); 
	    phi = map(Y, X, matrix {{1,0}});
	    D1 = {3,4,0,0};
	    RD1 = prune HDI(phi,0,D1)
	    D2 = toricDivisor({3,-4,0,0},X);
	    RD2 = prune HDI(phi,1,D2)
	Text
	    In the second case, we can find line bundles whose (higher) direct images which have relations or torsion.
	Example
	    Z = toricProjectiveSpace(2, Variable => z);
	    phi = map(Z, X, matrix {{0,-1},{1,0}});
	    M = (ring X)^{{-6,3}};
	    RM = prune phi_*^0 M
	    L = sheaf_X M;
	    RL = prune phi_*^1 L
	    annihilator RL
    SeeAlso
	(frobeniusDirectImage, ZZ, Module)
	(frobeniusDirectImage, ZZ, Matrix)
	(frobeniusDirectImage, ZZ, Complex)
///

------------------------------------------------------------------------------
-- TESTS
------------------------------------------------------------------------------

TEST ///
X = toricProjectiveSpace 2;
S = ring X;
FS = frobeniusDirectImage_2 S^1;
assert(degrees FS == {{0}, {1}, {1}, {1}})
///

TEST ///
X = smoothFanoToricVariety(3,7);
d = dim X;
p = 4;
S = ring X;
FS = frobeniusDirectImage_p S^1
assert(rank FS == p^d)
///

TEST ///
X = hirzebruchSurface 3;
S = ring X;
M = frobeniusDirectImage_2 koszul_1 vars S;
M' = map(S^{{0, 0}, {1, -1}, {-1, 0}, {1, -1}},
         S^{{-1, 0}, {0, -1}, {-1, 0}, {1, -1}, {1, -1}, {2, -1}, {1, -1},  {3, -1},
	    {-1, 0}, {0, -1}, {-1, 0}, {1, -1}, {0, -1}, {1, -1}, {-1, -1}, {1, -1}},
     matrix {{x_0, 0,   0, 0, 0, 0,   0, x_1, 0, 0,   x_2, 0, x_3, 0, 0,   0},
             {0,   x_0, 0, 0, 0, 0,   1, 0,   0, 0,   0,   1, 0,   1, 0,   0},
             {0,   0,   1, 0, 0, x_1, 0, 0,   1, 0,   0,   0, 0,   0, x_3, 0},
             {0,   0,   0, 1, 1, 0,   0, 0,   0, x_2, 0,   0, 0,   0, 0,   1}});
assert(matrix M == M')
///

TEST ///
X = toricProjectiveSpace 1 ** toricProjectiveSpace 1
d = dim X;
p = 2;
S = ring X;
C = complex koszul vars S;
FC = frobeniusDirectImage_p C;
assert(isWellDefined FC)
assert(length FC == length C)
assert(all(length C, i -> rank FC_i == (rank C_i) * p^d))
///

TEST ///
X = smoothFanoToricVariety(4,13);
phis = nefRayContractions X;
assert(all(phis, phi -> isWellDefined phi))
assert(not isSmooth target last phis)
psi = nefContraction(X, toList (rank picardGroup X:1));
Y = target psi;
assert(rays Y == rays X)
assert(max Y == max X)
assert(matrix psi == id_(ZZ^4))
///

TEST ///
X = hirzebruchSurface(1); 
Y = toricProjectiveSpace(1, Variable => y); 
phi = map(Y, X, matrix {{1,0}});
D1 = {3,-4,0,0}
RD1 = prune HDI(phi,0,D1)
assert(RD1 == 0)
D2 = {3,4,0,0}
RD2 = prune HDI(phi,0,D2)
assert(isFreeModule RD2)
assert(rank RD2 == 5)
assert(degrees RD2 == {{-3},{-2},{-1},{0},{1}})
RD3 = prune HDI(phi,1,D1)
assert(rank RD3 == 3)
assert(degrees RD3 == {{-6},{-5},{-4}})
RD4 = prune HDI(phi,1,D2)
assert(RD4 == 0)
///

TEST ///
X = hirzebruchSurface(1); 
Y = toricProjectiveSpace(2, Variable => y); 
phi = map(Y, X, matrix {{0,-1},{1,0}});
D1 = {1,-4,1,1}
RD1 = prune HDI(phi,0,D1)
assert(not isFreeModule RD1)
assert(numrows gens RD1 == 7)
assert(unique degrees RD1 == {{3}})
D2 = {-1,4,-1,-1}
RD2 = prune HDI(phi,0,D2)
assert(isFreeModule RD2)
assert(rank RD2 == 1)
assert(degrees RD2 == {{3}})
RD3 = prune HDI(phi,1,D1)
assert(RD3 == 0)
RD4 = prune HDI(phi,1,D2)
assert(not isFreeModule RD4)
assert(numrows gens RD4 == 5)
assert(unique degrees RD4 == {{4}})
///

TEST ///
X = normalToricVariety({{1,0,0},{0,1,0},{-1,1,0},{0,-1,0},{0,0,1},{0,0,-1},{0,1,1}}, {{2,3,4},{2,3,5},{0,3,4},{0,3,5},{0,1,5},{1,2,5},{0,1,6},{0,4,6},{1,2,6},{2,4,6}}); 
Y = hirzebruchSurface(1, Variable => y); 
phi = map(Y, X, matrix{{1,0,0},{0,1,0}});
D = {0,0,0,0,0,-2,-2};
RD1 = HDI(phi,0,D)
assert(RD1 == 0)
RD2 = prune HDI(phi,1,D)
Q = ring RD2
assert(presentation RD2 == matrix {{0,0},{0,y_1^2},{y_1,0}})
RD3 = HDI(phi,2,D)
assert(RD3 == 0)
///

TEST ///
X = smoothFanoToricVariety(5,100);
phis = nefRayContractions X
phi = phis_1
assert(isSmooth target phi)
D = for j to numgens ring X - 1 list -1
RD1 = prune HDI(phi, dim source phi - dim target phi, D)
assert(isFreeModule RD1)
assert(rank RD1 == 1)
assert(degrees RD1 == {sum degrees ring target phi})
///

TEST ///
X = normalToricVariety({{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1}},{{0,1},{1,2},{2,3},{3,4},{4,5},{5,6},{6,7},{0,7}})
Y = toricProjectiveSpace 1
phi = map(Y,X,matrix{{1,0}})
assert(isWellDefined phi)
D = {0,-2,0,0,0,-2,1,0}
HT = computeEigencharacters(phi,1,D);
assert(set keys HT == set {matrix{{0},{1}}, matrix{{0},{-1}}})
///

TEST ///
X = smoothFanoToricVariety(4,13);
f = (nefRayContractions X)_1;
Y = target f;
D = {-1,-1,-1,-1,-1,-1,-1};
D' = toricDivisor X
L = OO D'
assert(f_*^0 D == 0)
assert(f_*^1 D == 0)
assert(f_*^0 D == f_*^0 D')
assert(f_*^1 D == f_*^1 D')
assert(f_*^2 D == f_*^2 D')
assert(f_*^0 D == module f_*^0 L)
assert(f_*^1 D == module f_*^1 L)
assert(f_*^2 D == module f_*^2 L)
///

end--
