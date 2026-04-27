---------------------------------------------------------------------------
-- COPYRIGHT NOTICE:
--
-- Copyright 2015 Alvise Trevisan and Alexander I. Suciu
--
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
---------------------------------------------------------------------------

newPackage(
	"ToricTopology",
	Version => "1.1",
	Date => "November 7, 2025",
	Authors => {
		{Name => "Alvise Trevisan", Email => "a.trevisan@enpicom.com", HomePage => "http://www.enpicom.com"},
		{Name => "Alexander I. Suciu", Email => "a.suciu@neu.edu"},
		{Name => "Kumar Sannidhya Shukla", Email => "kshukla5@uwo.ca"}
	},
	Keywords => {"Toric Geometry"},
	PackageImports => { "OldChainComplexes", "SimplicialComplexes" },
	Headline => "toric topology"
)

protect QTMSimplicialComplex
protect QTMCharacteristicMatrix
protect QTMDimension
protect MACSimplicialComplex

export {
	"SmallCover", "QuasiToricManifold", "MomentAngleComplex",
	"isValidChar",
	"smallCover", "quasiToricManifold", "momentAngleComplex",
	"cohomologyRing", "equivariantCohomology",
	"chern", "stiefelWhitney",
	"bettiSmallCover", "bettiQTM", "bettiMAC",
	"realProjectiveSpace", "hessenbergVariety", "complexProjectiveSpace",
	"QTMSimplicialComplex", "QTMCharacteristicMatrix", "QTMDimension",
	"eulerMAC"
}

-- type definitions --
SmallCover = new Type of HashTable
SmallCover.synonym = "small cover"
QuasiToricManifold = new Type of HashTable
QuasiToricManifold.synonym = "quasitoric manifold"
MomentAngleComplex = new Type of HashTable
MomentAngleComplex.synonym = "moment-angle complex"

-- constructors --
-- note: if not mod 2, first reduce
smallCover = method(TypicalValue => SmallCover)
smallCover(SimplicialComplex,Matrix) := SmallCover => (sc,lambda) -> (
	lambdamod2 := sub(lambda,ZZ/2);
	if not isValidChar(sc,lambda) then error "expected characteristic matrix";
	new SmallCover from {
		QTMSimplicialComplex => sc,
		QTMCharacteristicMatrix => lambdamod2,
		QTMDimension => rank(target(lambda))
	}
)

quasiToricManifold = method(TypicalValue => QuasiToricManifold)
quasiToricManifold(SimplicialComplex,Matrix) := QuasiToricManifold => (sc,lambda) -> (
	if not isValidChar(sc,lambda) then error "expected characteristic matrix";
	new QuasiToricManifold from {
		QTMSimplicialComplex => sc,
		QTMCharacteristicMatrix => lambda,
		QTMDimension => 2*rank(target(lambda))
	}
)

momentAngleComplex = method(TypicalValue => MomentAngleComplex)
momentAngleComplex(SimplicialComplex) := MomentAngleComplex => (sc) -> (
	R := newRing(sc.ring, Degrees=>{#vertices sc:2});
	new MomentAngleComplex from {
		MACSimplicialComplex => substitute(sc, R)
	}
)

-- methods --

-- check whether a matrix is characteristic for a given simplicial complex
isValidChar = method(TypicalValue=>Boolean);
isValidChar(SimplicialComplex,Matrix) := Boolean => (sc,lambda) -> (
	flag := true;
	mins := listMinors(sc,lambda);
	for i in mins do if (i!=1 and i!=-1) then flag=false;
	flag
)

cohomologyRing = method(TypicalValue=>QuotientRing,Options=>true)
-- cohomology ring over the integers mod 2 of a small cover
cohomologyRing(SmallCover) := QuotientRing => {CoefficientRing=>ZZ/2} >> opts -> (N) -> (
	if not opts.CoefficientRing===ZZ/2 then error "Expected ZZ/2 as coefficient ring";
	sc := N.QTMSimplicialComplex;
	lambda := N.QTMCharacteristicMatrix;
	S := (opts.CoefficientRing)[(entries(vars(ring sc)))_0];
	newgens := apply((entries(gens(ideal sc)))_0, i->sub(i,S));
	I := ideal(newgens);
	J := ideal((vars S)*(transpose lambda));
	S/(I+J)
)

-- cohomology ring over the integers of a quasitoric manifold
cohomologyRing(QuasiToricManifold) := QuotientRing =>  {CoefficientRing=>ZZ} >> opts -> (M) -> (
	sc := M.QTMSimplicialComplex;
	lambda := M.QTMCharacteristicMatrix;
	C := opts.CoefficientRing;
	S := C[(entries(vars(ring sc)))_0];
	newgens := apply((entries(gens(ideal sc)))_0, i->sub(i,S));
	I := ideal(newgens);
	J := ideal((vars S)*(transpose lambda));
	S/(I+J)
)

chern = method(TypicalValue=>List)
-- Chern classes of a quasitoric manifold
chern(QuasiToricManifold) := List => opts -> (M) -> (
	T := cohomologyRing(M,CoefficientRing=>opts.CoefficientRing);
	c := 1;
	scan ((entries(vars(ambient T)))_0, i -> c = c*(1+i));
	n := numgens target(M.QTMCharacteristicMatrix);
	toList apply(1..n, i -> part(i,sub(c,T)))
)

stiefelWhitney = method(TypicalValue=>List)
-- Stiefel-Whitney classes of a small cover
stiefelWhitney(SmallCover) := List => (N) -> (
	T := cohomologyRing(N);
	w := 1;
	scan ((entries(vars(ambient T)))_0, i -> w = w*(1+i));
	n := numgens target(N.QTMCharacteristicMatrix);
	toList apply(1..n, i -> part(i,sub(w,T)))
)

bettiSmallCover = method()
-- k-th betti number of a small cover
bettiSmallCover(ZZ,SmallCover) := ZZ => (k,N) -> (
	sc := N.QTMSimplicialComplex;
	lambda := N.QTMCharacteristicMatrix;
	n := numgens(target(lambda));
	ind := subsets(toList(1..n));
	cclist := apply(ind, I -> complex(subComplex(sc, charSupport(lambda, I))));
	b := 0;
	scan(cclist, cc -> b = b + rank(HH_(k-1)(cc)));
	b
)

-- all the betti numbers up to n of an n-dimensional small cover
bettiSmallCover(SmallCover) := List => (N) -> (
	apply(N.QTMDimension+1, i -> bettiSmallCover(i,N))
)

bettiQTM = method()
-- k-th betti number of a quasitoric manifold
bettiQTM(ZZ,QuasiToricManifold) := ZZ => (k, M) -> (
	if ((k < 0) or (k > M.QTMDimension) or (k % 2 == 1)) then (
		0
	)
	else (
		coho := cohomologyRing M;
		(((coefficients numerator reduceHilbert hilbertSeries coho)_1)_0)_(sub(k/2,ZZ))
	)
)

-- all the betti numbers up to 2n of an 2n-dimensional quasitoric manifold
bettiQTM(QuasiToricManifold) := List => (M) -> (
	apply(M.QTMDimension + 1, i -> bettiQTM(i, M))
)

-- methods involving moment-angle complexes

-- equivariant cohomology module of the moment-angle complex wrt. T^m-action
equivariantCohomology = method()
equivariantCohomology(MomentAngleComplex) := Module => (mac) -> (
	coker gens monomialIdeal mac.MACSimplicialComplex
)

-- k-th betti number of a momemnt-angle complex (as given by the Baskakov-Buchstaber-Panov theorem)
bettiMAC = method()
bettiMAC(ZZ, MomentAngleComplex) := ZZ => (k, mac) -> (
	b := 0;
	btally := betti res equivariantCohomology mac;
	for j in 0..(#vertices mac.MACSimplicialComplex) do (
		key := (2*j-k, {2*j}, 2*j);
		if btally#?key then (
			b += btally#key;
		);
	);
	b
)

-- all the betti numbers up to 2m of a MAC over a complex with m vertices
bettiMAC(MomentAngleComplex) := List => (mac) -> (
	apply(2*#vertices mac.MACSimplicialComplex + 1, i -> bettiMAC(i, mac))
)

-- the Euler characteristic of the moment angle complex
eulerMAC = method()
eulerMAC(MomentAngleComplex) := ZZ => (mac) -> (
	b := bettiMAC(mac);
	e := 0;
	m := #vertices mac.MACSimplicialComplex;
	for i in 0..(2*m) do e += (-1)^i * b#i;
	e
)

-- Sample small covers --

realProjectiveSpace = method(TypicalValue=>SmallCover)
-- n-dimensional real projective space
realProjectiveSpace(ZZ) := SmallCover => (n) -> (
	smallCover(projectiveSpace(n, ZZ/2))
)

hessenbergVariety = method(TypicalValue=>SmallCover)
-- Hessenberg variety associated to the (dual of the) n-dimensional permutahedron
hessenbergVariety(ZZ) := SmallCover => (n) -> (
	smallCover(permutahedronDual(n), lambdaHessenberg(n))
)

-- Sample quasitoric manifolds

complexProjectiveSpace = method(TypicalValue=>QuasiToricManifold)
-- n-dimensional complex projective space
complexProjectiveSpace(ZZ) := QuasiToricManifold => (n) -> (
	quasiToricManifold(projectiveSpace(n, ZZ))
)

-- Helper functions --
projectiveSpace = (n, base) -> (
	I := id_(base^n);
	ones := matrix(apply(n, i -> {-1}));
	R := base[vars(0..n)];
	K := simplicialComplex monomialIdeal {product gens R};
	(K,I|ones)
)

listMinors = (sc, lambda) -> (
	apply(facets(sc), f -> determinant(submatrix(lambda, indices(f))))
)

-- method to compute the subcomplex of sc, restricted to variables indexed by
-- the subset V
-- if V is empty, the empty complex {1} is returned
subComplex = (sc, V) -> (
	if isEmpty V then
		return simplicialComplex {1_(ring sc)};
	varlist := (entries(vars(ring sc)))_0;
	mV := sub(varlist_(V_0-1),ring sc);
	scan(drop(V,1),i->mV=mV*sub(varlist_(i-1),ring(sc)));
	candidates := {};
	for k in (0..(length V)) do (
		candidates = join(candidates, faces(k,sc));
	);
	k:=0;
	lis := {};
	while k!= length(candidates) do (
		if (denominator(sub(mV, ring sc)/(candidates_k)))==1 then (
			lis=append(lis,candidates_k);
			candidates=drop(candidates,{k,k});
			k=k-1;
		);
		k=k+1;
	);
	simplicialComplex(lis)
);

-- given a char matrix lambda (n rows, m cols) and a subset I={i_1, .., i_n} of [n]
-- returns the support of lambda_I = lambda_{i_1} + ... + lambda_{i_n}
charSupport = (lambda, I) -> (
	cI := {};
	m := numgens(source(lambda));
	n := numgens(target(lambda));
	cI=apply(m, i -> 0);
	scan(I, i -> cI = entries((transpose lambda)_(i-1)) + cI);
	fincI := apply(cI, i -> sub(i,ZZ/2));
	toList select(1..m, i -> fincI_(i-1) != 0)
);


simplicialIntToMon = (sc) -> (
	p := max( flatten( sc ) );
	R := ZZ[vars(0..p-1)];
	e := apply(p, i -> 0);
	lis := {};
	for i in (0..length(sc)-1) do (
		lis = append(lis,new MutableList from e);
		for j in sc#i do (
			lis#i#(j-1)=1;
		);
	);
	lismon := apply(lis, i -> R_(toList(i)));
	simplicialComplex(lismon)
);

-- returns the characteristic matrix for the Hessenberg variety sitting on the dual of the n-dimensional permutahedron
lambdaHessenberg = (n) -> (
	-- finds the char matrix
	col1s := {};
	lambdasimplex := id_((ZZ/2)^n)|(transpose (matrix {apply(n,i->1)} ));
	columns := new MutableHashTable;
	i :=0;
	for maxl in subsets(n+1,n) do (
		columns#maxl = lambdasimplex_{i};
		i=i+1;
	);

	vertices := drop(drop(subsets(n+1),1),-1);
	for vert in vertices do (
		if not member(vert, subsets(n+1,n)) then (
			supersets := {};
			scan(subsets(n+1,n), i -> (if (not(i==vert) and isSubset(set(vert),set(i))) then
				supersets= append(supersets,i) ) );
			col := 0;
			scan(supersets, i -> col = col+ columns#i);
			columns#vert = col;
		);
	);

	--finally computes the char matrix
	lambda := columns#(vertices#0);
	for i in 1..(length(vertices)-1) do (
		lambda = lambda | columns#(vertices#i);
	);
	lambda
);

permsimplices = (lis) -> (
	resl :={};
	for fac in lis do (
		if length(last(fac))==1 then return lis
		else (
			tmplis := {};
			for sub in subsets(last(fac),length(last(fac))-1) do (
				tmplis = append(tmplis, append(fac, sub));
			);
			resl = join(resl, tmplis);
		);
	);
	return permsimplices(resl);
)

-- returns the simplicial complex dual to the n-dimensional permutahedron
permutahedronDual = (n) -> (
	vertices := drop(drop(subsets(n+1),1),-1);
	hashgen := {};
	for i in 1..length(vertices) do (
		hashgen = append(hashgen, {vertices#(i-1),i});
	);
	vhash := hashTable(hashgen);

	psimplices := {};
	for i in 0..n do (
		psimplices = append(psimplices,{(subsets(n+1,n))#i});
	);
	simplices := {};
	for permsimplex in permsimplices(psimplices) do (
		simplex := {};
		for sub in permsimplex do (
			simplex = append(simplex, vhash#sub);
		);
		simplices = append(simplices, simplex);
	);
	simplicialIntToMon(simplices)
)


-------------------
-- Documentation --
-------------------

beginDocumentation()

doc ///
	Key
		ToricTopology
	Headline
		homological computations in toric topology
	Description
		Text
			ToricTopology is a package for computing with quasitoric
			manifolds, small covers and moment-angle complexes.

			A quasitoric manifold (or small cover) is entirely determined by a
			pair consisting of a simplicial complex K and a matrix lambda which
			is characteristic for K.

			If K has n vertices, we can think of its k-faces as sets of
			integers between 1 and n. A matrix lambda is characteristic for K
			if all maximal minors of lambda indexed by the facets of  K have
			determinant equal to 1 or -1.
	SeeAlso
		--NormalToricVarieties
///

doc ///
	Key
		SmallCover
	Headline
		the class of all small covers
	Description
		Text
			A small cover is represented by a simplicial complex K and matrix
			which is characteristic for K.
	SeeAlso
		QuasiToricManifold
///

doc ///
	Key
		QuasiToricManifold
	Headline
		the class of all quasitoric manifolds
	Description
		Text
			A quasitoric manifold is represented by a simplicial complex K and
			matrix which is characteristic for K.
	SeeAlso
		SmallCover
///

doc ///
	Key
		MomentAngleComplex
	Headline
		the class of all moment-angle complexes
	Description
		Text
			Given a simplicial complex $K$ on $m$ vertices, the moment-angle
			complex $\mathcal{Z}_K$ is a cellular complex constructed as a
			union of certain products of disks and circles: $$\mathcal{Z_K} =
			\bigcup_{\sigma \in K} \left( (D^2)^\sigma \times (S^1)^{[m]
			\setminus \sigma} \right).$$ These spaces admit a natural action of
			the torus $T^m = (S^1)^m$. Non-singular toric varieties (not
			necessarily complete) are homotopy equivalent to partial quotients
			of moment-angle complexes by freely acting subtori of $T^m$. Thus,
			moment-angle complexes are an important class of spaces studied in
			Toric Topology. Their topological properties can be determined from
			the combinatorics of the underlying simplicial complex.  This
			package implements methods to determine some of these properties.
			A moment-angle complex is a special case of polyhedral products.
	SeeAlso
		--NormalToricVariety
		QuasiToricManifold
		SmallCover
///

doc ///
	Key
		isValidChar
		(isValidChar,SimplicialComplex,Matrix)
	Headline
		whether a matrix is characteristic for a simplicial complex
	Usage
		isValidChar(K,lambda)
	Inputs
		K:SimplicialComplex
		lambda:Matrix
	Outputs
		:Boolean
	Description
		Text
			Checks whether lambda is characteristic for K.
	SeeAlso
///

doc ///
	Key
		smallCover
		(smallCover,SimplicialComplex,Matrix)
	Headline
		create a small cover
	Usage
		smallCover(K,lambda)
	Inputs
		K:SimplicialComplex
		lambda:Matrix
	Outputs
		:SmallCover
	Description
		Text
			Create a small cover over K with characteristic matrix lambda.  If
			lambda is not characteristic for K, then an error is returned.  The
			entries of lambda are automatically converted to ZZ/2 entries, if
			they not already so.
		Text
			The following example illustrates creating the 2-torus as a small
			cover over the unit square.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[a..d]
			K = simplicialComplex {a*b, b*c, c*d, d*a}
			lambda = matrix{{1, 0, 1, 0}, {0, 1, 0, 1}}
			X = smallCover(K, lambda)
	SeeAlso
///

doc ///
	Key
		quasiToricManifold
		(quasiToricManifold,SimplicialComplex,Matrix)
	Headline
		create a quasitoric manifold
	Usage
		quasiToricManifold(K,lambda)
	Inputs
		K:SimplicialComplex
		lambda:Matrix
	Outputs
		:QuasiToricManifold
	Description
		Text
			Create a quasitoric manifold over K with characteristic matrix
			lambda.  If lambda is not characteristic for K, an error is
			returned.
		Text
			The following example creates the 2-dimensional complex projective
			space as a quasitoric manifold.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[a..c]
			K = simplicialComplex {a*b, b*c, c*a}
			lambda = matrix{{1, 0, -1}, {0, 1, -1}}
			X = quasiToricManifold(K, lambda)
	SeeAlso
///

doc ///
	Key
		momentAngleComplex
		(momentAngleComplex,SimplicialComplex)
	Headline
		create a moment-angle complex
	Usage
		momentAngleComplex(K)
	Inputs
		K:SimplicialComplex
	Outputs
		:MomentAngleComplex
	Description
		Text
			Create a moment-angle complex with simplicial complex K.
		Text
			This example creates a moment-angle complex over the simplicial
			complex consisting of two disjoint vertices.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x,y]
			K = simplicialComplex {x, y}
			Z = momentAngleComplex K
	SeeAlso
///

doc ///
	Key
		equivariantCohomology
		(equivariantCohomology,MomentAngleComplex)
	Headline
		compute the equivariant cohomology of a moment-angle complex
	Usage
		equivariantCohomology(Z)
	Inputs
		Z:MomentAngleComplex
	Outputs
		:Module
			the torus equivariant cohomology of Z, as a module over polynomial
			ring
	Description
		Text
			Compute the equivariant cohomology of a moment-angle complex over
			the polynomial ring k[x_1, ..., x_m] where k is the coefficient
			ring of the polynomial ring over which the underlying simplicial
			complex was created.
		Text
			The equivariant cohomology of a moment-angle complex is free over the polynomial ring
			when the simplicial complex is a full simplex.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x,y,z]
			K = simplicialComplex {x*y*z}
			Z = momentAngleComplex K
			M = equivariantCohomology Z
			isFreeModule M
		Text
			If there is any missing simplex, then the equivariant cohomology is
			not free.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x,y,z]
			K = simplicialComplex {x*y, y*z, x*z}
			Z = momentAngleComplex K
			M = equivariantCohomology Z
			isFreeModule M
///

doc ///
	Key
		cohomologyRing
		(cohomologyRing,SmallCover)
		(cohomologyRing,QuasiToricManifold)
	Headline
		compute the cohomology ring of a small cover or quasitoric manifold
	Usage
		cohomologyRing(N)
		cohomologyRing(M)
	Inputs
		N:SmallCover
		M:QuasiToricManifold
	Outputs
		:QuotientRing
	Description
		Text
			Compute the cohomology ring of a small cover (over ZZ/2) or
			quasitoric manifold (over ZZ).
	SeeAlso

///

doc ///
	Key
		stiefelWhitney
		(stiefelWhitney,SmallCover)
	Headline
		compute the Stiefel-Whitney classes of a small cover
	Usage
		stiefelWhitney(N)
	Inputs
		N:SmallCover
	Outputs
		:List
	Description
		Text
			Compute the Stiefel-Whitney classes of a small cover.
			The output is a list of elements in the cohomology ring of N.
	SeeAlso
		cohomologyRing
///

doc ///
	Key
		chern
		(chern,QuasiToricManifold)
	Headline
		compute the Chern classes of a quasitoric manifold
	Usage
		chern(M)
	Inputs
		M:QuasiToricManifold
	Outputs
		:List
	Description
		Text
			Compute the Chern classes of a quasitoric manifold.
			The output is a list of elements in the cohomology ring of M.
	SeeAlso
		cohomologyRing
///

doc ///
	Key
		bettiSmallCover
		(bettiSmallCover,ZZ,SmallCover)
		(bettiSmallCover,SmallCover)
	Headline
		compute the Betti numbers of a small cover
	Usage
		bettiSmallCover(k,N)
		bettiSmallCover(N)
	Inputs
		k:ZZ
		N:SmallCover
	Outputs
		:ZZ
		:List
	Description
		Text
			Compute the rational Betti numbers of a small cover.  If a dimension k is
			specified, then only the k-th Betti number of N is computed.  If no
			dimension is specified, all the Betti numbers between 0 and the
			dimension of N are computed.
	SeeAlso

///

doc ///
	Key
		bettiQTM
		(bettiQTM,ZZ,QuasiToricManifold)
		(bettiQTM,QuasiToricManifold)
	Headline
		compute the Betti numbers of a quasitoric manifold
	Usage
		bettiQTM(k,M)
		bettiQTM(M)
	Inputs
		k:ZZ
		M:QuasiToricManifold
	Outputs
		:ZZ
		:List
	Description
		Text
			Compute the Betti numbers of a quasitoric manifold.  If a
			dimension k is specified, then only the k-th Betti number of M is
			computed.  If no dimension is specified, all the Betti numbers
			between 0 and the dimension of M are computed.
	SeeAlso
///

doc ///
	Key
		bettiMAC
		(bettiMAC,ZZ,MomentAngleComplex)
		(bettiMAC,MomentAngleComplex)
	Headline
		compute the Betti numbers of a moment-angle complex
	Usage
		bettiMAC(k,Z)
		bettiMAC(Z)
	Inputs
		k:ZZ
			(optional)
		Z:MomentAngleComplex
	Outputs
		:ZZ
			the k-th Betti number of the moment angle complex (if k is provided)
		:List
			of all Betti numbers
	Description
		Text
			This method computes the Betti numbers of a moment-angle complex
			using the theorem of Baskakov-Buchstaber-Panov. If a dimension k is
			specified, then only the k-th Betti number of Z is computed. If no
			dimension is specified, all the Betti numbers between 0 and 2m are
			computed (where m is the number of vertices in the underlying
			simplicial complex).
		Text
			The moment-angle complex corresponding to the simplicial complex
			consisting of two disjoint vertices is homeomorphic to $S^3$, the
			3-sphere as indicated by its Betti numbers.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x,y]
			K = simplicialComplex {x, y}
			Z = momentAngleComplex K
			bettiMAC Z
		Text
			Let $\mathcal{Z}_K$ be the moment-angle corresponding to the
			simplicial complex consisting on 3 vertices, with an edge and a
			disjoint vertex. By Hochster's formula, its third cohomology
			$H^3(\mathcal{Z}_K)$ will have rank $2$. We can verify this as
			follows,
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x..z]
			K = simplicialComplex {x, y*z}
			Z = momentAngleComplex K
			bettiMAC (3, Z)
		Text
			The moment-angle corresponding to the boundary $\partial \Delta^2$
			of the 2-simplex is homeomorphic to $S^5$, as reflected by its
			Betti numbers.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[x..z]
			K = simplicialComplex {x*y, y*z, x*z}
			Z = momentAngleComplex K
			bettiMAC Z
///

doc///
	Key
		eulerMAC
		(eulerMAC,MomentAngleComplex)
	Headline
		compute the Euler characteristic of a moment-angle complex
	Usage
		eulerMAC(Z)
	Inputs
		Z:MomentAngleComplex
	Outputs
		:ZZ
			the Euler characteristic of the moment-angle complex
	Description
		Text
			This method computes the Euler characterisitc of moment-angle
			complexes.
		Text
			The Euler characteristic of a moment-angle complex is $0$ if the
			underlying simplicial complex is not a full simplex.
		Example
			needsPackage "SimplicialComplexes"
			R = QQ[a..d]
			K0 = simplicialComplex {a*b, b*c, c*d, d*a}
			Z0 = momentAngleComplex K0
			eulerMAC Z0
			K1 = simplicialComplex {a*b*c*d}
			Z1 = momentAngleComplex K1
			eulerMAC Z1
///

doc ///
	Key
		realProjectiveSpace
		(realProjectiveSpace,ZZ)
	Headline
		real projective space of dimension n
	Usage
		realProjectiveSpace(n)
	Inputs
		n:ZZ
	Outputs
		:SmallCover
	Description
		Text
			Real projective space of dimension n, as a small cover.
	SeeAlso
		complexProjectiveSpace
///

doc ///
	Key
		complexProjectiveSpace
		(complexProjectiveSpace,ZZ)
	Headline
		complex projective space of dimension n
	Usage
		complexProjectiveSpace(n)
	Inputs
		n:ZZ
	Outputs
		:QuasiToricManifold
	Description
		Text
			Complex projective space of dimension n, as a quasitoric manifold.
	SeeAlso
		realProjectiveSpace
///

doc ///
	Key
		hessenbergVariety
		(hessenbergVariety,ZZ)
	Headline
		Hessenberg variety asscoiated to the n-permutahedron
	Usage
		hessenbergVariety(n)
	Inputs
		n:ZZ
	Outputs
		:SmallCover
	Description
		Text
			Hessenberg variety asscoiated to the n-permutahedron, as small
			cover.
	SeeAlso
///

-- test 0
TEST ///
	X = complexProjectiveSpace 1
	assert(X.QTMCharacteristicMatrix == matrix{{1, -1}})
	assert(bettiQTM X == {1, 0, 1})
///

-- test 1
TEST ///
	X = complexProjectiveSpace 2
	assert(X.QTMCharacteristicMatrix == matrix{{1, 0, -1}, {0, 1, -1}})
	assert(bettiQTM X == {1, 0, 1, 0, 1})
///

-- test 2
TEST ///
	X = complexProjectiveSpace 3
	assert(X.QTMCharacteristicMatrix == matrix{{1, 0, 0, -1}, {0, 1, 0, -1}, {0, 0, 1, -1}})
	assert(bettiQTM X == {1, 0, 1, 0, 1, 0, 1})
///

-- test 3
TEST ///
	X = realProjectiveSpace 1
	assert(X.QTMCharacteristicMatrix == sub(matrix{{1, 1}}, ZZ/2))
	assert(bettiSmallCover X == {1, 1})
///

-- test 4
TEST ///
	X = realProjectiveSpace 2
	assert(X.QTMCharacteristicMatrix == sub(matrix{{1, 0, 1}, {0, 1, 1}}, ZZ/2))
	assert(bettiSmallCover X == {1, 0, 0})
///

-- test 5
TEST ///
	X = realProjectiveSpace 3
	assert(X.QTMCharacteristicMatrix == sub(matrix{{1, 0, 0, 1}, {0, 1, 0, 1}, {0, 0, 1, 1}}, ZZ/2))
	assert(bettiSmallCover X == {1, 0, 0, 1})
///

-- test 6
TEST ///
	needsPackage "SimplicialComplexes"
	R = QQ[a..d]
	K = simplicialComplex {a*b, b*c, c*d, d*a}
	lambda = matrix{{1, 0, 1, 0}, {0, 1, 0, 1}}
	X = smallCover(K, lambda) -- 2-torus
	assert(bettiSmallCover X == {1, 2, 1})
///

-- test 7
TEST ///
	needsPackage "SimplicialComplexes"
	R = QQ[a..d]
	K = simplicialComplex {a*b, b*c, c*d, d*a}
	lambda = matrix{{1, 1, 0, 1}, {0, 1, 1, 1}}
	X = smallCover(K, lambda) -- klein-bottle
	assert(bettiSmallCover X == {1, 1, 0})
///

-- test 8
TEST ///
	needsPackage "SimplicialComplexes"
	R = QQ[a,b]
	K = simplicialComplex {a, b}
	Z = momentAngleComplex K
	assert(bettiMAC Z == {1, 0, 0, 1, 0})
	assert(eulerMAC Z == 0)
///

-- test 9
TEST ///
	needsPackage "SimplicialComplexes"
	R = QQ[a,b]
	K = simplicialComplex {a*b}
	Z = momentAngleComplex K
	assert(bettiMAC Z == {1, 0, 0, 0, 0})
	assert(isFreeModule equivariantCohomology Z)
	assert(eulerMAC Z == 1)
///

end
