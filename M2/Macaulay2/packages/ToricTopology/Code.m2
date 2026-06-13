-- -*- coding: utf-8 -*-

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
smallCover(SimplicialComplex,Matrix) := SmallCover => (sc, lambda) -> (
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
isValidChar = method(TypicalValue => Boolean);
isValidChar(SimplicialComplex, Matrix) := Boolean => (sc, lambda) -> (
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
cohomologyRing(QuasiToricManifold) := QuotientRing => {CoefficientRing => ZZ} >> opts -> (M) -> (
	sc := M.QTMSimplicialComplex;
	lambda := M.QTMCharacteristicMatrix;
	C := opts.CoefficientRing;
	S := C[(entries(vars(ring sc)))_0];
	newgens := apply((entries(gens(ideal sc)))_0, i->sub(i,S));
	I := ideal(newgens);
	J := ideal((vars S)*(transpose lambda));
	S/(I+J)
)

-- Chern classes of a quasitoric manifold
chern(QuasiToricManifold) := List => (M) -> (
	T := cohomologyRing(M);
	c := 1;
	scan ((entries(vars(ambient T)))_0, i -> c = c*(1+i));
	n := numgens target(M.QTMCharacteristicMatrix);
	toList apply(n+1, i -> part(i,sub(c,T)))
)

stiefelWhitney = method(TypicalValue=>List)
-- Stiefel-Whitney classes of a small cover
stiefelWhitney(SmallCover) := List => (N) -> (
	T := cohomologyRing(N);
	w := 1;
	scan ((entries(vars(ambient T)))_0, i -> w = w*(1+i));
	n := numgens target(N.QTMCharacteristicMatrix);
	toList apply(n+1, i -> part(i,sub(w,T)))
)

bettiSmallCover = method()
-- k-th betti number of a small cover
-- deprecated, Use betti instead
bettiSmallCover(ZZ, SmallCover) := ZZ => (k, N) -> (
	print "warning: bettiSmallCover is deprecated. Use betti instead";
	betti (k, N)
)
betti(ZZ, SmallCover) := opts -> (k, N) -> (
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
-- deprecated, Use betti instead
bettiSmallCover(SmallCover) := List => (N) -> (
	print "warning: bettiSmallCover is deprecated. Use betti instead";
	betti N
)

betti(SmallCover) := opts -> (N) -> (
	apply(N.QTMDimension+1, i -> betti(i,N))
)

bettiQTM = method()
-- k-th betti number of a quasitoric manifold
-- deprecated, Use betti instead
bettiQTM(ZZ, QuasiToricManifold) := ZZ => (k, M) -> (
	print "warning: bettiQTM is deprecated. Use betti instead";
	betti (k, M)
)

betti(ZZ, QuasiToricManifold) := opts -> (k, M) -> (
	if ((k < 0) or (k > M.QTMDimension) or (k % 2 == 1)) then (
		0
	)
	else (
		coho := cohomologyRing M;
		(((coefficients numerator reduceHilbert hilbertSeries coho)_1)_0)_(sub(k/2,ZZ))
	)
)

-- all the betti numbers up to 2n of an 2n-dimensional quasitoric manifold
-- deprecated, Use betti instead
bettiQTM(QuasiToricManifold) := ZZ => (M) -> (
	print "warning: bettiQTM is deprecated. Use betti instead";
	betti M
)

betti(QuasiToricManifold) := opts -> (M) -> (
	apply(M.QTMDimension + 1, i -> betti(i, M))
)

-- methods involving moment-angle complexes

-- equivariant cohomology module of the moment-angle complex wrt. T^m-action
equivariantCohomology = method()
equivariantCohomology(MomentAngleComplex) := Module => (mac) -> (
	coker gens monomialIdeal mac.MACSimplicialComplex
)

bettiMAC = method()
-- k-th betti number of a momemnt-angle complex
-- as given by the Baskakov-Buchstaber-Panov theorem (https://arxiv.org/abs/math/0407189)
-- deprecated, Use betti instead
bettiMAC(ZZ, MomentAngleComplex) := ZZ => (k, mac) -> (
	print "warning: bettiMAC is deprecated. Use betti instead";
	betti (k, mac)
)

betti(ZZ, MomentAngleComplex) := opts -> (k, mac) -> (
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
-- deprecated, Use betti instead
bettiMAC(MomentAngleComplex) := ZZ => (mac) -> (
	print "warning: bettiMAC is deprecated. Use betti instead";
	betti mac
)

betti(MomentAngleComplex) := opts -> (mac) -> (
	apply(2*#vertices mac.MACSimplicialComplex + 1, i -> betti(i, mac))
)

-- the Euler characteristic of the moment angle complex
-- deprecated, Use euler instead
eulerMAC = method()
eulerMAC(MomentAngleComplex) := ZZ => (mac) -> (
	print "warning: eulerMAC is deprecated. Use euler instead";
	euler mac
)
euler(MomentAngleComplex) := ZZ => (mac) -> (
	b := betti mac;
	e := 0;
	m := #vertices mac.MACSimplicialComplex;
	for i in 0..(2*m) do e += (-1)^i * b#i;
	e
)

-- topological methods for normal toric varieties --

-- equivariant cohomology of a toric variety wrt. the action of the dense torus, as a module over the polynomial ring --
equivariantCohomology(NormalToricVariety) := Module => (X) -> (
	r := dim X;
	t := vars r;
	R := QQ[t_1..t_r, Degrees=>{r:2}];
	S := (ring X)/(dual monomialIdeal X);
	m := length(rays(X));
	S = newRing(S, Degrees=>{m:2});
	A := transpose(matrix(rays(X)));
	v := vector(gens(S));
	imgGen := flatten(entries(A*v));
	f := map(S, R, imgGen);
	M := pushForward(f, S^1);
	M
)

-- betti numbers of toric varieties --
-- as given by Franz's formula (https://arxiv.org/abs/math/0308253) --
betti(ZZ, NormalToricVariety) := opts -> (k, X) -> (
	b := 0;
	btally := betti res equivariantCohomology (X);
	for j in 0..(#rays X) do (
		key := (2*j-k, {2*j}, 2*j);
		if btally#?key then (
			b += btally#key;
		);
	);
	b
)

betti(NormalToricVariety) := opts -> (X) -> (
	apply(2*(dim X) + 1, i -> betti(i, X))
)

-- euler number of toric varieties --
euler(NormalToricVariety) := ZZ => (X) -> (
	length maxCones fan X
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

-- Sample smooth toric varieties

-- AFP variety (see,
-- https://pubs.ams.org/journals/tran/2014-366-12/S0002-9947-2014-06165-5)
-- these are toric varieties whose equivariant cohomology may not be free as a
-- module over a polynomial ring

numToCone = (n, d) -> apply(d, i -> if (n >> i)%2 == 0 then i else i + d)
AFPVariety = method()
AFPVariety(ZZ, ZZ) := NormalToricVariety => (d, ord) -> (
	if ord < 1 or ord >= d then
		error "ord must be between 1 and d-1 (inclusive)";
	positiveRays := apply(d, i -> (apply(d, j -> if i == j then 1 else 0)));
	negativeRays := apply(d, i -> (apply(d, j -> if i == j then -1 else 0)));
	rayList := positiveRays | negativeRays;
	coneToSkip := ((1 << (ord+1)) - 1);
	coneNums := delete(coneToSkip, toList (1..(2^d-1)));
	coneList := apply(coneNums, n -> (numToCone (n, d)));
	normalToricVariety (rayList, coneList)
)
