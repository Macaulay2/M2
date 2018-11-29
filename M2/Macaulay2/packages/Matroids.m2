newPackage("Matroids",
	AuxiliaryFiles => true,
	Version => "0.9.6",
	Date => "February 15, 2018",
	Authors => {{
		Name => "Justin Chen",
		Email => "jchen@math.berkeley.edu",
		HomePage => "https://math.berkeley.edu/~jchen"}},
	Headline => "a package for computations with matroids",
	HomePage => "https://github.com/jchen419/Matroids-M2",
	PackageImports => {"Graphs", "Posets"},
	PackageExports => {"Graphs", "Posets"},
	DebuggingMode => true
)
export {
	"Matroid",
	"matroid",
		"ParallelEdges",
		"Loops",
	"groundSet",
	"indicesOf",
	"bases",
	"nonbases",
	"circuits",
	"fundamentalCircuit",
	"loops",
	"coloops",
	"isDependent",
	"closure",
	"hyperplanes",
	"flats",
	"latticeOfFlats",
	"restriction",
	"deletion",
	"contraction",
	"minor",
	"hasMinor",
	"relaxation",
	"representationOf",
	"quickIsomorphismTest",
	"tutteEvaluate",
	"chromaticPolynomial",
	"simpleMatroid",
	"uniformMatroid",
	"affineGeometry",
	"projectiveGeometry",
	"getCycles",
	"basisIndicatorMatrix",
	"maxWeightBasis",
	"idealChowRing",
	"cogeneratorChowRing",
	"specificMatroids",
	"allMatroids"
}

Matroid = new Type of HashTable
Matroid.synonym = "matroid"

globalAssignment Matroid
net Matroid := M -> (
	net ofClass class M | " of rank " | toString(M.rank) | " on " | toString(#M.groundSet) | " elements"
)

Matroid == Matroid := (M, N) -> set bases M === set bases N and M.groundSet === N.groundSet

matroid = method(Options => {symbol EntryMode => "bases", symbol ParallelEdges => {}, symbol Loops => {}})
matroid (List, List) := Matroid => opts -> (E, L) -> (
	if #L > 0 and not instance(L#0, Set) then L = indicesOf(E, L);
	G := set(0..<#E);
	B := if opts.EntryMode == "nonbases" then if #L == 0 then {G} else subsets(G, #(L#0)) - set L
	else if opts.EntryMode == "bases" then if #L == 0 then error "There must be at least one basis" else L
	else if opts.EntryMode == "circuits" then (
		x := symbol x;
		R := QQ[x_0..x_(#E-1)];
		I := monomialIdeal({0_R} | L/(c -> product(c/(i -> R_i))));
		allVars := product gens R;
		(dual I)_* / (g -> set indices(allVars//g))
	);
	M := new Matroid from {
		symbol groundSet => G,
		symbol bases => B,
		symbol rank => if #B > 0 then #(B#0) else 0,
		cache => new CacheTable
	};
	if opts.EntryMode == "circuits" then (
		M.cache.ideal = I;
		M.cache.circuits = L;
	) else if opts.EntryMode == "nonbases" then M.cache.nonbases = L;
	M.cache.groundSet = E;
	M.cache.rankFunction = new MutableHashTable;
	M
)
matroid List := Matroid => opts -> B -> matroid(unique flatten B, B, opts)
matroid Matrix := Matroid => opts -> A -> (
	k := rank A;
	matroid(apply(numcols A, i -> matrix A_i), (select(subsets(numcols A, k), S -> rank A_S == k))/set)
)
matroid Graph := Matroid => opts -> G -> (
	P := opts.ParallelEdges;
	L := opts.Loops/(v -> set{v});
	e := #edges G;
	E := hashTable apply(e, i -> (edges G)#i => i);
	C := getCycles G/(c -> set apply(#c-1, i -> E#(set{c#i, c#(i+1)})));
	for i from 0 to #P - 1 do (
		C = C | select(C, c -> member(E#(P#i), c))/(c -> c - set{E#(P#i)} + set{e+i}) | {set{E#(P#i), e + i}};
	);
	matroid(edges G | P | L, C | apply(#L, i -> set{e + #P + i}), EntryMode => "circuits")
)
matroid (List, MonomialIdeal) := Matroid => opts -> (E, I) -> (
	allVars := product gens ring I;
	M := matroid(E, (dual I)_* / (g -> set indices(allVars//g)));
	M.cache.ideal = I;
	M
)
matroid Ideal := Matroid => opts -> I -> (
	J := if instance(I, MonomialIdeal) then I else monomialIdeal I;
	-- The following is ~2x faster than isSquareFree
	if not(J == I and isSubset(set flatten flatten(J_*/exponents), set{0,1})) then error "Expected a squarefree monomial ideal";
	matroid(gens ring J, J)
)

ideal Matroid := MonomialIdeal => M -> ( -- Stanley-Reisner ideal of independence complex
	if not M.cache.?ideal then (
		x := symbol x;
		R := QQ[x_0..x_(#M.groundSet - 1)];
		M.cache.ideal = dual monomialIdeal({0_R} | apply(bases M, b -> product(toList(M.groundSet - b) /(i -> R_i))))
	);
	M.cache.ideal
)

isWellDefined Matroid := Boolean => M -> (
	K := keys M;
	expectedKeys := set {
		symbol groundSet, 
		symbol bases, 
		symbol rank, 
		symbol cache
	};
	if set K =!= expectedKeys then (
		if debugLevel > 0 then (
			added := toList(K - expectedKeys);
			missing := toList(expectedKeys - K);
			if #added > 0 then << "-- unexpected key(s): " << toString added << endl;
			if #missing > 0 then << "-- missing keys(s): " << toString missing << endl;
		);
		return false
	);
	if not M.groundSet === set(0..<#M.groundSet) then (
		if debugLevel > 0 then << "-- expected groundSet to be a set of integers" << endl;
		return false
	);
	if not (instance(M.bases, List) and all(bases M, b -> instance(b, Set) and isSubset(b, M.groundSet))) then (
		if debugLevel > 0 then << "-- expected bases to be a list of subsets of groundSet" << endl;
		return false
	);
	if not all(M.bases, b -> #b === M.rank) then (
		if debugLevel > 0 then << "-- expected rank to be the size of all bases" << endl;
		return false
	);
	 -- circuit elimination
	I := ideal dual M;
	if numgens ideal M < numgens I then I = ideal M;
	R := ring I;
	J := ideal flatten apply(subsets(I_*, 2), p -> (indices gcd(p#0,p#1))/(i -> p#0*p#1//(R_i^2)));
	numgens J == 0 or isSubset(J, I)
)

Matroid _ ZZ := (M, i) -> M.cache.groundSet#i
Matroid _ List := (M, S) -> (M.cache.groundSet)_S
Matroid _ Set := (M, S) -> (M.cache.groundSet)_(keys S)
Matroid _* := M -> M.cache.groundSet

indicesOf = method()
indicesOf (List, List) := List => (E, L) -> (
	H := hashTable apply(#E, i -> E_i => i);
	L/(l -> set(l/(i -> H#i)))
)
indicesOf (Matroid, List) := List => (M, L) -> (
	if #L == 0 then return {};
	if not M.cache.?indices then M.cache.indices = hashTable apply(#M.groundSet, i -> M_i => i);
	if not M.cache.indices#?(L#0) then (
		print("Warning: " | toString(L#0) | " is not a member of " | toString(M_*));
		print("Treating " | toString(L#0) | " as an index (cf. 'help groundSet' for how to input subsets) ...");
		L
	) else L/(l -> M.cache.indices#l)
)

bases = method()
bases Matroid := List => M -> M.bases

nonbases = method()
nonbases Matroid := List => M -> (
	if not M.cache.?nonbases then M.cache.nonbases = subsets(M.groundSet, rank M) - set M.bases;
	M.cache.nonbases
)

circuits = method()
circuits Matroid := List => M -> (
	if not M.cache.?circuits then M.cache.circuits = (ideal M)_*/indices/set;
	M.cache.circuits
)

fundamentalCircuit = method()
fundamentalCircuit (Matroid, List, Thing) := Set => (M, I, e) -> fundamentalCircuit(M, set indicesOf(M, I), (indicesOf(M, {e}))#0)
fundamentalCircuit (Matroid, Set, ZZ) := Set => (M, I, e) -> (
	J := I + set{e};
	for c in circuits M do if isSubset(c, J) then return c;
	print("Expected " | toString J | " to be dependent");
)

loops = method()
loops Matroid := List => M -> toList(M.groundSet - flatten(bases M / toList))

coloops = method()
coloops Matroid := List => M -> loops dual M

independentSets Matroid := List => opts -> M -> unique flatten((bases M)/subsets)
independentSets (Matroid, ZZ) := List => opts -> (M, r) -> unique flatten(bases M/(b -> subsets(b, r)))

isDependent = method()
isDependent (Matroid, List) := Boolean => (M, S) -> isDependent(M, set indicesOf(M, S))
isDependent (Matroid, Set) := Boolean => (M, S) -> (
	if #S > rank M then return true;
	I := ideal M;
	product(S/(i -> (I.ring)_i)) % I == 0
)

rank Matroid := ZZ => M -> M.rank
rank (Matroid, List) := ZZ => (M, S) -> rank(M, set indicesOf(M, S))
rank (Matroid, Set) := ZZ => (M, S) -> (
	if not M.cache.rankFunction#?S then (
		currentRank := 0;
		if #bases M > 100 then (
			I := ideal M; R := ring I;
			currentRank = dim (map((coefficientRing R)[(gens R)_(keys S)], R))(I);
		) else (
			maxRank := min(#S, rank M);
			for b in bases M do (
				currentRank = max(currentRank, #(b*S));
				if currentRank == maxRank then return currentRank;
			);
		);
		M.cache.rankFunction#S = currentRank;
	);
	M.cache.rankFunction#S
)

closure = method()
closure (Matroid, List) := List => (M, S) -> toList closure(M, set indicesOf(M, S))
closure (Matroid, Set) := Set => (M, S) -> (
	r := rank(M, S);
	if r == rank M then return M.groundSet;
	limits := set{};
	for s in toList(M.groundSet - S) do (
		if r == rank(M, S + set{s}) then limits = limits + set{s};
	);
	S + limits
)

hyperplanes = method()
hyperplanes Matroid := List => M -> (
	if not M.cache.?hyperplanes then M.cache.hyperplanes = (circuits dual M)/(c -> M.groundSet - c);
	M.cache.hyperplanes
)

flats = method()
flats (Matroid, ZZ) := List => (M, r) -> ( -- returns flats of rank r
	if r > rank M or r < 0 then return {};
	if r == rank M then return {M.groundSet};
	if r == 0 then return {set loops M};
	if r == rank M - 1 then return hyperplanes M;
	unique (select(subsets(M.groundSet, r), s -> rank_M s == r)/closure_M)
)
flats Matroid := List => M -> (
	if not M.cache.?flats then (
		H := hyperplanes M;
		flatList := H;
		numFlatsFound := 0;
		while numFlatsFound < #flatList do (
			numFlatsFound = #flatList;
			flatList = unique flatten apply(flatList, F -> apply(H, h -> h*F));
		);
		M.cache.flats = append(sort(flatList, f -> #f), M.groundSet);
	);
	M.cache.flats
)

-- sorts L by values of f (note: L should not involve sequences at all, due to deepSplice)
sort (List, Function) := opts -> (L, f) -> (
	H := hashTable((a, b) -> (a, b), apply(L, l -> f(l) => l));
	deepSplice join apply(sort keys H, k -> H#k)
)

latticeOfFlats = method()
latticeOfFlats Matroid := Poset => M -> poset(flats M/toList, (a, b) -> isSubset(a, b))

fVector Matroid := HashTable => M -> hashTable pairs tally(flats M/rank_M)

dual Matroid := Matroid => {} >> opts -> M -> (
	if not M.cache.?dual then (
		D := matroid(M_*, (bases M)/(b -> M.groundSet - b));
		D.cache.dual = M;
		M.cache.dual = D;
	);
	M.cache.dual
)

restriction = method()
restriction (Matroid, List) := Matroid => (M, S) -> restriction(M, set indicesOf(M, S))
restriction (Matroid, Set) := Matroid => (M, S) -> ( -- assumes S is a subset of M.groundSet (not M_*)
	if #bases M > 100 then (
		I := ideal M; R := ring I;
		return matroid(M_S, monomialIdeal (map((coefficientRing R)[(gens R)_(keys S)], R))(I));
	);
	B := bases M/(b -> S*b);
	r := max(B/(b -> #b));
	matroid(M_S, indicesOf(toList S, unique select(B, b -> #b == r) /toList))
)
Matroid | Set := (M, S) -> restriction(M, S)
Matroid | List := (M, S) -> restriction(M, S)

deletion = method()
deletion (Matroid, List) := Matroid => (M, S) -> deletion(M, set indicesOf(M, S))
deletion (Matroid, Set) := Matroid => (M, S) -> restriction(M, M.groundSet - S)
Matroid \ Set := (M, S) -> deletion(M, S)
Matroid \ List := (M, S) -> deletion(M, S)

contraction = method()
contraction (Matroid, List) := Matroid => (M, S) -> contraction(M, set indicesOf(M, S))
contraction (Matroid, Set) := Matroid => (M, S) -> if #S == 0 then M else dual(deletion(dual M, S))
Matroid / Set := (M, S) -> contraction(M, S)
Matroid / List := (M, S) -> contraction(M, S)

minor = method()
minor (Matroid, List, List) := Matroid => (M, X, Y) -> minor(M, set indicesOf(M, X), set indicesOf(M, Y))
minor (Matroid, Set, Set) := Matroid => (M, X, Y) -> (
	if #(X*Y) > 0 then error "Expected disjoint sets";
	N := M / X;
	N \ set((toList Y)/(y -> position(N_*, e -> M_y === e)))
)

hasMinor = method()
hasMinor (Matroid, Matroid) := Boolean => (M, N) -> (
	n := #N.groundSet;
	m := #M.groundSet;
	if n > m or #bases N > #bases M then return false;
	for X in independentSets(M, rank M - rank N) do (
		MX := M / X;
		for Y in independentSets(dual MX, m - n - rank M + rank N) do (
			if areIsomorphic(N, MX \ Y) then (
				print("Contract "|toString X|", delete "|toString (Y/(y -> y + #select(toList X, x -> x <= y))));
				return true;
			);
		);
	);
	false
)

Matroid + Matroid := (M, N) -> (
	if not M_* === N_* then (
		if #set(M_*) < #(M_*) or #set(N_*) < #(N_*) then error "Cannot have duplicate elements in M (or N) - see help page for details";
		E := unique(M_* | N_*);
		H := hashTable apply(#N.groundSet, i -> i => position(E, e -> e === N_i));
		return matroid(E, bases M) + matroid(E, bases N/(b -> b/(i -> H#i)));
	);
	r := 0;
	if #bases M * #bases N < 1e6 then (
		basesUnion := unique flatten(bases M/(b -> (bases N)/(c -> b + c)));
		r = max(basesUnion/(b -> #b));
		matroid(M_*, select(basesUnion, b -> #b == r))
	) else (
		newbases := {};
		for b in bases M do (
			for c in bases N do (
				U := b + c;
				if #U > r then ( r = #U; newbases = {U} )
				else if #U == r and not member(U, newbases) then newbases = append(newbases, U);
			);
		);
		matroid(M_*, newbases)
	)
)

Matroid ++ Matroid := (M, N) -> (
	n := #M.groundSet;
	B := bases N/(b -> b/(i -> i + n));
	E1 := (M_*)/(e -> (e, 0));
	E2 := (N_*)/(e -> (e, 1));
	matroid(E1 | E2, unique flatten(bases M/(b -> B/(c -> c + b))))
)

getComponentsRecursive = method()
getComponentsRecursive (List, List) := List => (S, C) -> (
	if #S == 0 then return {}
	else if #(set S*set flatten(C/toList)) == 0 then return subsets(S, 1);
	comp0 := select(S, s -> any(C, c -> isSubset(set{s, S#0}, c)));
	C = select(C, c -> #(c*set comp0) == 0);
	join({comp0}, getComponentsRecursive(toList(set S - comp0), C))
)
components Matroid := List => M -> (
	singles := join(loops M, coloops M);
	join(subsets(singles, 1), getComponentsRecursive(toList(M.groundSet - singles), circuits M))/set/restriction_M
)

relaxation = method()
relaxation (Matroid, List) := Matroid => (M, S) -> relaxation(M, set indicesOf(M, S))
relaxation (Matroid, Set) := Matroid => (M, S) -> (
	if not member(S, circuits M) or not member(S, hyperplanes M) then error "Expected circuit-hyperplane";
	matroid(M_*, append(bases M, S))
)

representationOf = method()
representationOf Matroid := Thing => M -> (
	if instance(M_0, Matrix) then return transpose matrix((M_*)/(v -> flatten entries v))
	else if all(M_*, c -> instance(c, Set) and #c <= 2) then (
		return graph(join(M_*, (flatten(select(M_*, c -> #c == 1)/toList))/(v -> {v,v})))
	) else print "No representation found.";
)

-- Finds all permutations inducing a bijection on circuits
-- Note: as permutations(10) is already slow on a typical machine, this method performs a time/space tradeoff
isomorphism (Matroid, Matroid) := List => (M, N) -> (
	(C, D, e) := (sort(circuits M, c -> #c), circuits N, #M.groundSet);
	if #C == 0 then return if #D == 0 then permutations e else {};
	possibles := {};
	if e > 5 then (
		c0 := toList C#0;
		shiftedIndices := apply(e, i -> i - #select(c0, j -> j < i));
		for d0 in select(D, d -> #d == #c0)/toList do (
			d1 := sort keys(N.groundSet - d0);
			B := apply(permutations d0, q -> hashTable apply(#q, i -> c0#i => q#i));
			possibles = possibles | flatten apply(isomorphism(M \ set c0, N \ set d0), p -> (
				flatten apply(B, q -> (
					candidate := apply(e, i -> if member(i, c0) then q#i else (d1)#(p#(shiftedIndices#i)));
					if all(C, c -> member(c/(i -> candidate#i), D)) then {candidate} else {}
				))
			));
		);
		return possibles;
	) else return select(permutations(e), p -> all(C, c -> member(c/(i -> p#i), D)));
)

quickIsomorphismTest = method()
quickIsomorphismTest (Matroid, Matroid) := String => (M, N) -> (
	(r, b, c, e) := (rank M, #bases M, #circuits M, #M.groundSet);
	if not (r == rank N and b == #bases N and c == #circuits N and e == #N.groundSet) then return "false";
	if M == N then ( print "Matroids are equal"; return "true" );
	if min(b, c, binomial(e, r) - b) <= 1 then ( print "At most 1 basis/nonbasis/circuit"; return "true" );
	idealList := (M,N)/(m -> (m, dual m)/ideal);
	if idealList#0/res/betti === idealList#1/res/betti then "Could be isomorphic" else "false"
)

areIsomorphic (Matroid, Matroid) := Boolean => (M, N) -> (
	testResult := quickIsomorphismTest(M, N);
	if testResult == "Could be isomorphic" then #isomorphism(M, N) > 0 else value testResult
)

tuttePolynomial Matroid := RingElement => M -> (
	if not M.cache.?tuttePolynomial then M.cache.tuttePolynomial = tuttePolynomial(M, ZZ(monoid(["x","y"]/getSymbol)));
	M.cache.tuttePolynomial
)
tuttePolynomial (Matroid, Ring) := RingElement => (M, R) -> (
	a := coloops M;
	b := loops M;
	if #a + #b == #M.groundSet then return R_0^#a*R_1^#b
	else (
		c := set{(keys((bases M)#0 - a))#0};
		return tuttePolynomial(M \ c, R) + tuttePolynomial(M / c, R)
	)
)

tutteEvaluate = method()
tutteEvaluate (Matroid, Thing, Thing) := Thing => (M, a, b) -> (
	T := tuttePolynomial M;
	sub(T, {(ring T)_0 => a, (ring T)_1 => b})
)

characteristicPolynomial Matroid := RingElement => opts -> M -> (
	T := tuttePolynomial M;
	x := symbol x;
	(map(ZZ[x], ring T, {1 - x, 0}))((-1)^(rank M)*T)
)

chromaticPolynomial = method()
chromaticPolynomial Graph := RingElement => G -> (
	P := characteristicPolynomial matroid G;
	(ring P)_0^(#connectedComponents G)*P
)

isSimple Matroid := Boolean => M -> all((ideal M)_*, m -> first degree m > 2)

simpleMatroid = method()
simpleMatroid Matroid := Matroid => M -> M \ set(select((ideal M)_*, m -> first degree m <= 2)/indices/last)

uniformMatroid = method()
uniformMatroid (ZZ, ZZ) := Matroid => (k, n) -> (
	if 0 <= k and k <= n then matroid(toList(0..<n), subsets(n, k)/set) else error(k | " is not between 0 and " | n)
)

affineGeometry = method()
affineGeometry (ZZ, ZZ) := Matroid => (n, p) -> matroid affineMatrix(n, p)

affineMatrix = (n, p) -> sub(transpose matrix toList((prepend(1,n:0)..prepend(1,n:p-1))/toList), ZZ/p)

projectiveGeometry = method()
projectiveGeometry (ZZ, ZZ) := Matroid => (n, p) -> matroid projectiveMatrix(n, p)

projectiveMatrix = (n, p) -> (
	if n == 0 then return matrix{{1_(ZZ/p)}};
	affineMatrix(n, p) | (matrix{toList((p^n-1)//(p-1):0_(ZZ/p))} || projectiveMatrix(n-1, p))
)

getCycles = method()
getCycles Graph := List => G -> (
	if not isConnected G then return flatten((connectedComponents G)/(c -> getCycles inducedSubgraph(G, c)));
	G = graph edges G; -- removes loops
	if #edges G < #G.vertexSet then return {}; -- G is a tree
	possibleVertices := select(G.vertexSet, v -> #neighbors(G, v) > 1);
	if #possibleVertices < #G.vertexSet then G = inducedSubgraph(G, possibleVertices);
	cycles := {};
	while #G.vertexSet > 2 do (
		cycles = join(cycles, select(getClosedWalks(G, G.vertexSet#0, #G.vertexSet), p -> p#1 < p#(#p - 2)));
		G = deleteVertices(G, {G.vertexSet#0});
	);
	cycles
)

getClosedWalks = method()
getClosedWalks (Graph, Thing, ZZ) := List => (G, v, l) -> ( -- Returns walks at v of length <= l
	N := toList(neighbors(G, v));
	paths := N/(n -> {v, n});
	walks := {};
	for i from 2 to l - 1 do (
		paths = flatten(paths/(p -> (toList(neighbors(G, last p) - set{v} - set p))/(n -> append(p, n))));
		walks = join(walks, (select(paths, p -> member(last p, N)))/(w -> append(w, v)));
	);
	walks
)

basisIndicatorMatrix = method()
basisIndicatorMatrix Matroid := Matrix => M -> (
	initVector := toList M.groundSet;
	transpose matrix(bases M/(b -> initVector/(i -> if member(i, b) then 1 else 0)))
)

independenceComplex Matroid := SimplicialComplex => M -> simplicialComplex ideal M

maxWeightBasis = method()
maxWeightBasis (Matroid, List) := Set => (M, w) -> (
	maxWeightSol := set{};
	W := (rsort apply(#w, i -> (w#i, i)))/last;
	while not member(maxWeightSol, bases M) do (
		for i from 0 to #W-1 do (
			augmentedSol := maxWeightSol + set{W#i};
			if not isDependent(M, augmentedSol) then (
				maxWeightSol = augmentedSol;
				W = W_(delete(i, toList(0..<#W)));
				break;
			);
		);
	);
	maxWeightSol
)

idealChowRing = method()
idealChowRing Matroid := Ideal => M -> (
	x := symbol x;
	F := delete({}, delete(M.groundSet, flats M)/toList);
	R := QQ[F/(f -> x_f)];
	I2 := ideal(select(subsets(F, 2), s -> #unique(s#0 | s#1) > max(#(s#0), #(s#1)))/(p -> x_(p#0)*x_(p#1)));
	L0 := sum(select(F, f -> member(0, f))/(f -> x_f));
	I2 + ideal((1..#M.groundSet - 1)/(i -> sum(select(F, f -> member(i, f))/(f -> x_f)) - L0))
)

cogeneratorChowRing = method()
cogeneratorChowRing Matroid := RingElement => M -> ( -- sorted flats makes this method 3x faster
	t := symbol t;
	I := trim idealChowRing M;
	R := ring I;
	W := R[apply(gens R, v -> t_(last baseName v))];
	sub(value (factor((sum(#gens R, i -> W_i*R_i))^(rank M - 1) % sub(I, W)))#1, QQ[gens W])
)

specificMatroids = method()
specificMatroids String := Matroid => name -> (
	if name == "fano" then (
		projectiveGeometry(2, 2)
	) else if name == "nonfano" then (
		relaxation(specificMatroids "fano", set{1,3,4})
	) else if name == "V8+" then (
		matroid(toList(0..7), {{0,1,2,3},{0,3,4,5},{1,2,4,5},{0,3,6,7},{1,2,6,7},{4,5,6,7}}/set, EntryMode => "nonbases")
	) else if name == "vamos" then (
		relaxation(specificMatroids "V8+", set{4,5,6,7})
	) else if name == "pappus" then (
		matroid(toList(0..8), {{0,1,2},{0,3,7},{0,4,8},{1,3,6},{1,5,8},{2,4,6},{2,5,7},{6,7,8}}/set, EntryMode => "nonbases")
	) else if name == "nonpappus" then (
		relaxation(specificMatroids "pappus", set{6,7,8})
	) else if name == "AG32" then (
		affineGeometry(3, 2)
	) else if name == "R10" then (
		matroid(id_((ZZ/2)^5) | matrix{{-1_(ZZ/2),1,0,0,1},{1,-1,1,0,0},{0,1,-1,1,0},{0,0,1,-1,1},{1,0,0,1,-1}})
	) else error "Name string must be one of: fano, nonfano, V8+, vamos, pappus, nonpappus, AG32, R10"
)

allMatroids = method()
allMatroids ZZ := List => n -> (
	if n > 8 then ( print "Can only return all matroids on <= 8 elements."; return; );
	if n == 1 then return {uniformMatroid(0, 1), uniformMatroid(1, 1)};
	startedReading := false;
	E := toList(0..<n); r := 0;
	matroidList := for l in lines get(first select(apply(path, p -> p | "Matroids/SmallMatroids.txt"), p -> fileExists p)) list (
		if startedReading then (
			if #l == 0 then break
			else if #l > binomial(n, r) then ( r = r + 1; PE := reverse sort subsets(set E, r); );
			matroid(E, PE_(positions(characters l, c -> c === "*")))
		) else if l == ("-- " | n | " elements") then (startedReading = true;)
	);
	matroidList = {uniformMatroid(0, n)} | delete(null, matroidList);
	L := toList(0..#matroidList - #select(matroidList, M -> 2*rank M == n) - 1);
	matroidList | (matroidList_L / dual)_(rsort L)
)

beginDocumentation()

-- Documentation --
-- <<docTemplate
doc ///
	Key
		Matroids
	Headline
		a package for computations with matroids
	Description
		Text
			A matroid is a combinatorial structure abstracting the notion of 
			(linear algebraic, graph-theoretic) independence. This package 
			provides methods to perform computations with matroids in Macaulay2.

			This package provides capabilities for converting between various 
			representations of matroids, creating linear and graphic 
			matroids from a matrix or graph, forming and detecting existence 
			of minors, computing Tutte polynomials, and additional 
			functions for applications of matroids to areas like optimization and
			convex geometry.

			Matroids are stored as pairs (E, B) of a ground set E and a list of bases, 
			which are sets of elements of the ground set. Internally, a ground set of 
			size n is always identified with the set $\{0, ..., n-1\}$, and thus all 
			subsets of the ground set (e.g. bases, circuits, flats) are also treated 
			as subsets of $\{0, ..., n-1\}$ (for more, cf. @TO groundSet@). However, the 
			actual elements of the ground set are allowed to be arbitrary (e.g. 
			integers, symbols, vectors, edges in a graph), and can be accessed by
			@TO2{(symbol _, Matroid, List), "taking subscripts"}@.
			
		Example
			M = matroid({a, matrix{{-1.2},{3.78}}, x, set{4,6}, -9}, {{a, x}, {x, -9}})
			peek M
			M_{0,1,4}
			peek restriction(M, set{1,2,3})
			circuits M
			netList flats M
			tuttePolynomial M
			
		Text
			A matroid can be specified by its bases, nonbases, circuits, from a 
			matrix, graph, or ideal, or via a collection of predefined matroids. For more 
			on how to construct a matroid, see @TO matroid@.

			{\bf Reference} Oxley, Matroid Theory, second edition. Oxford University Press, 2011.
///

doc ///
	Key
		Matroid
	Headline
		the class of all matroids
	Description
		Text
			To see how to specify a matroid, see @TO matroid@.

			In this package, the ground set of the matroid is always (internally) 
			assumed to be a set of the form $\{0, ..., n-1\}$. This means that 
			although the actual elements of the ground set can be arbitrary, 
			all subsets of the ground set are specified by their indices, i.e. 
			as a subset of $\{0, ..., n-1\}$ (this includes bases, circuits, flats, 
			loops, etc.). 
			
			For convenience, the user can specify a subset of the ground set
			either by indices (which are integers between 0 and n-1), or as 
			actual elements. If indices are used, they should be given as a 
			@TO Set@, and if elements are used, they should be given as a 
			@TO List@.
			
			One can use the function @TO indicesOf@ to convert elements of
			the ground set to their indices. Conversely, use 
			@TO2{(symbol _, Matroid, List), "subscripts"}@
			to obtain the elements from their indices. 
			
			A recommended way to circumvent this distinction between indices 
			and elements is to make the elements of M equal to integers from 0 
			to n-1, in which case an element is equal to its index in M.groundSet.
			
			For more on this package-wide convention, see @TO groundSet@.
		
		Example
			U24 = uniformMatroid(2, 4)
			U24 == dual U24
			ideal U24
			peek U24
			tuttePolynomial U24
			N = U24 / {0}
			areIsomorphic(N, uniformMatroid(1, 3))
		Text
			
			Many computations performed in this package are 
			cached in order to speed up subsequent 
			calculations (as well as avoiding redundancy). 
			These include the @TO circuits@, @TO flats@, 
			@TO2{(ideal, Matroid), "ideal"}@,
			@TO2{(rank, Matroid, Set), "rank function"}@, and
			@TO2{(tuttePolynomial, Matroid), "Tutte polynomial"}@
			of a matroid, and are stored in the 
			@TO CacheTable@ of the matroid. Since the cache is
			a @TO MutableHashTable@, the user can also manually
			cache data (e.g. if it has been computed in a previous
			session), which can greatly speed up computation.
			
		Example
			R10 = specificMatroids "R10"
			keys R10.cache
			time isWellDefined R10
			time fVector R10
			keys R10.cache
			time fVector R10
///

doc ///
	Key
		matroid
		(matroid, List, List)
		(matroid, List)
		(matroid, Matrix)
		(matroid, Graph)
		(matroid, Ideal)
		(matroid, List, MonomialIdeal)
		[matroid, Loops]
		[matroid, ParallelEdges]
	Headline
		constructs a matroid
	Usage
		M = matroid(E, B)
		M = matroid(E, C, EntryMode => "circuits")
		M = matroid(B)
		M = matroid(A)
		M = matroid(G)
		M = matroid(I)
	Inputs
		E:List
			a ground set
		B:List
			a list of bases
		C:List
			a list of circuits
		A:Matrix
			whose column vectors form the ground set
		G:Graph
			whose edges form the ground set
		I:Ideal
			a squarefree monomial ideal defining the independence complex
	Outputs
		M:Matroid
	Description
		Text
			The default representation of a matroid in this package is by 
			its ground set and list of bases. 
			
		Example
			M = matroid({a,b,c,d}, {{a,b},{a,c}})
			peek M
		Text

			One can create a matroid by specifying its @TO circuits@ or 
			@TO nonbases@ instead, using the option 
			@TO2{[matroid, EntryMode], "EntryMode"}@. Regardless of the 
			value of EntryMode, the bases 
			are automatically computed in the process of creation.
			
		Example
			M = matroid({a,b,c,d},{}, EntryMode => "nonbases") -- defaults to uniform matroid of full rank
			peek M
			N = matroid({a,b,c,d}, {{b,c}}, EntryMode => "circuits")
			peek N
		Text	
		
			If no ground set is provided, the ground set is taken to be the 
			union of the basis/nonbasis/circuit elements. 
			
		Example
			M = matroid {{a,b},{a,c}}
			peek M
		Text

			If a matrix is provided, then the realizable matroid on the 
			columns of the matrix is returned. The ground set consists of
			columns of the matrix, and independence is determined by 
			the method @TO rank@ (to allow flexibility over general rings).
		
		Example
			M = matroid random(ZZ^3, ZZ^5)
			peek M
		Text
		
			If a graph is provided, then the graphic matroid is returned.
			The ground set consists of edges in the graph, and circuits 
			are precisely the (minimal) cycles in the graph. 
				
		Example
			M = matroid completeGraph 3
			peek M
		Text
		
			One can use the optional arguments Loops and ParallelEdges
			to specify loops and parallel edges for the graph, respectively.
			These options are intended only for use with graphic matroids
			(since the Graphs package does not provide functionality
			for loops or parallel edges). ParallelEdges should be given as a
			list of edges (which are two-element sets of the form set$\{i,j\}$
			where i, j are vertices in G), and Loops should be given as a 
			list of vertices where the loops are based.
			
		Example
			M = matroid(completeGraph 3, ParallelEdges => {set{0,1},set{0,1},set{1,2}}, Loops => {0,2})
			peek M
			circuits M
		Text
			
			If a squarefree monomial ideal is provided, corresponding to a 
			simplicial complex &Delta; via the Stanley-Reisner correspondence,
			then the matroid with 
			@TO2{(independenceComplex, Matroid), "independence complex"}@
			&Delta; is returned. The ground set consists of the variables in the
			ring of the ideal.
			
		Example
			R = QQ[x_0..x_4]
			I = monomialIdeal (x_0*x_1*x_3,x_1*x_2*x_4,x_0*x_2*x_3*x_4)
			M = matroid I
			peek M

	Caveat
		This function does not check if (E,B) defines a matroid - see 
		@TO2{(isWellDefined, Matroid), "isWellDefined"}@.
	
		The bases are not stored as subsets of the ground set - the 
		indices (with respect to the ground set) are stored instead. For more,
		see @TO groundSet@.
	SeeAlso
		(isWellDefined, Matroid)
		bases
		indicesOf
		specificMatroids
///

doc ///
	Key
		(symbol ==, Matroid, Matroid)
	Headline
		whether two matroids are equal
	Usage
		M == N
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Boolean
			whether the two matroids are equal
	Description
		Text
			Two matroids are considered equal if they have the same 
			set of (indexed) bases and same size grounds sets (in 
			particular, the ground sets need not be identical). This 
			happens iff the identity permutation is an isomorphism.

			The strong comparison operator === should not be used, as 
			bases (and ground sets) are internally stored as lists rather 
			than sets, so the same matroid with a different ordering on 
			the list of bases (or ground set) will be treated as different 
			under ===. (One might try to sort the list of bases, but 
			this is potentially time-consuming, as the list of bases can 
			grow rapidly with the size of the ground set.)
			
		Example
			M = matroid completeGraph 3
			peek M
			N = uniformMatroid(2, 3)
			peek N
			M == N
			M === N
			AG32 = specificMatroids "AG32" -- identically self-dual
			AG32 == dual AG32
			AG32 === dual AG32
			V = specificMatroids "vamos" -- self-dual, but not identically so
			V == dual V
			areIsomorphic(V, dual V)
	SeeAlso
		(isomorphism, Matroid, Matroid)
///

doc ///
	Key
		[matroid, EntryMode]
	Headline
		select method of specifying matroid
	Usage
		matroid(..., EntryMode => "bases")
		matroid(..., EntryMode => "nonbases")
		matroid(..., EntryMode => "circuits")
	Description
		Text
			A matroid is determined by its set of bases, i.e. maximal 
			(with respect to inclusion) independent sets, which are all 
			of the same size (namely, the rank of the matroid). However, 
			many interesting matroids have relatively few dependencies, 
			and thus it may be easier to specify the matroid by its 
			@TO nonbases@, i.e. dependent subsets of the ground set, with 
			size equal to the rank of the matroid.

			Similarly, a matroid can be specified by its @TO circuits@, i.e. 
			minimal dependent sets. This is done e.g. when creating 
			a graphical matroid.
			
			If EntryMode is not specified, then the default value is assumed, 
			which is EntryMode => "bases".
			
		Example
			M = matroid({{0,1,2}, {3,4,5}}, EntryMode => "circuits") -- bowtie graph
			bases M
			F7 = matroid({{0,1,2},{2,3,4},{2,5,6},{0,4,5},{0,3,6},{1,3,5},{1,4,6}}, EntryMode => "nonbases")
			F7 == specificMatroids "fano"
	SeeAlso
		matroid
		nonbases
		circuits
///

doc ///
	Key
		(isWellDefined, Matroid)
	Headline
		whether the input is a well-defined matroid
	Usage
		isWellDefined M
	Inputs
		M:Matroid
	Outputs
		:Boolean
			whether or not a set of subsets satisfies the circuit exchange axiom
	Description
		Text
			If E is a set and C is a collection of subsets of E such that (i) no two 
			elements of C are comparable, and (ii): for C1, C2 in C and 
			$e \in &nbsp; C1 \cap&nbsp;C2$, there exists $C3 \in &nbsp; C$ with 
			$C \subseteq&nbsp;(C1 \cup&nbsp;C2) - e$, then C is the set of circuits of a 
			matroid on E. Property (ii) is called the circuit exchange axiom, and
			these characterize the collections of subsets of E which can be circuits for
			a matroid on E. This method verifies if the circuit exchange axiom 
			holds for the given input, and additionally whether the input has the 
			correct keys and data types that an object of type Matroid has.
			
		Example
			isWellDefined matroid({a,b,c,d},{{a,b},{c,d}})
			isWellDefined matroid({a,b,c,d},{{a,b},{a,c}})
			isWellDefined matroid({{1,2,3},{1,4,5},{2,3,4,5},{2,3,6,7},{4,5,6,7}}, EntryMode =>"circuits") -- the Escher "matroid"
			isWellDefined matroid({{1,2,3},{1,4,5},{1,6,7},{2,3,4,5},{2,3,6,7},{4,5,6,7}}, EntryMode =>"circuits")
			isWellDefined matroid random(ZZ^3, ZZ^5)
			isWellDefined matroid completeGraph 4
			isWellDefined uniformMatroid(4, 5)
		Text
		
			A theorem of Terai and Trung states that a monomial ideal 
			is the Stanley-Reisner ideal for (the independence complex of) 
			a matroid iff all symbolic powers is Cohen-Macaulay (indeed, 
			this happens iff the 3rd symbolic power is Cohen-Macaulay). 
			We can verify this as follows:
			
		Example
			R = QQ[x_0..x_3]
			I = monomialIdeal(x_0*x_1, x_0*x_2, x_3)
			isWellDefined matroid I
			symbolicCube = intersect apply(irreducibleDecomposition I, P -> P^3)
			(codim symbolicCube, pdim betti res symbolicCube)
			
///

doc ///
	Key
		groundSet
	Headline
		(internal) ground set
	Usage
		M.groundSet
	Inputs
		M:Matroid
	Outputs
		:Set
			of integers starting from 0
	Description
		Text
			Returns the internal representation of the ground set.
			
			Important: read the following if you encounter errors when 
			specifying subsets of a matroid (e.g. 
			restriction/deletion/contraction, rank of subset, etc.)
			
			For a matroid M, there are 2 important differences between 
			M.groundSet and the elements of M (given by 
			@TO2{(symbol _*, Matroid), "M_*"}@). First is data 
			types: M.groundSet is a @TO Set@, and M&#95;* is a @TO List@. 
			Second, M.groundSet always consists of integers from 0 to n-1, 
			where n is the number of elements of M: on the other hand,
			the elements of M themselves can be arbitrary (e.g. symbols, 
			matrices, edges in a graph, etc.).
			
			Thus, one can think of M.groundSet as the
			set of indices of the elements in the list M&#95;*: the first
			element of M has index 0, corresponding to the 
			element 0 in M.groundSet; the second element of M 
			has index 1, etc. 
			
			The key point is that all sets associated to the structure of a 
			matroid - bases, circuits, flats, etc. - are subsets of M.groundSet
			(not M&#95;*). In particular, they are also of class @TO Set@
			(although a collection of them is usually a @TO List@), and 
			are also indexed from 0 to n-1. (An exception here is loops and
			coloops, which are given as a list of indices, rather than
			single-element sets).
			
			A recommended way to circumvent this distinction between indices 
			and elements is to use $\{0, ..., n-1\}$ as the actual elements of M, 
			in which case an element is equal to its index in M.groundSet. Most methods
			in this package will accept either a list of elements or a set of indices,
			and if the elements of M are $\{0, ..., n-1\}$, then functionally there will
			be no difference between inputting lists or sets.
			
			In summary: @TO2{List, "lists"}@ are used for elements in M, and 
			given as sublists of M&#95;*, while @TO2{Set, "sets"}@ are used for 
			indices, and given as subsets of M.groundSet.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			peek M
			M.groundSet
			M_*
			bases M
			(bases M)#0
			circuits M
			flats M
			loops M
			coloops M
		Text
			
			Note in particular the types of the various outputs above.
			
			The following illustrates how to perform operations with a specified
			subset of M.groundSet. In the final example, a list of indices is given, 
			which goes against the conventions above, but a warning is printed,
			and the elements of the list are treated (correctly) as indices.
			
		Example
			N1 = M | {a,c,d}
			N2 = M | set{0,2,3}
			N1 == N2
			N3 = M | {0,2,3} -- gives a warning, but attempts to treat 0 as an index
			N3 == N2
	SeeAlso
		(symbol _, Matroid, List)
///

doc ///
	Key
		(symbol _, Matroid, List)
		(symbol _, Matroid, Set)
		(symbol _, Matroid, ZZ)
		(symbol _*, Matroid)
	Headline
		elements of matroid
	Usage
		M_S
		M_i
		M_*
	Inputs
		M:Matroid
		S:List
			or @TO2{Set, "set"}@, of indices in M.groundSet
	Outputs
		:List
			of elements of M
	Description
		Text
			Converts a list or set of indices to the list of elements of the 
			matroid with those indices. The inverse of this 
			function is @TO indicesOf@.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			M_2
			M_{0,2,3}
			B = (bases M)#0
			M_B
		Text
		
			If used with the operator @TO2{symbol _*, "_*"}@, then the 
			list of all elements in M is returned. This is useful in 
			conjunction with @TO apply@, to iterate over all elements in 
			a matroid.
			
		Example
			F7 = specificMatroids "fano"
			M4 = matroid completeGraph 4
			all(F7_*, x -> #isomorphism(F7 \ {x}, M4) > 0)
	Caveat
		There are important differences between this method and
		@TO groundSet@: see that page for more details.
	SeeAlso
		groundSet
		indicesOf
///

doc ///
	Key
		indicesOf
		(indicesOf, List, List)
		(indicesOf, Matroid, List)
	Headline
		indices of a sublist
	Usage
		indicesOf(E, L)
		indicesOf(M, L)
	Inputs
		E:List
		M:Matroid
		L:List
			a list of sublists of E, or a sublist of M&#95;*
	Outputs
		:List
			of indices
	Description
		Text
			This method has two typical-use cases. The first case is
			to convert sublists of a list E to their corresponding indices. 
			For technical reasons, the accepted input is a list L of 
			sublists of E, and the output is a list of sets of indices, 
			one set for each sublist in L. Note that the order of elements
			in the sublist is lost, when viewed as a set.
			
		Example
			indicesOf(toList(a..z) | toList(0..9), {{m,a,c,a,u,l,a,y,2},{i,s},{f,u,n}})
		Text
		
			The second case is with a matroid as input. Here the ambient 
			list E is taken as the ground set of the matroid (i.e. E = M&#95;*),
			and L should be a list of elements of M (not a list of lists); 
			in this case the inverse of this method is given by 
			@TO2{(symbol _, Matroid, List), "taking subscripts"}@
			with respect to M.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			B = {a, c}
			S = indicesOf(M, B)
			M_S == B
		Text
			
			In this case, if L is not a sublist of M&#95;*, then a warning is 
			printed, and L itself is returned. This is done so that if a user 
			inputs a list of indices, then it will be interpreted as a set of 
			indices. For more on this, cf. @TO groundSet@.
	SeeAlso
		(symbol _, Matroid, List)
///

doc ///
	Key
		(ideal, Matroid)
	Headline
		Stanley-Reisner ideal of matroid
	Usage
		ideal M
	Inputs
		M:Matroid
	Outputs
		:MonomialIdeal
			the Stanley Reisner ideal of the independence complex
	Description
		Text
			The independent sets of a matroid M form a simplicial complex
			(i.e., are downward closed), called the independence complex
			of M. Via the Stanley-Reisner correspondence, the independence
			complex of M corresponds uniquely to a squarefree monomial 
			ideal, which is the output of this method. 
			
			The minimal generators of the ideal correspond to minimal 
			non-faces of the simplicial complex. As the faces of the independence
			complex are precisely the independent sets, the minimal non-faces
			are exactly the minimal dependent sets, i.e. the circuits of M.
			
			The facets of the simplicial complex correspond to bases of M,
			and thus also to irreducible components of the ideal of M; which 
			are in bijection with the minimal generators of the Alexander dual 
			ideal via taking complements.
			
			Internally, the ideal of the matroid is a fundamental complete 
			invariant, and is heavily used in many algorithms in this package.
			Accordingly, once the ideal of a matroid is computed, it is cached in 
			the CacheTable of the matroid, which speeds up any algorithm
			which requires the ideal as part of the input.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			circuits M
			ideal M
			J = dual ideal M
			J_*/indices
			bases M
			
	SeeAlso
		circuits
		bases
		(independenceComplex, Matroid)
///

doc ///
	Key
		bases
		(bases, Matroid)
	Headline
		bases of matroid
	Usage
		bases M
	Inputs
		M:Matroid
	Outputs
		:List
			of bases
	Description
		Text
			Returns a list of bases of the matroid. The basis elements 
			are represented as sets of nonnegative integers, which are 
			the indices of the elements in the ground set that make up 
			a basis element. To get the subset of the ground set 
			corresponding to a set of indices, use 
			@TO2{(symbol _, Matroid, List), "subscripts"}@.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			bases M
			M_((bases M)#0)
		Text
		
			In this package, bases are the only structure that is always 
			computed upon creation of a matroid. Additional invariants 
			(such as the @TO2{(ideal, Matroid), "ideal"}@ or 
			@TO circuits@) are not precomputed, but are cached after 
			being computed once. This allows for maximum speed in 
			methods that need to call the 
			@TO2{matroid, "constructor function"}@ many, many times: 
			e.g. @TO2{(tuttePolynomial, Matroid), "tuttePolynomial"}@
			and @TO hasMinor@.
	SeeAlso
		nonbases
		(independentSets, Matroid)
		(symbol _, Matroid, List)
///

doc ///
	Key
		nonbases
		(nonbases, Matroid)
	Headline
		nonbases of matroid
	Usage
		nonbases M
	Inputs
		M:Matroid
	Outputs
		:List
			of nonbases
	Description
		Text
			In any matroid, all basis elements have the same cardinality 
			r (which is the rank of the matroid). The nonbases of a matroid 
			are the subsets of the ground set of cardinality r that are dependent. 

			Just as with @TO bases@ and @TO circuits@, nonbases are 
			stored via their indices (rather than the elements of the ground 
			set themselves).
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			nonbases M
	SeeAlso
		bases
///

doc ///
	Key
		circuits
		(circuits, Matroid)
	Headline
		circuits of matroid
	Usage
		circuits M
	Inputs
		M:Matroid
	Outputs
		:List
			of circuits
	Description
		Text
			The circuits of a matroid are the minimal dependent 
			subsets of the ground set.

			Just as with @TO bases@ and @TO nonbases@, circuits 
			are stored via their indices (rather than the elements of the 
			ground set themselves).
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			circuits M
	SeeAlso
		fundamentalCircuit
		loops
///

doc ///
	Key
		fundamentalCircuit
		(fundamentalCircuit, Matroid, List, Thing)
		(fundamentalCircuit, Matroid, Set, ZZ)
	Headline
		fundamental circuit of independent set
	Usage
		fundamentalCircuit(M, I, e)
	Inputs
		M:Matroid
		I:Set
			of indices, or a @TO2{List, "list"}@ of elements in M, 
			which is an independent set
		e:ZZ
			an index, or an element in M, such that 
			$I \cup&nbsp;\{e\}$ is dependent
	Outputs
		:Set
			the fundamental circuit of e with respect to I
	Description
		Text
			If I is an independent set I, and e is an element such
			that $I \cup&nbsp;\{e\}$ is dependent (in particular
			e is not in I), then there is a unique circuit contained in 
			$I \cup&nbsp;\{e\}$, called the fundamental circuit of e 
			with respect to I, which moreover contains e.
			Every circuit is the fundamental circuit of some element 
			with respect to some basis.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			circuits M
			fundamentalCircuit(M, {a,c}, b)
			fundamentalCircuit(M, set{0,2}, 1)
			fundamentalCircuit(M, set{0,2}, 3)
		Text
			
			This method does not perform any checks (e.g. 
			whether $I$ is independent, or if $e$ is not in $I$). 
			If $I \cup&nbsp;\{e\}$ is independent, then a warning is
			printed, and @TO null@ is returned. In the example below, 
			the elements with indices 2 and 3 are parallel (indeed, both 
			are equal to the column vector (1, 1)). Thus in general it is 
			safer to refer to a subset by its indices, rather than its elements.
			
		Example
			M = matroid matrix{{1,0,1,1},{0,1,1,1}}
			circuits M
			M_2
			M_2 == M_3
			fundamentalCircuit (M, M_{1,2}, M_3)
			fundamentalCircuit (M, set{1,2}, 3)
	SeeAlso
		circuits
		(independentSets, Matroid)
		isDependent
///

doc ///
	Key
		loops
		(loops, Matroid)
	Headline
		loops of matroid
	Usage
		loops M
	Inputs
		M:Matroid
	Outputs
		:List
			the loops of M
	Description
		Text
			The loops of a matroid are the one-element @TO circuits@.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			loops M
			all(loops M, l -> member(set{l}, circuits M))
			loops(M/(set loops M)) == {}
	SeeAlso
		coloops
		circuits
///

doc ///
	Key
		coloops
		(coloops, Matroid)
	Headline
		coloops of matroid
	Usage
		coloops M
	Inputs
		M:Matroid
	Outputs
		:List
	Description
		Text
			The coloops of a matroid M are the loops of the 
			@TO2{(dual, Matroid), "dual"}@ matroid. 
			The set of coloops of M equals both the intersection of the 
			@TO bases@ of M, and the complement of the union of the 
			@TO circuits@ of M.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			circuits M
			C = set coloops M
			C === M.groundSet - fold(circuits M, (a, b) -> a + b)
			C === fold(bases M, (a, b) -> a*b)
			M_C
			D = dual M; peek D
			coloops matroid completeGraph 4 == {}
	SeeAlso
		loops
		(dual, Matroid)
///

doc ///
	Key
		(independentSets, Matroid)
		(independentSets, Matroid, ZZ)
	Headline
		independent subsets of a matroid
	Usage
		independentSets M
		independentSets(M, s)
	Inputs
		M:Matroid
	Outputs
		:List
			the independent sets in M of size s
	Description
		Text
			A subset of the ground set is called independent if it is 
			contained in a @TO2{bases, "basis"}@, or equivalently, 
			does not contain a @TO2{circuits, "circuit"}@.
			This method returns all independent subsets of the ground 
			set of a fixed size $s$. If no size $s$ is given, returns a list of 
			all independent sets of M.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			independentSets(M, 2)
			netList independentSets M
			V = specificMatroids "vamos"
			I3 = independentSets(V, 3)
			#I3
	SeeAlso
		bases
		isDependent
		(independenceComplex, Matroid)
///

doc ///
	Key
		isDependent
		(isDependent, Matroid, Set)
		(isDependent, Matroid, List)
	Headline
		whether a subset is dependent
	Usage
		isDependent(M, S)
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:Boolean
			whether S is dependent in M
	Description
		Text
			This method checks if the given subset of the ground set
			is dependent, i.e. contains a @TO2{circuits, "circuit"}@.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			isDependent(M, {a,b})
			isDependent(M, {d})
	SeeAlso
		(independentSets, Matroid)
///

doc ///
	Key
		(rank, Matroid)
	Headline
		rank of a matroid
	Usage
		rank M
	Inputs
		M:Matroid
	Outputs
		:ZZ
	Description
		Text
			The rank of a matroid is the common size of a(ny) 
			basis of M. This is a basic numerical invariant of a matroid.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			rank M
	SeeAlso
		(rank, Matroid, Set)
///

doc ///
	Key
		(rank, Matroid, Set)
		(rank, Matroid, List)
	Headline
		rank of a subset of a matroid
	Usage
		rank(M, S)
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:ZZ
	Description
		Text
			The rank of a subset S of a matroid is the size of a maximal 
			independent subset of S. The map 2^E $\to \mathbb{N}$, 
			S $\mapsto$ rank(S), is called the rank function, and 
			completely determines the matroid.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			for s in subsets M_* do print(toString s | " has rank " | rank_M s)
	SeeAlso
		(rank, Matroid)
///

doc ///
	Key
		closure
		(closure, Matroid, List)
		(closure, Matroid, Set)
	Headline
		closure of a subset of a matroid
	Usage
		closure(M, S)
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:List
	Description
		Text
			The closure of a subset S of a matroid (E, B) is the set 
			cl(S) := \{x $\in$ E : rank(S) = rank(S $\cup$ \{x\}) \}. 
			The closure operator 2^E -> 2^E, S $\mapsto$ cl(S), 
			completely determines the matroid (indeed, the maximal proper
			closed sets - i.e. @TO2{(hyperplanes, Matroid), "hyperplanes"}@ -
			already determine the matroid).
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			for s in subsets M_* do print(toString s | " has closure " | toString closure_M s)
			F = flats M
			all(F, f -> closure_M f === f)
	SeeAlso
		flats
///

doc ///
	Key
		flats
		(flats, Matroid, ZZ)
		(flats, Matroid)
	Headline
		flats of a matroid
	Usage
		flats(M, r)
		flats M
	Inputs
		M:Matroid
		r:ZZ
			the target rank (optional)
	Outputs
		:List
	Description
		Text
			A flat, or closed subset, of a matroid is a subset A of 
			the ground set which equals its @TO closure@. The 
			set of flats, partially ordered by inclusion, forms a lattice, 
			called the @TO latticeOfFlats@.
			This is an important invariant of the matroid: one can
			recover the matroid from the lattice of flats, and for
			simple matroids (i.e. matroids whose circuits all have size
			>= 3), the isomorphism type of the lattice is already a
			complete invariant.
			
			If a target rank r is provided, then this method computes 
			closures of independent sets of size r. This may be slower 
			than simply computing all flats, and then selecting those
			of rank r.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			flats(M, 1)
			netList flats M
		
		Text
			If no target rank is provided, this method computes flats by 
			iteratively intersecting 
			@TO2{(hyperplanes, Matroid), "hyperplanes"}@ of 
			M: every flat of corank k (i.e. of rank = rank M - k) 
			can be expressed as an intersection of k hyperplanes 
			(cf. Oxley, Prop. 1.7.8). Thus if hyperplanes of M have 
			been precomputed, then this function is typically much 
			faster.
			
		CannedExample
			i4 : M = matroid completeGraph 7

			o4 = a matroid of rank 6 on 21 elements
			
			o4 : Matroid
			
			i5 : time #hyperplanes M
			     ‐‐ used 6.47885 seconds
			
			o5 = 63
			
			i6 : time #flats M
			     ‐‐ used 0.939786 seconds
			
			o6 = 877
	SeeAlso
		closure
		(hyperplanes, Matroid)
		(fVector, Matroid)
		latticeOfFlats
///

doc ///
	Key
		hyperplanes
		(hyperplanes, Matroid)
	Headline
		hyperplanes of a matroid
	Usage
		hyperplanes M
	Inputs
		M:Matroid
	Outputs
		:List
	Description
		Text
			The hyperplanes of a matroid are the flats of rank 
			equal to rank M - 1. The complements of the hyperplanes 
			are precisely the @TO circuits@ of the 
			@TO2{(dual, Matroid), "dual"}@ matroid (which is indeed 
			how this method computes hyperplanes), and thus 
			a matroid is determined by its hyperplanes.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			hyperplanes M
	SeeAlso
		flats
		(rank, Matroid)
///

doc ///
	Key
		latticeOfFlats
		(latticeOfFlats, Matroid)
	Headline
		lattice of flats of a matroid
	Usage
		latticeOfFlats M
	Inputs
		M:Matroid
	Outputs
		:Poset
	Description
		Text
			The lattice of flats of a matroid M is the set of flats of M, 
			partially ordered by containment; i.e. F1 <= F2 if F1 is contained 
			in F2. The lattice of flats of a matroid is a geometric lattice: i.e. 
			it is atomic (every element is a join of atoms = rank 1 elements) 
			and semimodular (h(x) + h(y) >= h(x &or; y) + h(x &and; y) for 
			any x, y, where h is the height function = maximum length of 
			a chain from 0). Conversely, every geometric lattice is the 
			lattice of flats of a matroid.
			
			If M1 and M2 are @TO2{(isSimple, Matroid), "simple matroids"}@
			(i.e. no loops or parallel classes) with isomorphic lattice of 
			flats, then M1 and M2 are isomorphic.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			latticeOfFlats M
		Text
		
			One can also view the lattice of flats, using @TO displayPoset@
			provided by the @TO Posets@ package (together with the option
			@TO SuppressLabels@ => false).
	SeeAlso
		flats
		(rank, Matroid)
		(fVector, Matroid)
///

doc ///
	Key
		(fVector, Matroid)
	Headline
		f-vector of a matroid
	Usage
		fVector M
	Inputs
		M:Matroid
	Outputs
		:HashTable
	Description
		Text
			The f-vector of a matroid M is the invariant (f_0, f_1, ..., f_r), 
			where f_i is the number of @TO2{(rank, Matroid, Set), "rank"}@ 
			i @TO flats@ of M, and r is the @TO2{(rank, Matroid), "rank"}@ 
			of M. Note that f_0 = f_r = 1, as the set of @TO loops@ 
			is the unique flat of rank 0, and the ground set is the unique
			flat of maximal rank.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			fVector M
			fVector matroid completeGraph 4
	Caveat
		This is not the same as the f-vector of the 
		@TO2{(independenceComplex, Matroid), "independence complex"}@
		of the matroid M, which counts the number of independent sets of 
		a given size. To do this instead, use "fVector independenceComplex M".
	SeeAlso
		flats
		(rank, Matroid)
		latticeOfFlats
///

doc ///
	Key
		(dual, Matroid)
	Headline
		dual matroid
	Usage
		dual M
	Inputs
		M:Matroid
	Outputs
		:Matroid
			the dual matroid of M
	Description
		Text
			The dual matroid of a matroid M has the same ground set 
			as M, and bases equal to the complements of bases of M.
			
			Duality is a fundamental operation in matroid theory: 
			for nearly any property/operation of matroids, there
			is a corresponding dual version, usually denoted with the
			prefix "co-". For instance, coloops are loops of the dual,
			and contraction is dual to deletion.
			
			In this package, every dual matroid is created as a 
			matroid-dual matroid pair, and each is cached as the dual
			of the other. Often the ideal of the dual matroid has a 
			significantly different number of generators, so many 
			algorithms in this package will use an equivalent check 
			for the ideal with fewer generators.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			D = dual M
			peek D
			M == dual D
			loops D == coloops M
			hyperplanes M === apply(circuits D, C -> D.groundSet - C)
		Text
			
			A matroid that is 
			@TO2{(areIsomorphic, Matroid, Matroid), "isomorphic"}@ 
			to its dual is called self-dual; and a matroid that is 
			@TO2{(symbol ==, Matroid, Matroid), "equal"}@ to its 
			dual is called identically self-dual.
			
		Example
			V8plus = specificMatroids "V8+"
			V8plus == dual V8plus
			V = relaxation(V8plus, set{4,5,6,7})
			V == dual V
			areIsomorphic(V, dual V)
///

doc ///
	Key
		restriction
		(restriction, Matroid, Set)
		(restriction, Matroid, List)
		(symbol |, Matroid, Set)
		(symbol |, Matroid, List)
	Headline
		restriction to subset of matroid
	Usage
		restriction(M, S)
		M | S
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:Matroid
			the restriction M | S
	Description
		Text
			The restriction of M to S, M | S, has ground set S and 
			independent sets equal to the independent sets of M 
			contained in S.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			N = M | {a,b,d}
			peek N
			N == M | set{0,1,3}
	SeeAlso
		deletion
		minor
///

doc ///
	Key
		deletion
		(deletion, Matroid, Set)
		(deletion, Matroid, List)
		(symbol \, Matroid, Set)
		(symbol \, Matroid, List)
	Headline
		deletion of subset of matroid
	Usage
		deletion(M, S)
		M \ S
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:Matroid
			the deletion M &#92; S
	Description
		Text
			The deletion M &#92; S is obtained by restricting to the complement of S.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			N = M \ {a}
			peek N
			N == M \ set{0}
	SeeAlso
		restriction
		contraction
		minor
///

doc ///
	Key
		contraction
		(contraction, Matroid, Set)
		(contraction, Matroid, List)
		(symbol /, Matroid, Set)
		(symbol /, Matroid, List)
	Headline
		contraction of subset of matroid
	Usage
		contraction(M, S)
		M / S
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
	Outputs
		:Matroid
			the contraction M / S
	Description
		Text
			The contraction of M by S is given by M/S := (M* &#92; S)*, 
			where * stands for dual, and &#92; is deletion.
			
		Example
			M = matroid({a,b,c,d},{{a,b},{a,c}})
			N = M / {c}
			peek N
			N == M / set{2}
	SeeAlso
		deletion
		(dual, Matroid)
		minor
///

doc ///
	Key
		minor
		(minor, Matroid, List, List)
		(minor, Matroid, Set, Set)
	Headline
		minor of matroid
	Usage
		minor(M, X, Y)
	Inputs
		M:Matroid
		X:Set
			of indices, or a @TO2{List, "list"}@ of elements in M
		Y:Set
			of indices, or a @TO2{List, "list"}@ of elements in M, disjoint from X
	Outputs
		:Matroid
			the minor M / X &#92; Y
	Description
		Text
			The minor M / X &#92; Y of M is given by contracting X and 
			deleting Y from M. The resulting matroid is independent 
			of the order in which deletion and contraction is done. If 
			X (or Y) is a set (of indices in M.groundSet), then X is identified 
			with the sublist of elements of M with indices in X: 
			cf. @TO groundSet@ for more on this package-wide convention.
			
		Example
			M = matroid random(ZZ^3,ZZ^6)
			M_*
			M.groundSet
			(X, Y) = (set{3}, set{0,1})
			(X1, Y1) = (M_X, M_Y)
			N = minor(M, X, Y)
			peek N
			N == minor(M, X1, Y1)
		Text	
			
			Note that there is potential ambiguity for the second argument - 
			namely, whether or not Y is treated with respect to the ground set 
			of M or M / X (which are different). This method assumes that the 
			indices of Y (and X) are taken with respect to the ground set of M. 
			
			If one already has the indices Y0 of Y in M / X (or the indices X0 of 
			X in M &#92; Y), one can simply use the notation M 
			@TO2{contraction, "/"}@ X @TO2{deletion, "\\"}@ Y0 
			(or (M &#92; Y) / X0). Thus this method serves purely as a 
			convenience, to save the user the (trivial) task of computing Y0 from Y.
			
			If X and Y are not disjoint, then an error is thrown (thus one should
			@TO2{(symbol -, Set, Set), "subtract"}@ X from Y beforehand).
			
		Example
			M5 = matroid completeGraph 5
			M5.groundSet
			N = minor(M5, set{8}, set{3,4,9})
			areIsomorphic(N, matroid completeGraph 4)
			N == (M5 \ set{3,4,9}) / set{6} -- after deleting 3,4 (and 9), index 8 -> 6
			N == M5 / set{8} \ set{3,4,8} -- after contracting 8, index 9 -> 8
			(try minor(M5, set{8}, set{3,4,8,9})) === null
			minor(M5, set{8}, set{3,4,8,9} - set{8})
	SeeAlso
		deletion
		contraction
		hasMinor
///

doc ///
	Key
		hasMinor
		(hasMinor, Matroid, Matroid)
	Headline
		whether a matroid has a given minor
	Usage
		hasMinor(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Boolean
			whether N is a minor of M
	Description
		Text
			Determines if N is a minor of M, i.e. can be obtained 
			from M by a contraction followed by a deletion. Since 
			deletion and contraction by disjoint subsets commute, 
			every sequence of deletion and contraction operations 
			can be written as a single contraction and deletion.
			
			Many families of matroids can be defined by a 
			list of forbidden minors: i.e. a matroid M is in the family
			iff M does not have any of the forbidden minors as a minor. 
			For instance, a matroid is representable over F_2
			iff it does not have U_{2,4} as a minor, i.e. U_{2,4} is 
			the (sole) forbidden minor for binary matroids.

			If a minor is found that is 
			@TO2{(areIsomorphic, Matroid, Matroid), "isomorphic"}@ 
			to N, then the sets to be contracted and deleted are printed.
			
		Example
			(M4, M5, M6) = (4,5,6)/completeGraph/matroid
			hasMinor(M4, uniformMatroid(2,4))
			time hasMinor(M6, M5)
	SeeAlso
		minor
///

doc ///
	Key
		relaxation
		(relaxation, Matroid, Set)
		(relaxation, Matroid, List)
	Headline
		relaxation of matroid
	Usage
		relaxation(M, S)
	Inputs
		M:Matroid
		S:Set
			of indices, or a @TO2{List, "list"}@ of elements in M,
			which is a circuit-hyperplane of M
	Outputs
		:Matroid
			the relaxation of M by S
	Description
		Text
			Let M = (E, B) be a matroid with bases B. If there is a 
			subset S of E that is both a @TO2{circuits, "circuit"}@ 
			and a @TO2{(hyperplanes, Matroid), "hyperplane"}@ of 
			M, then the set $B \cup&nbsp;\{S\}$ is the set of bases
			of a matroid on E, called the relaxation of M by S.
			
			Many interesting matroids arise as relaxations of other
			matroids: e.g. the non-Fano matroid is a relaxation of the
			@TO2{specificMatroids, "Fano matroid"}@, and the 
			non-Pappus matroid is a relaxation of the Pappus matroid.
			
		Example
			P = specificMatroids "pappus"
			NP = specificMatroids "nonpappus"
			NP == relaxation(P, set{6,7,8})
	Caveat
		Note that relaxation does not change the ground set. Thus e.g.
		@TO representationOf@ will return the same for both the Fano and
		non-Fano matroids.
///

doc ///
	Key
		(symbol +, Matroid, Matroid)
	Headline
		union of matroids
	Usage
		M + N
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Matroid
			the sum, or union, of M and N
	Description
		Text
			The union of M and N has ground set equal to the 
			union of those of M and N, and independent sets given
			by pairwise unions of independent sets of M and N.
			
		Example
			M = uniformMatroid(2,4) + uniformMatroid(1,4)
			peek M
			M == uniformMatroid(3, 4)
		Text
			
			When the ground sets of M and N are disjoint, this is
			the @TO2{(symbol ++, Matroid, Matroid), "direct sum"}@
			of M and N. Beware of order when using 
			@TO2{(symbol ==, Matroid, Matroid), "=="}@ though:
			
		Example
			M1 = uniformMatroid(2, 4) + matroid completeGraph 4
			M1 == uniformMatroid(2, 4) ++ matroid completeGraph 4
			M2 = matroid completeGraph 4 ++ uniformMatroid(2, 4)
			M1 == M2
			areIsomorphic(M1, M2)
		Text
			
			Matroid union is an important operation in combinatorial optimization,
			and via duality, is related to the problem of matroid intersection.
			
			With the operation of union, one can work with transversal
			matroids and gammoids. A matroid is transversal iff it is a union of 
			rank 1 matroids; strict gammoids are precisely the duals of transversal 
			matroids, and gammoids are restrictions of strict gammoids. In general 
			the problem of determining if a given matroid is a gammoid is difficult.
			
			A union of two uniform matroids is again uniform, but a union 
			of two graphic matroids need not be binary:
			
		Example
			M1 = matroid({a,b,c,d}, {{a},{b},{c}})
			M2 = matroid({a,b,c,d}, {{b},{c},{d}})
			M1 + M2 == uniformMatroid(2,4)
			F7 = specificMatroids "fano"
			NF = specificMatroids "nonfano"
			all({F7 + NF, F7 + F7, NF + NF}, M -> M == uniformMatroid(6, 7))
		Text
		
			One potential caveat: the ground set of M must not have repeated 
			elements. If this is not the case, the user MUST relabel elements of 
			M so that they become distinct. Of course, this needs to be done
			for both M and N, and one should also keep track of which 
			elements of M and N are meant to be the same after the relabelling 
			(otherwise the entire point of taking unions, as opposed to direct 
			sums, is lost).
			
			In the example below, M contains the vector {1,1} twice. Macaulay2 
			has no way of distinguishing the repeated vectors, so the second 
			occurrence of {1,1} is relabelled to the symbol d (of course,
			if the symbol d also happened to be an element of N, then a different 
			label would have to be chosen).
			
		Example
			A = matrix{{0,1,1,1},{0,0,1,1}}
			M = matroid A
			M_*
			unique M_*
			M1 = matroid(M_{0,1,2} | {d}, bases M)
			M == M1
			B = matrix{{0,1,2},{0,1,2}}
			N = matroid B
			U = M1 + N
			peek U
			U_*
	SeeAlso
		(symbol ++, Matroid, Matroid)
///

doc ///
	Key
		(symbol ++, Matroid, Matroid)
	Headline
		direct sum of matroids
	Usage
		M ++ N
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Matroid
			the direct sum of M and N
	Description
		Text
			The direct sum of M and N has ground set equal to the 
			disjoint union of those of M and N, and bases equal to 
			the union of bases of M and N.
			
		Example
			S = uniformMatroid(2,3) ++ uniformMatroid(1,3)
			peek S
			S_*
			(S ++ uniformMatroid(1, 3))_*
	Caveat
		The elements of the ground set of the direct sum will receive
		placeholders to ensure disjointness (as evidenced by the 
		elements of S being ordered pairs above). As this method is 
		binary, repeated applications of this function will result in nested 
		placeholders. Since the bases are stored as indices, the 
		bases of M will not change, but those of N will be shifted up by 
		the size of the ground set of M.
	SeeAlso
		(symbol +, Matroid, Matroid)
		(components, Matroid)
///

doc ///
	Key
		(components, Matroid)
	Headline
		connected components of matroid
	Usage
		components M
	Inputs
		M:Matroid
	Outputs
		:List
			the connected components of M
	Description
		Text
			Define an equivalence relation ~ on the ground set of M 
			by e ~ f if e = f or $\{e,f\}$ is contained in a circuit. The 
			equivalence classes under ~ are the connected components 
			of M. A matroid is the direct sum of its connected components.
			
		Example
			M = matroid graph({{0,1},{0,2},{1,2},{3,4},{4,5}})
			C = components M
			areIsomorphic(M, fold(C, (a, b) -> a ++ b))
			G = graph({{0,1},{0,2},{0,3},{0,4},{1,2},{3,4}})
			isConnected G
			components matroid G
	Caveat
		As the examples above show, the connected components of 
		the graphic matroid M(G) need not be the same as the connected 
		components of the graph G (indeed, for any graph G, there exists 
		a connected graph H with M(G) isomorphic to M(H)).
	SeeAlso
		circuits
		(symbol ++, Matroid, Matroid)
///

doc ///
	Key
		representationOf
		(representationOf, Matroid)
	Headline
		represents a realizable/graphic matroid
	Usage
		representationOf M
	Inputs
		M:Matroid
	Outputs
		:Thing
			a representation of M (either a matrix or a graph)
	Description
		Text
			Given a realizable matroid whose ground set elements are 
			vectors, returns the matrix with those column vectors. If the 
			matroid is graphical, then the graph with edge set equal to 
			the ground set is returned (without loops or parallel edges,
			since the Graphs package does not support these).
			
		Example
			A = random(ZZ^3,ZZ^5)
			M = matroid A
			A == representationOf M
			K4 = completeGraph 4
			M4 = matroid K4
			representationOf M4 === K4
	SeeAlso
		matroid
		affineGeometry
		projectiveGeometry
		specificMatroids
///

doc ///
	Key
		(isomorphism, Matroid, Matroid)
	Headline
		isomorphisms between two matroids
	Usage
		isomorphism(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:List
			of all isomorphisms between M and N
	Description
		Text
			This method computes all 
			@TO2{(areIsomorphic, Matroid, Matroid), "isomorphisms"}@ 
			between M and N: in particular, this method returns an 
			empty list iff M and N are not isomorphic. 
			
		Example
			M = matroid({a,b,c},{{a,b},{a,c}})
			isomorphism(M, uniformMatroid(2,3)) -- not isomorphic
			(M5, M6) = (5,6)/completeGraph/matroid
			minorM6 = minor(M6, set{8}, set{4,5,6,7})
			time #isomorphism(M5, minorM6)
		Text
			
			We can verify that the Fano matroid (which is the projective plane
			over the field of two elements) has automorphism group of order
			168 (and even give an explicit permutation representation for this
			group PGL(3, F_2) inside the symmetric group S_7):
			
		Example
			F7 = specificMatroids "fano"
			time autF7 = isomorphism(F7, F7); -- output is a list of permutations
			#autF7
	SeeAlso
		quickIsomorphismTest
		(areIsomorphic, Matroid, Matroid)
///

doc ///
	Key
		quickIsomorphismTest
		(quickIsomorphismTest, Matroid, Matroid)
	Headline
		quick checks for isomorphism between matroids
	Usage
		quickIsomorphismTest(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:String
			either "true" or "false" or "Could be isomorphic"
	Description
		Text
			This method performs relatively quick tests to determine 
			whether or not two matroids are isomorphic. A result of "false"
			is definitive proof that the matroids are not isomorphic, a result
			of "true" is definitive proof that the matroids are isomorphic,
			and a result of "Could be isomorphic" is strong evidence that 
			the matroids may be isomorphic.
			
			If "true" or "false" is returned, use @TO value@ to convert to a
			@TO Boolean@.
			
		Example
			M1 = matroid(toList(a..z)/toString,{{"m","a","t","r","o","i","d"}})
			M2 = matroid(toList(0..25), {{random(ZZ),23,15,12,19,20,11}})
			quickIsomorphismTest(M1, M2)
			quickIsomorphismTest(matroid random(ZZ^5,ZZ^8), uniformMatroid(5, 8))
			quickIsomorphismTest(uniformMatroid(5, 9), uniformMatroid(4, 9))
			M1 = matroid graph({{a,b},{b,c},{c,d},{d,e},{e,f},{f,g},{f,h},{c,h},{c,f},{a,g},{d,g}})
			M2 = matroid graph({{a,b},{b,c},{c,d},{d,e},{e,f},{f,g},{f,h},{c,h},{c,f},{a,g},{a,h}})
			R = ZZ[x,y]; tuttePolynomial(M1, R) == tuttePolynomial(M2, R)
			time quickIsomorphismTest(M1, M2)
			value oo === false
	SeeAlso
		(isomorphism, Matroid, Matroid)
		(areIsomorphic, Matroid, Matroid)
		
///

doc ///
	Key
		(areIsomorphic, Matroid, Matroid)
	Headline
		whether two matroids are isomorphic
	Usage
		areIsomorphic(M, N)
	Inputs
		M:Matroid
		N:Matroid
	Outputs
		:Boolean
			true if the matroids are isomorphic, false otherwise
	Description
		Text
			Two matroids are isomorphic if there is a bijection between 
			their ground sets which induces a bijection between bases,
			or equivalently, circuits (of which there are often fewer than bases). 
			
			This method first runs @TO quickIsomorphismTest@,
			then @TO2{(isomorphism, Matroid, Matroid), "isomorphism"}@ 
			if the tests are inconclusive.
			
		Example
			M = matroid({a,b,c},{{a,b},{a,c},{b,c}})
			areIsomorphic(M, uniformMatroid(2,3))
			M1 = matroid({a,b,c},{{a,b},{a,c}})
			areIsomorphic(M, M1)
	Caveat
		Isomorphism of matroids should not be confused with equality: cf.
		@TO2{(symbol ==, Matroid, Matroid), "=="}@ for more details.
	SeeAlso
		(isomorphism, Matroid, Matroid)
		quickIsomorphismTest
///

doc ///
	Key
		(tuttePolynomial, Matroid)
		(tuttePolynomial, Matroid, Ring)
	Headline
		Tutte polynomial of a matroid
	Usage
		tuttePolynomial M
	Inputs
		M:Matroid
	Outputs
		:RingElement
			the Tutte polynomial of M
	Description
		Text
			The Tutte polynomial is an invariant of a matroid that is 
			universal with respect to satisfying a deletion-contraction 
			recurrence. Indeed, one way to define the Tutte polynomial 
			of a matroid is: if $M$ is a matroid consisting of $a$ loops 
			and $b$ coloops, then T_M(x, y) = x^ay^b, and if 
			$e \in&nbsp;M$ is neither a loop nor a coloop, then 
			T_M(x, y) := T_{M/e}(x, y) + T_{M&#92;e}(x, y), where 
			M&#92;e is the @TO deletion@ of M with respect to \{e\},
			and M/e is the @TO contraction@ of M with respect to 
			\{e\}. Many invariants of a matroid can be determined by 
			substituting values into its Tutte polynomial - cf.
			@TO tutteEvaluate@.
			
		Example
			tuttePolynomial matroid completeGraph 4
			tuttePolynomial specificMatroids "nonpappus"
	SeeAlso
		tutteEvaluate
		(characteristicPolynomial, Matroid)
		chromaticPolynomial
///

doc ///
	Key
		tutteEvaluate
		(tutteEvaluate, Matroid, Thing, Thing)
	Headline
		evaluate Tutte polynomial
	Usage
		tutteEvaluate(M, a, b)
	Inputs
		M:Matroid
		a:Thing
		b:Thing
	Outputs
		:Thing
			the evaluation of the Tutte polynomial of M at (a, b)
	Description
		Text
			Provides a user-friendly method to evaluate the Tutte 
			polynomial at given values (i.e. calls @TO sub@ with the 
			correct variables in the ring of the Tutte polynomial). 
			For example, if M has Tutte polynomial T, then 
			T(1,1) is the number of bases of M.
			
		Example
			M = uniformMatroid(2, 4)
			tutteEvaluate(M, 1, 1)
		Text
		
			If M = M(G) is the graphic matroid of a graph G, then 
			T(2, 1) counts the number of spanning forests of G,
			and T(2, 0) counts the number of acyclic orientations
			of G.
			
		Example
			M = matroid completeGraph 5
			tutteEvaluate(M, 2, 1)
			tutteEvaluate(M, 2, 0)
	SeeAlso
		(tuttePolynomial, Matroid)
///

doc ///
	Key
		(characteristicPolynomial, Matroid)
	Headline
		computes characteristic polynomial of a matroid
	Usage
		characteristicPolynomial M
	Inputs
		M:Matroid
	Outputs
		:RingElement
			the characteristic polynomial of M
	Description
		Text
			The characteristic polynomial is a particular specialization 
			of the Tutte polynomial. If M is a matroid of rank r with Tutte 
			polynomial T(x, y), then the characteristic polynomial of M 
			is given by (-1)^r * T(1 - x, 0).

			This function computes the characteristic polynomial as an 
			evaluation of the Tutte polynomial. If the Tutte polynomial of 
			the matroid has already been computed, then this function 
			should return the characteristic polynomial instantaneously.
			
		Example
			M = matroid completeGraph 4
			T = tuttePolynomial M
			factor characteristicPolynomial M
			
	Caveat
		if M = M(G) is a graphic matroid, then the characteristic polynomial 
		of M and the chromatic polynomial of G differ by a factor of x^k, 
		where k is the number of connected components of the graph G.
	SeeAlso
		(tuttePolynomial, Matroid)
		chromaticPolynomial
///

doc ///
	Key
		chromaticPolynomial
		(chromaticPolynomial, Graph)
	Headline
		computes chromatic polynomial of a graph
	Usage
		chromaticPolynomial G
	Inputs
		G:Graph
	Outputs
		:RingElement
			the chromatic polynomial of G
	Description
		Text
			The chromatic polynomial is an invariant of a graph 
			that counts the number of vertex colorings. The value 
			of this polynomial at a natural number n is the number 
			of ways to color the vertices of G using at most n colors, 
			such that no adjacent vertices have the same color. 

			This method computes the chromatic polynomial as a 
			multiple of the characteristic polynomial of the graphic matroid.
			
		Example
			factor chromaticPolynomial cycleGraph 7
		Text
			
			The Four Color Theorem states that if G is a 
			planar graph, then its chromatic polynomial has 
			value > 0 at n = 4. In accordance with this, we see that
			K5 is not planar (on the other hand, note that K_{3,3} is 
			bipartite, hence 2-colorable):
		
		Example
			factor chromaticPolynomial completeGraph 5
	SeeAlso
		(tuttePolynomial, Matroid)
		(characteristicPolynomial, Matroid)
///

doc ///
	Key
		(isSimple, Matroid)
	Headline
		whether a matroid is simple
	Usage
		isSimple M
	Inputs
		M:Matroid
	Outputs
		:Boolean
			whether M is a simple matroid
	Description
		Text
			A matroid is simple if it has no 
			@TO loops@ or parallel classes;
			equivalently, it has no 
			@TO circuits@ of size <= 2.
			
			Among the class of simple matroids, the 
			@TO2{latticeOfFlats, "lattice of flats"}@
			is a complete invariant. Every matroid has 
			a unique @TO2{simpleMatroid, "simplification"}@
			which has the same lattice of flats.
			
		Example
			isSimple matroid completeGraph 3
			M = matroid(completeGraph 3, ParallelEdges => {set{0,1},set{0,1},set{1,2}}, Loops => {0,2})
			isSimple M
			S = simpleMatroid M
			isSimple S
			latticeOfFlats M == latticeOfFlats S
		Text
		
			Note that the @TO2{(dual, Matroid), "dual"}@ 
			of a simple matroid may not be simple:
			
		Example
			U = uniformMatroid(2, 2)
			isSimple U
			isSimple dual U
	SeeAlso
		simpleMatroid
///

doc ///
	Key
		simpleMatroid
		(simpleMatroid, Matroid)
	Headline
		simple matroid associated to a matroid
	Usage
		S = simpleMatroid M
	Inputs
		M:Matroid
	Outputs
		:Matroid
			the simple matroid associated to M
	Description
		Text
			The simple matroid associated to a matroid M
			is obtained from M by deleting all @TO loops@, 
			and all but one element from each parallel class.
			
			In a simple matroid, the 
			@TO2{latticeOfFlats, "lattice of flats"}@ has the
			empty set as minimal element, and all atoms
			are singletons.
			
		Example
			M = uniformMatroid(0, 2) ++ uniformMatroid(1, 2) ++ uniformMatroid(2, 4)
			isSimple M
			S = simpleMatroid M
			latticeOfFlats M == latticeOfFlats S
			select(flats S, f -> rank(S, f) <= 1)
	SeeAlso
		(isSimple, Matroid)
///

doc ///
	Key
		uniformMatroid
		(uniformMatroid, ZZ, ZZ)
	Headline
		uniform matroid
	Usage
		U = uniformMatroid(k, n)
	Inputs
		k:ZZ
		n:ZZ
	Outputs
		:Matroid
			the uniform matroid of rank k on n elements
	Description
		Text
			The uniform matroid of rank k has as bases all 
			size k subsets. The ground set is $\{0, ..., n-1\}$.
			
		Example
			U35 = uniformMatroid(3,5)
			peek U35
///

doc ///
	Key
		affineGeometry
		(affineGeometry, ZZ, ZZ)
	Headline
		affine geometry of rank n+1 over F_p
	Usage
		M = affineGeometry(n, p)
	Inputs
		n:ZZ
			the dimension of the ambient vector space
		p:ZZ
			a prime
	Outputs
		:Matroid
			the affine geometry of rank n+1 over F_p
	Description
		Text
			The affine geometry of rank n+1 over F_p is the matroid
			whose ground set consists of all vectors in a vector 
			space over F_p of dimension n, where independence 
			is given by affine independence, i.e. vectors are dependent 
			iff there is a linear combination equaling zero in which 
			the coefficients sum to zero (equivalently, the vectors
			are placed in the hyperplane x_0 = 1 in a vector space
			of dimension n+1, with ordinary linear
			independence in the larger space). 
			
		Example
			M = affineGeometry(3, 2)
			M === specificMatroids "AG32"
			circuits M
			representationOf M
	SeeAlso
		projectiveGeometry
///

doc ///
	Key
		projectiveGeometry
		(projectiveGeometry, ZZ, ZZ)
	Headline
		projective geometry of dimension n over F_p
	Usage
		M = projectiveGeometry(n, p)
	Inputs
		n:ZZ
			the dimension of the projective space
		p:ZZ
			a prime
	Outputs
		:Matroid
			the projective geometry of dimension n over F_p
	Description
		Text
			The projective geometry of dimension n over F_p is the 
			matroid whose ground set consists of points in an 
			n-dimensional projective space over F_p. The matroid
			structure is precisely the 
			@TO2{simpleMatroid, "simple matroid"}@ associated
			to the realizable matroid of (F_p)^(n+1) (i.e. all vectors in an 
			(n+1)-dimensional vector space over F_p) - the origin
			(being a loop) has been removed, and a representative
			is chosen for all parallel classes (= lines).
			
			Note that projective space has a stratification into affine
			spaces (one of each smaller dimension). In particular,
			deleting any hyperplane from PG(n, p) gives AG(n, p).
			
		Example
			PG22 = projectiveGeometry(2, 2)
			PG22 == specificMatroids "fano"
			A = transpose sub(matrix toList(((3:0)..(3:2-1))/toList), ZZ/2) -- all vectors in (ZZ/2)^3
			areIsomorphic(PG22, simpleMatroid matroid A)
			PG32 = projectiveGeometry(3, 2)
			representationOf PG32
			H = first hyperplanes PG32
			areIsomorphic(affineGeometry(3, 2), PG32 \ H)
	SeeAlso
		affineGeometry
///

doc ///
	Key
		getCycles
		(getCycles, Graph)
	Headline
		find cycles of graph
	Usage
		getCycles(G)
	Inputs
		G:Graph
	Outputs
		:List
			a list of (simple) cycles of G
	Description
		Text
			A cycle of G is a connected, 2-regular subgraph of 
			G (i.e. every vertex has degree 2). This method 
			returns all cycles of length at least 3, as ordered 
			lists of vertices (for cycles of length 1 or 2, see 
			the options Loops and ParallelEdges at @TO matroid@). 
			This method is used to create the graphic matroid:
			the output is in bijection with the circuits of the graphic 
			matroid (excluding loops and parallel edges).
			
		Example
			getCycles completeGraph 4
///

doc ///
	Key
		basisIndicatorMatrix
		(basisIndicatorMatrix, Matroid)
	Headline
		matrix of basis polytope
	Usage
		basisIndicatorMatrix M
	Inputs
		M:Matroid
	Outputs
		:Matrix
	Description
		Text
			The matroid (basis) polytope of a matroid on n elements lives in 
			R^n, and is the convex hull of the indicator vectors of the bases.
			
			For uniform matroids, the basis polytope is precisely the hypersimplex:
		
		Example
			U24 = uniformMatroid(2, 4)
			A = basisIndicatorMatrix U24
		Text
		
			In order to obtain an actual polytope, one must take the convex
			hull of the columns of the indicator matrix, which is provided by the
			Polyhedra package:
			
		Example
			needsPackage "Polyhedra"
			P = convexHull A
			vertices P
		Text

			The Gelfand-Goresky-MacPherson-Serganova 
			characterizes which polytopes are basis polytopes for a matroid: 
			namely, each edge is of the form $e_i - e_j$ for some $i, j$, where 
			$e_i$ are the standard basis vectors.
			
		-- Example
			-- M = matroid({{0,1},{0,2},{0,3},{1,2},{2,3}})
			-- n = #M.groundSet
			-- P = polytope M
			-- E = Polyhedra$faces(n - 2, P)/Polyhedra$vertices -- edges of P
			-- all(E, e -> sort flatten entries(e_0 - e_1) == ({-1} | toList(n-2:0) | {1})) -- GGMS criterion
	SeeAlso
		bases
		(independenceComplex, Matroid)
///

doc ///
	Key
		(independenceComplex, Matroid)
	Headline
		independence complex of matroid
	Usage
		independenceComplex M
	Inputs
		M:Matroid
	Outputs
		:SimplicialComplex
	Description
		Text
			The independence complex of a matroid is the simplicial 
			complex associated (via the Stanley-Reisner correspondence) 
			to the circuit ideal of the matroid (which is a squarefree 
			monomial ideal). This method uses the @TO SimplicialComplexes@
			package to return an object of type @TO SimplicialComplex@.
			
		Example
			M = matroid({{0,1},{0,2},{0,3},{1,2},{2,3}})
			independenceComplex M
	SeeAlso
		(independentSets, Matroid)
		(ideal, Matroid)
		(basisIndicatorMatrix, Matroid)
///

doc ///
	Key
		maxWeightBasis
		(maxWeightBasis, Matroid, List)
	Headline
		maximum weight basis using greedy algorithm
	Usage
		maxWeightBasis(M, w)
	Inputs
		M:Matroid
		w:List
			a weight function
	Outputs
		:Set
			a maximum-weight basis obtained by the greedy algorithm
	Description
		Text
			For a matroid M on ground set E, a weight function on M is 
			a function $w : E -> \mathbb{R}$, extended to all subsets of 
			E by setting $w(X) := \sum_{x\in X} w(x)$. The greedy 
			algorithm for finding a maximum-weight independent subset 
			of E starts with the empty set, and proceeds by successively 
			adding elements of E of maximum weight, which together with 
			the elements already added, form an independent set.

			In this method, a weight function is specified by its list of values 
			on E. Thus if $E = \{e_1, ..., e_n\}$, then w is represented as the 
			list $\{w(e_1), ..., w(e_n)\}$.

			Matroids can be characterized via the greedy algorithm as follows: 
			a set $\mathcal{I}$ of subsets of E is the set of independent sets 
			of a matroid on E iff $\mathcal{I}$ is nonempty, downward closed, 
			and for every weight function $w : E -> \mathbb{R}$, the greedy 
			algorithm returns a maximal member of $\mathcal{I}$ of 
			maximum weight.
			
		Example
			M = matroid completeGraph 4
			bases M
			w1 = apply(M_*, e -> (toList e)#1)
			maxWeightBasis(M, w1)
			w2 = rsort w1
			maxWeightBasis(M, w2)
			
///

doc ///
	Key
		idealChowRing
		(idealChowRing, Matroid)
	Headline
		the defining ideal of the Chow ring
	Usage
		idealChowRing M
	Inputs
		M:Matroid
	Outputs
		:Ideal
			the defining ideal of the Chow ring of M
	Description
		Text
			The Chow ring of M is the ring R := QQ[x_F]/(I1 + I2), 
			where $I1 = (\sum_{i_1\in F} x_F - \sum_{i_2\in F} x_F : i_1, i_2 
			\in&nbsp;M)$ and $I2 = (x_Fx_{F'} : F, F' incomparable)$, 
			as $F$ runs over all proper nonempty flats of $M$. This is the 
			same as the Chow ring of the toric variety associated to the 
			Bergman fan of M. This ring is an Artinian standard graded 
			Gorenstein ring, by a result of Adiprasito, Katz, and Huh: cf. 
			https://arxiv.org/abs/1511.02888, Theorem 6.19.
			
			This method returns the defining ideal of the Chow ring, 
			which lives in a polynomial ring with variable indices equal to 
			the flats of M. To work with these subscripts, use 
			"last baseName v" to get the index of a variable v, as shown below:
			
		Example
			M = matroid completeGraph 4
			I = idealChowRing M
			(0..<rank M)/(i -> hilbertFunction(i, I))
			apply(gens ring I, v -> last baseName v)
	SeeAlso
		latticeOfFlats
		cogeneratorChowRing
///

doc ///
	Key
		cogeneratorChowRing
		(cogeneratorChowRing, Matroid)
	Headline
		cogenerator of the Chow ring of a matroid
	Usage
		cogeneratorChowRing M
	Inputs
		M:Matroid
	Outputs
		:RingElement
			the dual socle generator of the Chow ring of M
	Description
		Text
			If R is an Artinian Gorenstein k-algebra, then the Macaulay 
			inverse system of R is generated by a single polynomial (in 
			dual/differential variables), called the cogenerator (or dual 
			socle generator) of R. By a result of Adiprasito, Katz, and 
			Huh, the Chow ring of a matroid M is always Gorenstein. 
			This function computes the cogenerator of the Chow ring of M, 
			which is also called the volume polynomial of M. Note that 
			this is a very fine invariant of M - indeed, this single polynomial 
			can recover the entire Chow ring of M, and thus most of the 
			lattice of flats of M.
			
		Example
			M = matroid completeGraph 4
			I = idealChowRing M;
			numgens I
			F = cogeneratorChowRing M
			diff(gens((map(ring F, ring I, gens ring F)) I), F)
	SeeAlso
		latticeOfFlats
		idealChowRing
///

doc ///
	Key
		specificMatroids
		(specificMatroids, String)
	Headline
		creates built-in matroid
	Usage
		specificMatroids(S)
	Inputs
		S:String
			the name of the matroid
	Outputs
		:Matroid
	Description
		Text
			Returns one of the named matroids below.
		Code
			UL {
				"fano",
				"nonfano",
				"V8+",
				"vamos",
				"pappus",
				"nonpappus",
				"AG32",
				"R10"
			}
		Text
			Many of these matroids are interesting for their non-representability
			or duality properties:
		Code
			UL {
				"The Fano matroid F7 is the matroid of the projective plane 
				over F_2, and is representable only in characteristic 2. The non-Fano
				matroid is a relaxation of F7, and is representable only in 
				characteristic not equal to 2.",
				"The Pappus matroid is an illustration of Pappus' theorem. By the same
				token, the non-Pappus matroid is a relaxation which is not representable 
				over any field.",
				"The Vamos matroid V, which is a relaxation of V8+, is the smallest (size)
				matroid which is not representable over any field - indeed, it is not 
				even algebraic. V8+ is identically self-dual, while V is isomorphic to its 
				dual.",
				"AG32 is the affine geometry corresponding to a 3-dimensional vector
				space over F_2, and is identically self-dual, with circuits equal
				to its hyperplanes. A relaxation of AG32 is the smallest matroid not 
				representable over any field, with fewer basis elements than V."
			}
		Example
			F7 = specificMatroids "fano"
			all(F7_*, x -> areIsomorphic(matroid completeGraph 4, F7 \ {x}))
			AG32 = specificMatroids "AG32"
			representationOf AG32
			AG32 == dual AG32
			R10 = specificMatroids "R10"
			representationOf R10
			areIsomorphic(R10 \ set{0}, matroid completeMultipartiteGraph {3,3})
	Caveat
		Notice that the ground set is a subset of \{0, ..., n-1\} &nbsp; rather than
		\{1, ..., n\}.
///

doc ///
	Key
		allMatroids
		(allMatroids, ZZ)
	Headline
		returns all n-element matroids
	Usage
		allMatroids n
	Inputs
		n:ZZ
			the size of the ground set
	Outputs
		:List
			of matroids on n elements
	Description
		Text
			This method returns a list of matroids on n elements, for small n 
			(currently, n <= 8). This list is complete for isomorphism types
			of matroids on n elements, i.e. every matroid on n elements is
			@TO2{(areIsomorphic, Matroid, Matroid), "isomorphic"}@ to a 
			unique matroid in this list.
			
			One can perform many verifications using this method:
			
		CannedExample
			i1 : L = allMatroids 5; #L

			o2 = 38
			
			i3 : all(L, M -> isWellDefined M)
			
			o3 = true
			
			i4 : all(subsets(L, 2), S -> quickIsomorphismTest(S#0, S#1) == "false")
			
			o4 = true
			
			i5 : tally(L/fVector/values)
			
			o5 = Tally{{1, 1} => 5              }
				{1, 2, 1} => 6
				{1, 3, 1} => 4
				{1, 3, 3, 1} => 4
				{1, 4, 1} => 2
				{1, 4, 4, 1} => 3
				{1, 4, 6, 1} => 2
				{1, 4, 6, 4, 1} => 2
				{1, 5, 1} => 1
				{1, 5, 5, 1} => 1
				{1, 5, 6, 1} => 1
				{1, 5, 8, 1} => 1
				{1, 5, 8, 5, 1} => 1
				{1, 5, 10, 1} => 1
				{1, 5, 10, 7, 1} => 1
				{1, 5, 10, 10, 1} => 1
				{1, 5, 10, 10, 5, 1} => 1
				{1} => 1
			
			o5 : Tally
			
			i6 : smallMatroids = flatten apply(6, i -> allMatroids i); -- all matroids on < 6 elements
			
			i7 : #smallMatroids
			
			o7 = 70
///

undocumented {
	(net, Matroid),
	Loops,
	ParallelEdges
}

TEST ///
M = matroid({a, b, c, d}, {{a, b}, {a, c}})
assert(isWellDefined M and not isSimple M)
assert(set bases M === set {set{0, 1}, set{0, 2}})
assert(set nonbases M === set {set {2, 3}, set {0, 3}, set {1, 3}, set {1, 2}})
assert(set circuits M === set {set {1, 2}, set {3}})
assert(M == matroid({a,b,c,d},{{b,c},{d}}, EntryMode => "circuits"))
assert(not isDependent(M, {b}))
assert(set independentSets_M 1 === set {set{0}, set{1}, set{2}})
assert(coloops M === {0})
assert(loops M === {3})
assert(rank_M {a,d} === 1)
assert(closure_M {c,d} === {1, 2, 3})
assert(set hyperplanes M === set {set {0, 3}, set {1, 2, 3}})
assert(set flats M === set {set {3}, set {0, 3}, set {1, 2, 3}, set {0, 1, 2, 3}})
assert(values fVector M === {1, 2, 1})
D = dual M
assert(D == dual M)
N1 = M \ {d}
assert((N1_*, set bases N1) === ({a, b, c}, set {set {0, 1}, set {0, 2}}))
N2 = M / set{1}
assert((N2_*, set bases N2) === ({a, c, d}, set {set {0}}))
MA = matroid matrix{{0,4,-1,6},{0,2/3,7,1}}
assert(areIsomorphic(MA, M))
///

TEST ///
assert(not isWellDefined matroid({a,b,c,d}, {{b,c},{b,d}}, EntryMode=>"nonbases"))
M = matroid({a,b,c,d}, {}, EntryMode => "nonbases")
assert(isWellDefined M)
assert(ideal M == 0)
assert(M == matroid({a,b,c,d}, {}, EntryMode => "circuits"))
assert(M == uniformMatroid(4,4))
assert(#bases M == 1)
assert(fundamentalCircuit(M, set{1,2}, 3) === null)
R = ZZ/101[x_0..x_3]
assert(M == matroid monomialIdeal 0_R)
assert((try matroid ideal 1_R) === null)
assert((try matroid ideal()) === null)
N = matroid({a,b,c,d}, {{}})
assert(rank N == 0 and isWellDefined N and N == dual M)
M = matroid matrix{{1,0,1,1},{0,1,1,1}}
assert(M \ set{0} == M \ set{1} and not M \ set{0} == M \ set{2})
assert(fundamentalCircuit (M, (bases M)#2, 3) === set{2, 3})
assert(fundamentalCircuit (M, M_{0,1}, M_3) === set{0,1,3})
assert(fundamentalCircuit (M, M_{1,2}, M_3) === null)
assert(toString tuttePolynomial M == "x^2+x*y+y^2+x+y")
///

TEST ///
S = uniformMatroid(2,4) ++ matroid completeGraph 3
assert(S == uniformMatroid(2,4) + matroid completeGraph 3)
C = components S
assert(S == C#0 ++ C#1)
M = matroid(graph({{0,1},{1,2},{0,2},{3,4},{4,5},{3,5}}), Loops => {0,3,5})
assert(#loops M == 3 and #connectedComponents representationOf M == 2)
C = components M
assert(#C == 5 and #isomorphism(M, fold(C, (a, b) -> a ++ b)) == 432)
assert(characteristicPolynomial M == 0)
M1 = matroid({a,b,c,d}, {{a},{b},{c}})
M2 = matroid({a,b,c,d}, {{b},{c},{d}})
assert(M1 + M2 == uniformMatroid(2,4))
F7 = specificMatroids "fano"
NF = specificMatroids "nonfano"
assert(all({F7 + NF, F7 + F7, NF + NF}, M -> M == uniformMatroid(6, 7)))
///

TEST ///
M5 = matroid completeGraph 5
U24 = uniformMatroid(2, 4)
M4 = matroid completeGraph 4
assert(#bases M5 === 125 and #bases U24 == 6)
assert(set isomorphism(U24, dual U24) === set permutations 4)
assert(hasMinor(M5, M4))
minorM5 = minor(M5, set{9}, set{3,5,8})
assert(areIsomorphic(minorM5, M4))
assert(not hasMinor(M5, U24))
///

TEST ///
K4 = completeGraph 4
M4 = matroid K4
assert(toString tuttePolynomial M4 === "x^3+y^3+3*x^2+4*x*y+3*y^2+2*x+2*y")
assert(tutteEvaluate(M4, 2, 1) === 38)
assert(representationOf M4 === K4)
A = random(ZZ^3,ZZ^5)
assert(representationOf matroid A === A)
///

TEST ///
U34 = uniformMatroid(3,4)
I = idealChowRing U34
assert((0..<rank U34)/(i -> numColumns basis(i, comodule I)) === (1,7,1))
F = cogeneratorChowRing U34
phi = map(ring F, ring I, gens ring F)
assert(0 == diff(gens phi I, F))
///

TEST ///
F7 = specificMatroids "fano"
PG22 = projectiveGeometry(2,2)
A = transpose sub(matrix toList(((3:0)..(3:2-1))/toList), ZZ/2)
assert(PG22 == F7 and areIsomorphic(PG22, simpleMatroid matroid A))
M4 = matroid completeGraph 4
assert(all(F7_*, x -> areIsomorphic(M4, F7 \ {x})))
w = {0, log(2), 4/3, 1, -4, 2, pi_RR}
assert(maxWeightBasis(F7, w) === set{2,5,6})
assert(maxWeightBasis(F7, rsort w) === set{0,1,2})
///

TEST ///
M1 = matroid graph({{a,b},{b,c},{c,d},{d,e},{e,f},{f,g},{f,h},{c,h},{c,f},{a,g},{d,g}})
M2 = matroid graph({{a,b},{b,c},{c,d},{d,e},{e,f},{f,g},{f,h},{c,h},{c,f},{a,g},{a,h}})
T = ZZ[x,y]
assert(isWellDefined M1 and isWellDefined M2)
assert(tuttePolynomial(M1, T) === tuttePolynomial(M2, T))
F1 = set{0,1,2,3,7}
F2 = F1 + set{5,8}
assert(areIsomorphic(uniformMatroid(2,2), minor(M1, F1, M1.groundSet - F2)))
assert(areIsomorphic(M1, matroid graph edges graph M1_*))
Delta = independenceComplex M1
F = fVector Delta
assert(ideal Delta == ideal M1 and F === fVector independenceComplex M2)
assert((sort keys F)/(k -> F#k) === {1,11,55,164,319,409,324,125})
assert(not areIsomorphic(M1, M2))
///

TEST ///
AG32 = specificMatroids "AG32"
assert(AG32 == affineGeometry(3,2))
assert(set circuits AG32 === set hyperplanes AG32 and #circuits AG32 == 14)
isos = isomorphism(AG32, dual AG32)
assert(#isos == 1344 and member(toList(0..7), isos))
V8plus = specificMatroids "V8+"
assert(V8plus == dual V8plus)
V = specificMatroids "vamos"
assert(V == relaxation(V8plus, set{4,5,6,7}))
isos = isomorphism(V, dual V)
assert(#isos == 64 and not member(toList(0..7), isos))
assert(hasMinor(V, uniformMatroid(2,4)))
R10 = specificMatroids "R10"
assert(#isomorphism(R10 \ set{0}, matroid completeMultipartiteGraph {3,3}) == 72)
///

TEST ///
P8 = matroid(id_((ZZ/3)^4) | matrix{{0_(ZZ/3),1,1,-1},{1,0,1,1},{1,1,0,1},{-1,1,1,0}})
aut = isomorphism (P8, P8) -- automorphism group is transitive
assert(all(subsets(P8.groundSet,2)/toList, s -> any(aut, sigma -> sigma_(s#0) == s#1)))
sigma1 = {7,6,5,4,0,1,2,3}
sigma2 = {1,3,0,2,5,7,4,6}
assert(member(sigma1, aut) and member(sigma2, aut))
S8 = matroid(id_((ZZ/2)^4) | matrix{{0_(ZZ/2),1,1,1},{1,0,1,1},{1,1,0,1},{1,1,1,1}})
F7 = specificMatroids "fano"
assert(#select(S8_*, x -> areIsomorphic(S8 / {x}, F7)) == 1)
assert(#select(S8_*, x -> areIsomorphic(S8 \ {x}, dual F7)) == 1)
assert(#isomorphism(F7, F7) == 168)
///

TEST ///
smallMatroids = apply(6, i -> allMatroids i)
assert(smallMatroids/(L -> #L) == {1,2,4,8,17,38})
smallMatroids = flatten smallMatroids
assert(all(smallMatroids, M -> isWellDefined M))
assert(not any(subsets(smallMatroids, 2), S -> areIsomorphic(S#0, S#1)))
assert(all(smallMatroids_{1..69}, M -> areIsomorphic(M, fold(components M, (a, b) -> a ++ b))))
///

end--
restart
loadPackage("Matroids", Reload => true)
uninstallPackage "Matroids"
installPackage "Matroids"
installPackage("Matroids", RemakeAllDocumentation => true)
viewHelp "Matroids"
check "Matroids"