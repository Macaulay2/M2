newPackage(
    "HomotopyLieAlgebra",                                                 
                Headline => "Homotopy Lie algebra",
                Version => "1.0",
                Date => "November 18, 2026",
                Authors => {
                    {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "https://www.msri.org/~de"},
                    {Name => "Keller VandeBogert", Email => "keller.v@uky.edu", HomePage => "https://sites.google.com/view/kellervandebogert/home"}
		    },
                DebuggingMode => false,
		PackageExports => {"DGAlgebras"},
		Keywords => {"Lie Groups and Lie Algebras"}
                )                                      


export {"bracket",
	"bracketGamma",
	"bracketMatrix",
	"allgens",
	"ad",
	"isGradedSkewSymmetric",
	"isGradedJacobi",
	"isLieSuperalgebra",
	"LInfinity",
	"lInfinity",
	"isLInfinity",
	"LInfinityModule",
	"lInfinityModule",
	"mu",
	"muGamma",
	"isLInfinityModule",
	"OrderLimit",
	"GradeLimit",
	"DividedPower"}

-* Code section *-
homdeg = f -> first degree f 
intdeg = f -> last degree f
absdeg = m -> sum(listForm m)_0_0 -- number of factors of a monomial
isSquare = m -> max (listForm m)_0_0 >=2

allgens = method()
-*
allgens DGAlgebra := List => A -> (
    g := apply(gens((flattenRing A.natural)_0), 
               t -> sub(t, A.natural));
    sort(g, T -> homdeg T))
allgens(DGAlgebra, ZZ) := List => (A,d) -> select(allgens A, T -> homdeg T == d)		
*-
-- Read a polynomial tower down to its coefficient field. Field generators
-- (for example, a primitive element of a finite extension) are scalars.
hlaVariableLayers = A -> (
    base := coefficientRing A.natural;
    layers := while instance(base,PolynomialRing) and not isField base
        list base do base = coefficientRing base;
    (prepend(A.natural,layers),base))

allgens DGAlgebra := List => A -> (
    R := A.natural;
    if not R.cache#?"hlaAllGenerators" then (
        (layers,base) := hlaVariableLayers A;
        -- On quotient base rings allgens can still list the presentation
        -- variables, even though the Lie constructors require a free model.
        rings := if isField base then reverse layers else prepend(base,reverse layers);
        R.cache#"hlaAllGenerators" = flatten apply(rings, C -> apply(gens C, u -> promote(u,R))));
    R.cache#"hlaAllGenerators")

allgens(DGAlgebra, ZZ) := List => (A,d) -> select(allgens A, T -> homdeg T == d)		

--if V is a variable with a coefficient, we want to extract the index of the variable!
ind = V -> (keys ((keys standardForm V)_0))_0

pairing1 = method()
pairing1(List, RingElement) := RingElement => (L,M) -> (
    --L = {U,V}, where U,V are (dual) variables of A1
    --M is a monomial of absdeg 2 (possibly with nontrivial coef) in A1
    --return 0 unless M == r*x*y, and U,V = x,y or y,x, where r is in CoefficientRing A1.
    --Returns:
    --r(contract(V,x)*contract(U,y)+(-1)^((deg U)*(deg V))*contract(U,x)*contract(V,y).
    --Note that the monomials xy in diff(A, T_) are always written with increasing index.
    --Thus if index V > index U then <V,x> = 0, so the result is (-1)^(deg*deg)<u,x><v,y>
    (U,V) := (L_0,L_1);
    A1 := ring U;
    M2 := contract(V,M);
    if M2 == 0  then return 0_A1;
    Mcoef := contract(U,M2);
    if Mcoef == 0 then return 0_A1;
    --at this point M = cxy, with index y >= index x
    if isSquare M then if  homdeg U%2==0 then return 2*Mcoef else return 0_A1;
    if ind V < ind U then Mcoef else
    (-1)^((homdeg U)*(homdeg V)) * Mcoef
    )

pairing1(List, RingElement,String) := RingElement => (L,M,s) -> (
    --this is an alternate version, maybe faster?
    
    --L = {U,V}, where U,V are (dual) variables of A1
    --M is a monomial of absdeg 2 (possibly with nontrivial coef) in A1
    --return 0 unless M == r*x*y, and U,V = x,y or y,x, where r is in CoefficientRing A1.
    --Assumes ind x < ind y
    --Returns:
    --r(contract(V,x)*contract(U,y)+(-1)^((deg U)*(deg V))*contract(U,x)*contract(V,y).
    --Note that the monomials xy in diff(A, T_) are always written with increasing index.
    --Thus if index V > index U then <V,x> = 0, so the result is (-1)^(deg*deg)<u,x><v,y>
    (U,V) := (L_0,L_1);
    Mcoef := contract(U*V,M);
    --treat the case where the sign is +:
    if ((homdeg U)*(homdeg V)) % 2 == 0 then
       if U == V then return 2*Mcoef else return Mcoef;
    --now both are odd, sign is -
    if U == V then return 0;
    if ind V<ind U then return Mcoef else return -Mcoef
    )

///
debug HomotopyLieAlgebra
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
lastCyclesDegree = 4
KR = koszulComplexDGA(ideal R)
A = acyclicClosure(KR, EndDegree => lastCyclesDegree)
-- Write each product in the model generator order.
N = flatten for u in allgens(A,1) list for v in allgens(A,1) list (
    if ind u < ind v then M = u*v else M = v*u;
    (u,v,pairing1({u,v},M) == pairing1({u,v},M,"")))
N/last
-- Evaluate both pairings on the same ordered pair of generators.
t = allgens (A,1)
L = {t_1,t_0}, M = t_0*t_1
pairing1(L,M)
pairing1(L,M,"")
///

pairing = method()
pairing(List, RingElement) := RingElement => (L,M) -> (
    --L = {U,V}, where U,V are scalar linear combinations of generators u, v of A1,
    --all of the same homdeg
    --M is an element of A1
    --returns the sum of the values of pairing1({u,v},m)
    (U,V) := (L_0,L_1);
    MM := select(terms M, m -> absdeg m == 2);
    UU := terms U;
    VV := terms V;
    sum apply(UU, 
	u-> sum apply(VV, 
	    v -> sum apply(MM, 
		M'-> pairing1({u,v},M'))))
     )

pairing(List, RingElement, String) := RingElement => (L,M,s) -> (
    --L = {U,V}, where U,V are scalar linear combinations of generators u, v of A1,
    --all of the same homdeg
    --M is an element of A1
    --returns the sum of the values of pairing1({u,v},m)
    (U,V) := (L_0,L_1);
    MM := select(terms M, m -> absdeg m == 2);
    UU := terms U;
    VV := terms V;
    sum apply(UU, 
	u-> sum apply(VV, 
	    v -> sum apply(MM, 
		M'-> pairing1({u,v},M',"s"))))
     )

bracket = method()
bracket (DGAlgebra, List) := RingElement => (A,args) -> (
    validateMinimalModel A;
    if #args != 2 then error "the Lie bracket requires exactly two arguments";
    if not A.cache#?"bracketLInfinity" then
        A.cache#"bracketLInfinity" = lInfinity A;
    bracket(A.cache#"bracketLInfinity", args)
    )

bracketMatrix = method()
bracketMatrix (DGAlgebra, ZZ,ZZ) := Matrix => (A,d,e) -> (
    validateMinimalModel A;
    -- Cached: this matrix only depends on (A, d, e), and isGradedJacobi /
    -- isGradedSkewSymmetric call it repeatedly on the same arguments.
    if not A.cache#?"bracketMatrices" then A.cache#"bracketMatrices" = new MutableHashTable;
    if not (A.cache#"bracketMatrices")#?(d,e) then (
	Pd := allgens(A,d-1); --dual basis of d-th homotopy group
	Pe := allgens(A,e-1); --dual basis of e-th homotopy group
	(A.cache#"bracketMatrices")#(d,e) = if #Pd == 0 or #Pe == 0 then
	    map((A.natural)^(#Pd), (A.natural)^(#Pe), 0)
	else matrix apply(Pd, T -> apply(Pe, T' -> bracket(A,{T,T'}))));
    (A.cache#"bracketMatrices")#(d,e)
    )

bracket (DGAlgebra, List, RingElement) := RingElement => (A,args,T) -> (
    validateLieArguments(A,{T});
    contract(sub(T,A.natural), bracket(A,args))
    )

bracket(DGAlgebra, ZZ, ZZ) := HashTable => (A,d1,d2) -> (
    validateMinimalModel A;
    g1 := allgens(A,d1-1);
    g2 := allgens(A,d2-1);
    g3 := allgens(A,d1+d2-1);
    if #g1 * #g2 * #g3 == 0 then return hashTable {};
    diffs := apply(g3, T -> diff(A,T));
    hashTable flatten flatten apply(g1,u -> apply(g2,v -> (
        value := bracket(A,{u,v});
        apply(#g3, i -> (({u,v},diffs#i) => contract(g3#i,value))))))
    )

ad = method()
ad(DGAlgebra, RingElement, ZZ) := Matrix => (A,U,e) ->(
    --U is a scalar linear combination of generators of A.natural all of the same
    --homdeg, d-1
    --regarded as an element of Pi_(d).
    --ad(U): Pi^e -> Pi^(d+e) is a linear transformation V-> [U,V].
    validateLieArguments(A,{U});
    if U == 0 then (
        Pe := (A.natural)^(#allgens(A,e-1));
        return map(Pe,Pe,0));
    (toA1,fromA1) := A.cache#"maps";
    u := toA1 U;
    d := homdeg u + 1;
    if any(terms u, t -> homdeg t + 1 != d) then
        error "the adjoint requires a Lie element of a single homological degree";
    md := matrix{allgens(A,d-1)};
    mf := matrix{allgens(A, d + e - 1)};
    UU := contract(md, fromA1 u);
    b := bracketMatrix(A,d,e);
    contract(transpose mf, UU*b)
    )

-- Lie-superalgebra axiom checkers.
-- In characteristic =/= 2, 3 the graded Lie-superalgebra axioms reduce to
-- graded skew-symmetry and the graded Jacobi identity; both are bilinear /
-- trilinear, so checking on basis elements (the generators of Pi^d returned
-- by allgens(A,d-1)) suffices.

isGradedSkewSymmetric = method()
isGradedSkewSymmetric(DGAlgebra, ZZ, ZZ) := Boolean => (A, d, e) -> (
    validateMinimalModel A;
    -- [U,V] = -(-1)^(d*e) [V,U] for all U in Pi^d, V in Pi^e.
    -- Equivalent matrix form: bracketMatrix(A,d,e) + (-1)^(d*e) * (transpose bracketMatrix(A,e,d)) == 0.
    if #allgens(A,d-1) == 0 or #allgens(A,e-1) == 0 then return true;
    Bde := bracketMatrix(A,d,e);
    Bed := bracketMatrix(A,e,d);
    Bde + (-1)^(d*e) * transpose Bed == 0
    )

isGradedJacobi = method()
isGradedJacobi(DGAlgebra, ZZ, ZZ, ZZ) := Boolean => (A, d, e, f) -> (
    validateMinimalModel A;
    -- Leibniz form of the graded Jacobi identity:
    --   [U,[V,W]] = [[U,V],W] + (-1)^(d*e) [V,[U,W]]
    -- for all basis triples U in Pi^d, V in Pi^e, W in Pi^f.
    --
    -- The inner brackets [U,V], [V,W], [U,W] are looked up from cached
    -- bracketMatrix entries (a basis pair appears in many triples), and a zero
    -- inner bracket lets us skip the corresponding outer bracket call. The
    -- bracket matrices are very sparse on typical examples so this is a large
    -- speedup over recomputing the inner brackets per triple.
    Pid := allgens(A, d-1);
    Pie := allgens(A, e-1);
    Pif := allgens(A, f-1);
    if #Pid == 0 or #Pie == 0 or #Pif == 0 then return true;
    BUV := bracketMatrix(A, d, e);
    BVW := bracketMatrix(A, e, f);
    BUW := bracketMatrix(A, d, f);
    R := A.natural;
    z := 0_R;
    s := (-1)^(d*e);
    all(0..#Pid-1, i ->
	all(0..#Pie-1, j ->
	    all(0..#Pif-1, k -> (
		    iVW := BVW_(j,k);
		    iUV := BUV_(i,j);
		    iUW := BUW_(i,k);
		    lhs := if iVW == z then z else bracket(A, {Pid_i, iVW});
		    rhs := (if iUV == z then z else bracket(A, {iUV, Pif_k}))
			 + s * (if iUW == z then z else bracket(A, {Pie_j, iUW}));
		    lhs == rhs
		    ))))
    )

isLieSuperalgebra = method()
isLieSuperalgebra(DGAlgebra, ZZ) := Boolean => (A, n) -> (
    validateMinimalModel A;
    -- Verify the bracket defines a graded Lie superalgebra structure on the
    -- graded components Pi^1, ..., Pi^n: skew-symmetry on every pair (d,e) and
    -- Jacobi on every triple (d,e,f) with 1 <= d,e,f <= n.
    --
    -- In characteristic 2 the identity [x,x] = 0 (x of even Lie grade) is NOT
    -- implied by skew-symmetry, and in characteristic 3 the identity
    -- [x,[x,x]] = 0 (x of odd Lie grade) is NOT implied by Jacobi. We check
    -- the characteristic of A.natural and only run the corresponding extra
    -- axiom when relevant; in characteristics other than 2 and 3, both extra
    -- checks are vacuous.
    skewOK := all(1..n, d -> all(1..n, e -> isGradedSkewSymmetric(A, d, e)));
    jacOK := all(1..n, d -> all(1..n, e -> all(1..n, f -> isGradedJacobi(A, d, e, f))));
    p := char A.natural;
    evenOK := if p == 2 then
	all(1..n, d -> if d % 2 != 0 then true
	    else all(allgens(A, d-1), U -> bracket(A, {U, U}) == 0_(A.natural)))
	else true;
    oddOK := if p == 3 then
	all(1..n, d -> if d % 2 == 0 then true
	    else all(allgens(A, d-1), U ->
		bracket(A, {U, bracket(A, {U, U})}) == 0_(A.natural)))
	else true;
    skewOK and jacOK and evenOK and oddOK
    )

---------------------------------------------------------------
-- L_infinity structure on the homotopy Lie algebra
---------------------------------------------------------------
-- The minimal model differential expands as d = d_2 + d_3 + d_4 + ..., where
-- d_r: X -> S^r X is the word-length-r piece. Dualizing and applying the
-- canonical decalage gives the higher brackets:
--
--   ell_r(alpha_1, ..., alpha_r) = (-1)^eps * (prod e_i!) * <alpha_1 ... alpha_r, d_r(T)>
--
-- where eps = |x_{r-1}| + |x_{r-3}| + ... is the decalage sign from the
-- mapping-spaces reference (sum of homological degrees of arguments at
-- positions r-1, r-3, ..., 1-indexed), and the product of factorials is the
-- multiplicity factor for repeated arguments in the divided-power pairing.
-- Cf. Lemma 4.91 and Proposition 4.93 of the KoszulDualityCourse notes.
--
-- The sign uses the homological degrees before suspension. Together with
-- the graded permutation sign, this convention satisfies the standard
-- unshuffle identities with factor (-1)^(i*(j-1)). The DGAlgebra bracket
-- delegates to this same binary operation, so the two APIs agree exactly.

LInfinity = new Type of HashTable

-- Ensure the toA1 / fromA1 ring maps from HomotopyLieAlgebra are cached.
ensureMaps = A -> (
    if not A.cache#?"maps" then A.cache#"maps" = (
        R := A.natural;
        (layers,kk) := hlaVariableLayers A;
        if not instance(R,PolynomialRing) or not isField kk then
            error "homotopy Lie operations require a free graded-commutative algebra over a field; quotient and nonfield bases are not supported";
        if any(drop(layers,1), C -> any(gens C, u -> homdeg promote(u,R) != 0)) then
            error "the polynomial base of the DG model must lie in homological degree zero";
        for C in layers do (
            expectedSkew := select(toList(0..<numgens C), j -> odd homdeg promote(C_j,R));
            if set((options C)#SkewCommutative) != set expectedSkew or
                (options C)#WeylAlgebra != {} or (options C)#Inverses then
                error "homotopy Lie operations require polynomial even generators and exterior odd generators in nonnegative homological degrees");
        -- map expects the variables of the outermost ring first.
        vv := gens(R,CoefficientRing=>kk);
        if any(vv,T -> homdeg T < 0) then
            error "homotopy Lie operations require nonnegative homological degrees";
        -- Private names keep construction of the coordinate ring from
        -- rebinding the user's polynomial and DG variables.
        coord := local hlaVar;
        coord <- new IndexedVariableTable from coord;
        B := kk[apply(#vv,j -> coord_j), Degrees=>apply(vv,degree),
            SkewCommutative=>select(toList(0 ..< #vv),j -> odd homdeg vv#j)];
        (map(B,R,gens B),map(R,B,vv)));
    A.cache#"maps"
    )

-- The intrinsic checker tests d^2 in a bounded range, but the differential
-- must have degree -1 and no unary or constant terms in every degree.
requireMinimalAlgebraTerms = A -> (
    (toA1,fromA1) := ensureMaps A;
    for T in gens target toA1 do (
        dT := toA1 diff(A,fromA1 T);
        if any(terms dT, t -> homdeg t != homdeg T - 1) then
            error "every differential term must have homological degree one below its generator";
        if any(terms dT, t -> absdeg t < 2) then
            error "a minimal differential cannot contain constant or linear terms";
    );
    )

-- Verify the assumptions used by the minimal, uncurved L-infinity API.
validateMinimalModel = A -> (
    if A.cache#?"minimalModelDifferential" and
        A.cache#"minimalModelDifferential" === A.diff then return true;
    if not isWellDefined A then
        error "lInfinity requires a DG algebra with a degree -1 differential satisfying d^2 = 0";
    requireMinimalAlgebraTerms A;
    for key in {"bracketMatrices","bracketDiffs","bracketLInfinity"} do
        if A.cache#?key then remove(A.cache,key);
    A.cache#"minimalModelDifferential" = A.diff;
    true
    )

lInfinity = method(Options => {OrderLimit => 3, GradeLimit => 3})
lInfinity DGAlgebra := LInfinity => opts -> A -> (
    if not instance(opts.OrderLimit,ZZ) or opts.OrderLimit < 2 then
        error "OrderLimit must be an integer at least 2";
    if not instance(opts.GradeLimit,ZZ) or opts.GradeLimit < 1 then
        error "GradeLimit must be a positive integer";
    validateMinimalModel A;
    L := new LInfinity from hashTable {
        "dga" => A,
        "OrderLimit" => opts.OrderLimit,
        "GradeLimit" => opts.GradeLimit,
        cache => new CacheTable };
    L.cache#"brackets" = new MutableHashTable;
    L
    )

-- Arguments represent k-linear combinations of the dual generators.
validateLieArguments = (A,args) -> (
    validateMinimalModel A;
    (toA1,fromA1) := A.cache#"maps";
    for U in args do (
        if U == 0 then continue;
        if not instance(U,RingElement) then
            error "bracket arguments must be ring elements representing linear forms in the generators";
        V := toA1 U;
        if any(terms V,t -> absdeg t != 1) then
            error "bracket arguments must be linear forms in the generators over the coefficient field";
    );
    true
    )

-- Multiplicity factor prod e_i! for a single monomial m. Captures the
-- divided-power pairing factor when basis args repeat at the same variable.
multFactor = m -> product apply((listForm m)_0_0, e -> e!)

-- Graded Koszul sign (symmetric) for permuting (a_0,...,a_{n-1}) to
-- (a_{perm_0},...,a_{perm_{n-1}}). originalHomdegs#i = |a_i|.
gradedPermSign = (originalHomdegs, perm) -> (
    n := #perm;
    inv := 0;
    for i from 0 to n-1 do
	for j from i+1 to n-1 do
	    if perm#i > perm#j then
		inv = inv + originalHomdegs#(perm#i) * originalHomdegs#(perm#j);
    (-1)^inv
    )

-- Ordinary signature (-1)^(number of inversions) of a permutation.
permutationSign = perm -> (
    n := #perm;
    inv := 0;
    for i from 0 to n-1 do
	for j from i+1 to n-1 do
	    if perm#i > perm#j then inv = inv + 1;
    (-1)^inv
    )

-- Cartesian product of a list of lists. cartesianProduct {} = {{}}.
cartesianProduct = method()
cartesianProduct List := List => lists -> (
    if #lists == 0 then return {{}};
    rest := cartesianProduct drop(lists, 1);
    flatten apply(first lists, x -> apply(rest, tup -> prepend(x, tup)))
    )

-- Generate (i, k)-unshuffles of {0,...,i+k-1}, each as a list of length i+k.
unshuffles = (i, k) -> (
    n := i + k;
    apply(subsets(toList(0..n-1), i), S -> (
	    rest := select(toList(0..n-1), j -> not member(j, S));
	    join(S, rest)))
    )

-- Internal: build a (targetHomdeg, r, canonMon) -> list of (T, coefficient) cache
-- for the A-side diff decomposition. Mirrors the muTermCache on the module side.
-- After building, lInfBracketBasisA1 becomes a single hash lookup.
buildBracketTermCache = L -> (
    A := L#"dga";
    if L.cache#?"bracketTermCache" and L.cache#?"bracketDifferential" and
        L.cache#"bracketDifferential" === A.diff then return L.cache#"bracketTermCache";
    (toA1, fromA1) := A.cache#"maps";
    A1 := target toA1;
    cache := new MutableHashTable;
    for T in gens A1 do (
	tgtHomdeg := homdeg T;
	dT := toA1 diff(A, fromA1 T);
	if dT == 0 then continue;
	for t in terms dT do (
	    mon := leadMonomial t;
	    coef := leadCoefficient t;
	    r := sum (listForm t)#0#0;
	    key := (tgtHomdeg, r, mon);
	    if not cache#?key then cache#key = new MutableHashTable;
	    bucket := cache#key;
	    bucket#(#bucket) = (T, coef);
	);
    );
    -- Freeze each bucket once, retaining the order of the generators.
    for key in keys cache do (
        bucket := cache#key;
        cache#key = apply(#bucket, i -> bucket#i);
    );
    L.cache#"bracketTermCache" = cache;
    L.cache#"bracketDifferential" = A.diff;
    cache
    )

-- ell_r at a tuple of *basis* monomials, working entirely in A1.
-- args1 is a list of A1 monomials (single variables, leading coef 1).
-- Returns the bracket as an element of A1.
lInfBracketBasisA1 = (L, args1) -> (
    A := L#"dga";
    r := #args1;
    (toA1, fromA1) := A.cache#"maps";
    A1 := target toA1;

    homdegs := apply(args1, U -> homdeg U);
    targetHomdeg := (sum homdegs) + 1;

    epsilon := sum apply(select(toList(0..r-2), j -> (r - 2 - j) % 2 == 0),
	j -> homdegs#j);
    decalSign := (-1)^epsilon;

    indexedArgs := apply(toList(0..r-1), i -> {ind args1#i, i});
    sortedIdx := apply(sort indexedArgs, p -> p#1);
    sortedArgs1 := apply(sortedIdx, j -> args1#j);
    sortSign := gradedPermSign(homdegs, sortedIdx);

    canonMon := product sortedArgs1;
    if canonMon == 0 then return 0_A1;
    mult := multFactor canonMon;

    cache := buildBracketTermCache L;
    key := (targetHomdeg, r, canonMon);
    if not cache#?key then return 0_A1;
    contribs := cache#key;
    decalSign * sortSign * mult * sum apply(contribs, p -> p#1 * p#0)
    )

-- Multilinearly extended ell_r: accepts linear forms in any argument.
-- args may contain elements of the coefficient ring (which leadMonomial
-- in A.natural mishandles), so we map to A1 *first*, do the term
-- decomposition there, and map back at the end.
bracket (LInfinity, List) := RingElement => (L, args) -> (
    A := L#"dga";
    r := #args;
    if r < 2 then error "L-infinity bracket requires at least 2 arguments";
    validateLieArguments(A,args);
    if any(args, U -> U == 0) then return 0_(A.natural);

    (toA1, fromA1) := A.cache#"maps";
    args1 := apply(args, U -> toA1 U);
    if any(args1, U -> U == 0) then return 0_(A.natural);

    termsLists := apply(args1, U -> terms U);
    result := sum apply(cartesianProduct termsLists, tup -> (
	    coeffs := apply(tup, t -> leadCoefficient t);
	    monomials := apply(tup, t -> leadMonomial t);
	    (product coeffs) * lInfBracketBasisA1(L, monomials)));
    fromA1 result
    )

-- Internal: divided-power variant of lInfBracketBasisA1. Returns the
-- bracket value WITHOUT the multFactor (= ∏ e_v!) symmetrization weight,
-- giving ell_r evaluated on the divided-power basis element γ^e directly
-- rather than on the tensor x_{i_1} ⊗ ... ⊗ x_{i_r}. This is the
-- characteristic-independent formulation.
lInfBracketBasisA1Gamma = (L, args1) -> (
    A := L#"dga";
    r := #args1;
    (toA1, fromA1) := A.cache#"maps";
    A1 := target toA1;

    homdegs := apply(args1, U -> homdeg U);
    targetHomdeg := (sum homdegs) + 1;

    epsilon := sum apply(select(toList(0..r-2), j -> (r - 2 - j) % 2 == 0),
	j -> homdegs#j);
    decalSign := (-1)^epsilon;

    indexedArgs := apply(toList(0..r-1), i -> {ind args1#i, i});
    sortedIdx := apply(sort indexedArgs, p -> p#1);
    sortedArgs1 := apply(sortedIdx, j -> args1#j);
    sortSign := gradedPermSign(homdegs, sortedIdx);

    canonMon := product sortedArgs1;
    if canonMon == 0 then return 0_A1;
    -- NOTE: NO multFactor — gives the divided-power-evaluated bracket.

    cache := buildBracketTermCache L;
    key := (targetHomdeg, r, canonMon);
    if not cache#?key then return 0_A1;
    contribs := cache#key;
    decalSign * sortSign * sum apply(contribs, p -> p#1 * p#0)
    )

-- bracketGamma(L, args): divided-power variant of bracket. The args list
-- represents the divided-power basis element γ^e, where e_v is the
-- multiplicity of v among args. Returns ell_r evaluated on γ^e directly,
-- without the ∏ e_v! symmetrization weight that bracket uses.
--
-- For distinct args (no repeats), bracketGamma agrees with bracket.
-- For repeated args (or higher divided-power input), bracketGamma is
-- the characteristic-independent value; bracket gives multFactor times it,
-- which in characteristics p ≤ max e_v can vanish.
bracketGamma = method()
bracketGamma (LInfinity, List) := RingElement => (L, args) -> (
    A := L#"dga";
    r := #args;
    if r < 2 then error "bracketGamma requires at least 2 arguments";
    validateLieArguments(A,args);
    if any(args, U -> U == 0) then return 0_(A.natural);

    (toA1, fromA1) := A.cache#"maps";
    args1 := apply(args, U -> toA1 U);
    if any(args1, U -> U == 0) then return 0_(A.natural);

    requireGammaBasisArgs args1;
    termsLists := apply(args1, U -> terms U);
    result := sum apply(cartesianProduct termsLists, tup -> (
	    coeffs := apply(tup, t -> leadCoefficient t);
	    monomials := apply(tup, t -> leadMonomial t);
	    (product coeffs) * lInfBracketBasisA1Gamma(L, monomials)));
    fromA1 result
    )

-- Verify the generalized Jacobi (L_n) identity at a tuple of basis args.
-- The minimal-model assumption ell_1 = 0 means only i, j >= 2 terms contribute.
--   sum_{i+j=n+1, i,j >= 2} sum_{sigma in Sh(i, n-i)}
--        (-1)^{i(j-1)} * Koszul(sigma; |x|) *
--        ell_j(ell_i(x_{sigma(0)},...,x_{sigma(i-1)}), x_{sigma(i)},...,x_{sigma(n-1)}) = 0
checkLnIdentity = (L, args) -> (
    A := L#"dga";
    n := #args;
    -- The L_infty brackets live on the suspended space g = sV. Koszul signs in
    -- the L_n identity use the SUSPENDED degrees |sx| = |x| + 1 (= Lie grade),
    -- not the underlying homological degrees.
    lieGrades := apply(args, u -> (first degree u) + 1);
    accumulator := 0_(A.natural);
    for i from 2 to n - 1 do (
	j := n + 1 - i;
	signIJ := (-1)^(i * (j - 1));
	for unsh in unshuffles(i, n - i) do (
	    permArgs := apply(unsh, k -> args#k);
	    -- graded SIGNATURE: ordinary sign(sigma) * chi(sigma; |sx|)
	    koszulSign := permutationSign(unsh) * gradedPermSign(lieGrades, unsh);
	    inner := bracket(L, take(permArgs, i));
	    if inner != 0 then (
		outerArgs := prepend(inner, drop(permArgs, i));
		outerVal := bracket(L, outerArgs);
		accumulator = accumulator + signIJ * koszulSign * outerVal)));
    accumulator == 0_(A.natural)
    )

-- Warn if the coefficient characteristic is small enough that the
-- Sym-coalgebra (= classical char-0) formulation used by checkLnIdentity /
-- checkLnModuleIdentity may miss divided-power structure.
--
-- Reason: the bracket/mu formulas use `multFactor canonMon = prod e_i!` to
-- capture the symmetrization weight on repeated arguments. In characteristic
-- p where (n-1)! ≡ 0 (mod p), the L_n identity on repeated arguments
-- collapses trivially — the identity is "satisfied" in the Sym formulation
-- but doesn't probe the genuine L_∞ structure on divided-power basis
-- elements γ_n(x). The intrinsic checker retains those coefficients.
maybeWarnSmallChar = (A, n) -> (
    p := char A.natural;
    if p > 0 and p <= n then (
        stderr << "-- warning: characteristic " << p << " <= arity " << n
               << ": " << n << "! is not invertible. Classical tuple checks"
               << " may lose divided-power information. Use DividedPower => true"
               << " to check the structure encoded by the differential." << endl;
    );
)

-- The monomial basis of the free graded-commutative algebra is dual to
-- the divided-power/exterior coalgebra basis. Therefore the coefficient
-- of a monomial in d^2(T) is exactly the corresponding dual coderivation
-- square. Work in A.natural to retain its actual graded Leibniz signs;
-- do not replace the divided-power coproduct by binomial weights.
-- This verifies the structure encoded by the differential, independently
-- of the normalization used by the public bracket evaluators.
gammaTermInRange = (t, low, high, gradeLimit) -> (
    exponents := (listForm t)#0#0;
    wordLength := sum exponents;
    if wordLength < low or wordLength > high then return false;
    vars := gens ring t;
    all(#exponents, j -> exponents#j == 0 or homdeg(vars#j) + 1 <= gradeLimit)
    )

checkAlgebraGammaSquare = (L, nMax) -> (
    A := L#"dga";
    gradeLimit := L#"GradeLimit";
    requireMinimalAlgebraTerms A;
    (toA1, fromA1) := ensureMaps A;
    A1 := target toA1;
    all(gens A1, T -> (
        residual := toA1 diff(A, diff(A, fromA1 T));
        not any(terms residual, t -> gammaTermInRange(t, 3, nMax, gradeLimit))
    ))
    )

-- A list encodes a scalar multiple of a divided-power basis monomial:
-- its variable multiplicities specify the multi-index. It is not a
-- multilinear argument list. General linear combinations would require
-- the divided-power addition law, not slotwise distributivity.
requireGammaBasisArgs = args1 -> (
    if any(args1, U -> #terms U != 1 or sum((listForm U)#0#0) != 1) then
        error "divided-power arguments must be scalar multiples of single basis generators; linear combinations and products are not supported";
    args1
    )

-- Visit ordered tuples one at a time and stop at the first false result.
allBasisTuples = (inputs,n,test) -> (
    local visit;
    visit = (remaining,revTuple) -> if remaining == 0 then test(reverse revTuple)
        else all(inputs, u -> visit(remaining-1, prepend(u,revTuple)));
    visit(n,{})
    )

isLInfinity = method(Options => {DividedPower => false})
isLInfinity (LInfinity, ZZ) := Boolean => opts -> (L, nMax) -> (
    A := L#"dga";
    gradeLimit := L#"GradeLimit";
    if opts.DividedPower then checkAlgebraGammaSquare(L, nMax)
    else (
        validateMinimalModel A;
        maybeWarnSmallChar(A, nMax);
        inputs := select(allgens A, u -> 1 <= homdeg u + 1 and homdeg u + 1 <= gradeLimit);
        all(3..nMax, n -> allBasisTuples(inputs,n,args -> checkLnIdentity(L,args)))
    )
    )

---------------------------------------------------------------
-- L_infinity module: holonomy representation of an S-module
---------------------------------------------------------------
-- Given a DGAlgebra A and an S-module M (where S = A.ring) descending
-- to H_0(A), write its minimal semifree resolution as F = A otimes V.
-- Let W span all algebra generators, including the base polynomial variables.
-- The generator differential decomposes by word length:
--   d_F = sum_{i >= 2} d_F^{(i)},   d_F^{(i)}: V -> Sym^{i-1}(W) otimes V.
-- Dualizing with the suspension signs gives multilinear operations
--   mu_i: wedge^{i-1}(pi) otimes M_infty --> M_infty
-- on M_infty = V^vee, the L_infinity module structure associated to M.
--
-- The same decalage sign applies as in the algebra case, but parameterized
-- by the (i-1) algebra arguments (the module argument carries no extra
-- suspension): epsilon = |x_r| + |x_{r-2}| + ... with r = i-1.
-- For mu_2, epsilon is the homological degree of its single algebra argument.
-- This is the strict action compatible with the common binary bracket.
--
-- F^vee is represented self-dually by the underlying free A.natural-module
-- M.natural; the l-th basis vector (M.natural)_l plays the role of e_l^vee.

LInfinityModule = new Type of HashTable

-- Unary module operations are not supported: the differential must
-- vanish on the coefficient-field fiber. Acyclicity is not needed for
-- this structural check; the resolution constructor supplies that data.
moduleDataSignature = (A,DGM) ->
    {A.natural,A.diff,DGM.dgAlgebra,DGM.ring,DGM.natural,toList DGM.diff,toList DGM.Degrees}

validateModulePresentation = (A,DGM) -> (
    if DGM.dgAlgebra =!= A or DGM.ring =!= A.ring or ring DGM.natural =!= A.natural then
        error "the DG module must be over the supplied DG algebra";
    if not isFreeModule DGM.natural then error "the DG module must be semifree";
    if #DGM.diff != rank DGM.natural or DGM.Degrees != degrees DGM.natural then
        error "the DG module degrees and differentials must match its free generators";
    (toA1,fromA1) := ensureMaps A;
    for j from 0 to #DGM.diff-1 do (
        column := DGM.diff#j;
        if not instance(column,Vector) or module column =!= DGM.natural then
            error "each module differential must belong to the underlying free module";
        cs := entries column;
        for i from 0 to #cs-1 do if cs#i != 0 then (
            expected := first(DGM.Degrees#j) - first(DGM.Degrees#i) - 1;
            for t in terms(toA1 cs#i) do (
                if homdeg t != expected then error "the DG module differential must have homological degree -1";
                if absdeg t == 0 then
                    error "the module differential must be augmentation-minimal: a nonzero unary operation is not supported";
            );
        );
    );
    true
    )
validateMinimalModule = (A,DGM) -> (
    signature := moduleDataSignature(A,DGM);
    if DGM.cache#?"hlaMinimalModuleSignature" and
       DGM.cache#"hlaMinimalModuleSignature" === signature then return true;
    validateMinimalModel A;
    validateModulePresentation(A,DGM);
    -- Once d_A^2=0, the graded Leibniz rule makes d_F^2 A-linear.
    if any(DGM.diff, column -> column != 0 and diff(DGM,column) != 0) then
        error "the DG module differential must square to zero on each generator";
    DGM.cache#"hlaMinimalModuleSignature" = signature;
    true
    )

-- A module concentrated in degree zero descends along A -> H_0(A)
-- only when the boundaries in A_0 annihilate it.
validateModuleOverAugmentation = (A,Min) -> (
    if ring Min =!= A.ring then error "the module must be defined over the DG algebra's base ring";
    for T in allgens(A,1) do (
        f := lift(diff(A,T),A.ring);
        if f * id_(Min) != 0 then
            error "the degree-one differential of A must annihilate the input module (the module must descend to H_0(A))";
    );
    true
    )

makeLInfinityModule = (L, F, Min) -> (
    A := L#"dga";
    validateMinimalModule(A,F);
    inputModule := if Min === null then {} else {"module" => Min};
    new LInfinityModule from (inputModule | {
        "dga" => A, "lInfinity" => L, "dgmodule" => F, cache => new CacheTable})
    )

lInfinityModule = method(Options => {OrderLimit => 3, GradeLimit => 3, EndDegree => 3})
lInfinityModule DGModule := LInfinityModule => opts -> F -> (
    L := lInfinity(F.dgAlgebra, OrderLimit => opts.OrderLimit, GradeLimit => opts.GradeLimit);
    makeLInfinityModule(L,F,null)
    )
lInfinityModule (DGAlgebra, Module) := LInfinityModule => opts -> (A, Min) -> (
    if not instance(opts.EndDegree,ZZ) or opts.EndDegree < 0 then
        error "EndDegree must be a nonnegative integer";
    if ring Min =!= A.ring then error "the module must be defined over the DG algebra's base ring";
    L := lInfinity(A, OrderLimit => opts.OrderLimit, GradeLimit => opts.GradeLimit);
    validateModuleOverAugmentation(A,Min);
    F := minimalSemifreeResolution(A, Min, EndDegree => opts.EndDegree);
    makeLInfinityModule(L,F,Min)
    )

-- Internal: build a (k, r, canonMon) -> list of (l, coefficient) cache.
-- For each (l, k) module-differential entry, expand it as a sum of terms
-- in A1 and store (k, hom-deg-sum-r, monomial) -> (l, scalar coefficient).
-- After this is built, muBasisBasisA1 reduces from an O(numGens) loop with
-- per-iteration term decomposition to a single hash lookup + a short sum.
buildMuTermCache = LM -> (
    A := LM#"dga";
    DGM := LM#"dgmodule";
    signature := moduleDataSignature(A,DGM);
    if LM.cache#?"muTermCache" and LM.cache#?"muTermSignature" and
       LM.cache#"muTermSignature" === signature then return LM.cache#"muTermCache";
    (toA1, fromA1) := A.cache#"maps";
    A1 := target toA1;
    numGens := rank DGM.natural;
    kk := coefficientRing A1;
    cache := new MutableHashTable;
    for l from 0 to numGens - 1 do (
	if l >= #DGM.diff then continue;
	dEl := DGM.diff#l;
	if dEl == 0 then continue;
	ents := entries dEl;
	for k from 0 to numGens - 1 do (
	    cLK := ents#k;
	    if cLK == 0 then continue;
	    cLK1 := toA1 cLK;
	    for t in terms cLK1 do (
		mon := leadMonomial t;
		coef := leadCoefficient t;
		r := sum (listForm t)#0#0;  -- hom-deg sum of monomial in A1
		key := (k, r, mon);
		if not cache#?key then cache#key = new MutableHashTable;
		bucket := cache#key;
		bucket#(#bucket) = (l, lift(coef, kk));
	    );
	);
    );
    for key in keys cache do (
        bucket := cache#key;
        cache#key = apply(#bucket, i -> bucket#i);
    );
    LM.cache#"muTermCache" = cache;
    LM.cache#"muTermSignature" = signature;
    cache
    )

-- Internal: mu_i at a tuple of basis A1 monomials + module basis index.
-- args1 : list of (i-1) basis A1 monomials.
-- k     : module basis index.
-- Returns Vector in DGM.natural.
muBasisBasisA1 = (LM, args1, k) -> (
    A := LM#"dga";
    DGM := LM#"dgmodule";
    Mnat := DGM.natural;
    r := #args1;

    homdegs := apply(args1, U -> first degree U);
    -- decalage sign for mu_i (i = r + 1): positions r-1, r-3, ... (0-indexed)
    epsilon := sum apply(select(toList(0..r-1), j -> (r - 1 - j) % 2 == 0),
	j -> homdegs#j);
    decalSign := (-1)^epsilon;

    indexedArgs := apply(toList(0..r-1), i -> {ind args1#i, i});
    sortedIdx := apply(sort indexedArgs, p -> p#1);
    sortedArgs1 := apply(sortedIdx, j -> args1#j);
    sortSign := gradedPermSign(homdegs, sortedIdx);

    canonMon := product sortedArgs1;
    if canonMon == 0 then return 0_Mnat;
    mult := multFactor canonMon;
    sign := decalSign * sortSign * mult;

    cache := buildMuTermCache LM;
    key := (k, r, canonMon);
    if not cache#?key then return 0_Mnat;
    contribs := cache#key;
    result := 0_Mnat;
    for pair in contribs do
	result = result + (sign * pair#1) * Mnat_(pair#0);
    result
    )

-- mu_i with multilinear extension over algebra args (basis k for module).
mu = method()
mu (LInfinityModule, List, ZZ) := Vector => (LM, algArgs, k) -> (
    A := LM#"dga";
    DGM := LM#"dgmodule";
    Mnat := DGM.natural;
    validateMinimalModule(A,DGM);
    r := #algArgs;
    if r < 1 then error "mu requires at least one algebra arg + module index";
    if k < 0 or k >= rank Mnat then error "module basis index out of range";
    validateLieArguments(A,algArgs);
    if any(algArgs, U -> U == 0) then return 0_Mnat;
    (toA1, fromA1) := A.cache#"maps";
    args1 := apply(algArgs, U -> toA1 U);
    if any(args1, U -> U == 0) then return 0_Mnat;
    termsLists := apply(args1, U -> terms U);
    result := sum apply(cartesianProduct termsLists, tup -> (
	    coeffs := apply(tup, t -> leadCoefficient t);
	    monomials := apply(tup, t -> leadMonomial t);
	    (product coeffs) * muBasisBasisA1(LM, monomials, k)));
    if result == 0 then 0_Mnat else result
    )

-- Divided-power variant of muBasisBasisA1 — skips multFactor.
muBasisBasisA1Gamma = (LM, args1, k) -> (
    A := LM#"dga";
    DGM := LM#"dgmodule";
    Mnat := DGM.natural;
    r := #args1;

    homdegs := apply(args1, U -> first degree U);
    epsilon := sum apply(select(toList(0..r-1), j -> (r - 1 - j) % 2 == 0),
	j -> homdegs#j);
    decalSign := (-1)^epsilon;

    indexedArgs := apply(toList(0..r-1), i -> {ind args1#i, i});
    sortedIdx := apply(sort indexedArgs, p -> p#1);
    sortedArgs1 := apply(sortedIdx, j -> args1#j);
    sortSign := gradedPermSign(homdegs, sortedIdx);

    canonMon := product sortedArgs1;
    if canonMon == 0 then return 0_Mnat;
    -- NO multFactor — divided-power evaluation.
    sign := decalSign * sortSign;

    cache := buildMuTermCache LM;
    key := (k, r, canonMon);
    if not cache#?key then return 0_Mnat;
    contribs := cache#key;
    result := 0_Mnat;
    for pair in contribs do
	result = result + (sign * pair#1) * Mnat_(pair#0);
    result
    )

-- muGamma: divided-power variant of mu (module operation).
-- Same as mu but with no multFactor symmetrization weight.
muGamma = method()
muGamma (LInfinityModule, List, ZZ) := Vector => (LM, algArgs, k) -> (
    A := LM#"dga";
    DGM := LM#"dgmodule";
    Mnat := DGM.natural;
    validateMinimalModule(A,DGM);
    r := #algArgs;
    if r < 1 then error "muGamma requires at least one algebra arg + module index";
    if k < 0 or k >= rank Mnat then error "module basis index out of range";
    validateLieArguments(A,algArgs);
    if any(algArgs, U -> U == 0) then return 0_Mnat;
    (toA1, fromA1) := A.cache#"maps";
    args1 := apply(algArgs, U -> toA1 U);
    if any(args1, U -> U == 0) then return 0_Mnat;
    requireGammaBasisArgs args1;
    termsLists := apply(args1, U -> terms U);
    result := sum apply(cartesianProduct termsLists, tup -> (
	    coeffs := apply(tup, t -> leadCoefficient t);
	    monomials := apply(tup, t -> leadMonomial t);
	    (product coeffs) * muBasisBasisA1Gamma(LM, monomials, k)));
    if result == 0 then 0_Mnat else result
    )
muGamma (LInfinityModule, List, Vector) := Vector => (LM, algArgs, m) -> (
    A := LM#"dga";
    DGM := LM#"dgmodule";
    Mnat := DGM.natural;
    validateMinimalModule(A,DGM);
    if not (module m === Mnat) then error "expected a vector in the stored DG module's basis space";
    if #algArgs < 1 then error "muGamma requires at least one algebra argument";
    validateLieArguments(A,algArgs);
    if m == 0_Mnat then return 0_Mnat;
    comps := entries m;
    kk := coefficientRing target first ensureMaps A;
    result := 0_Mnat;
    for l from 0 to #comps - 1 do (
	cl := comps#l;
	if cl == 0 then continue;
	scalar := try lift(cl, kk) else error "module vectors must have coefficients in the coefficient field";
	result = result + scalar * muGamma(LM, algArgs, l));
    result
    )

-- mu_i with multilinear extension over a Vector module arg.
mu (LInfinityModule, List, Vector) := Vector => (LM, algArgs, m) -> (
    A := LM#"dga";
    DGM := LM#"dgmodule";
    Mnat := DGM.natural;
    validateMinimalModule(A,DGM);
    if not (module m === Mnat) then error "expected a vector in the stored DG module's basis space";
    if #algArgs < 1 then error "mu requires at least one algebra argument";
    validateLieArguments(A,algArgs);
    if m == 0_Mnat then return 0_Mnat;
    comps := entries m;
    kk := coefficientRing target first ensureMaps A;
    result := 0_Mnat;
    for l from 0 to #comps - 1 do (
	cl := comps#l;
	if cl == 0 then continue;
	scalar := try lift(cl, kk) else error "module vectors must have coefficients in the coefficient field";
	result = result + scalar * mu(LM, algArgs, l));
    result
    )

-- L_n^M identity at (algArgs of length n-1, k = module basis index).
-- Minimal-model form: only i, j >= 2 contribute.
--   Type A: mu_j(ell_i(x_{sigma(I)}), x_{sigma(complement)}, e_k^vee)
--   Type B: mu_j(x_{sigma(first j-1)}, mu_i(x_{sigma(last i-1)}, e_k^vee))
-- Type B carries (-1)^((i-1)(j-1) + (2-i) times the sum of the
-- outer algebra Lie grades). This follows by moving the inner module
-- output to the last slot in the standard L-infinity identity.
checkLnModuleIdentity = (LM, algArgs, k) -> (
    A := LM#"dga";
    L := LM#"lInfinity";
    DGM := LM#"dgmodule";
    Mnat := DGM.natural;
    (toA1, fromA1) := A.cache#"maps";
    n := #algArgs + 1;
    args1 := apply(algArgs, U -> toA1 U);
    lieGrades := apply(args1, U -> (first degree U) + 1);
    accumulator := 0_Mnat;
    for i from 2 to n - 1 do (
	j := n + 1 - i;
	if j < 2 then continue;
	signIJ := (-1)^(i * (j - 1));
	-- Type A: sum over (i, n-1-i)-unshuffles of the (n-1) algebra args
	for unsh in unshuffles(i, n - 1 - i) do (
	    permArgs := apply(unsh, idx -> algArgs#idx);
	    koszulSign := permutationSign(unsh) * gradedPermSign(lieGrades, unsh);
	    inner := bracket(L, take(permArgs, i));
	    if inner != 0 then (
		outerAlgArgs := prepend(inner, drop(permArgs, i));
		outerVal := mu(LM, outerAlgArgs, k);
		accumulator = accumulator + signIJ * koszulSign * outerVal));
	-- Type B: sum over (j-1, i-1)-unshuffles; outer takes first j-1 algebra
	-- args, inner takes last i-1 algebra args + e_k. (-1) overall vs Type A.
	for unsh in unshuffles(j - 1, i - 1) do (
	    permArgs := apply(unsh, idx -> algArgs#idx);
	    koszulSign := permutationSign(unsh) * gradedPermSign(lieGrades, unsh);
	    outerAlgArgs := take(permArgs, j - 1);
	    innerArgs := drop(permArgs, j - 1);
	    innerVal := mu(LM, innerArgs, k);
	    if innerVal != 0_Mnat then (
		outerVal := mu(LM, outerAlgArgs, innerVal);
                -- Moving the inner module output to the last slot and
                -- expressing the shuffle with the outer algebra inputs first
                -- contributes this sign (the inner operation has degree 2-i).
                outerLieDegree := sum apply(outerAlgArgs, U -> (first degree(toA1 U)) + 1);
                moduleSign := (-1)^((i-1)*(j-1) + (2-i)*outerLieDegree);
		accumulator = accumulator + signIJ * koszulSign * moduleSign * outerVal)));
    accumulator == 0_Mnat
    )


-- Dual-comodule square-zero test. The word-length-(n-1) coefficients
-- of d_F^2 are the arity-n module identities on the monomial-dual basis.
-- The DGModule differential applies the graded Leibniz rule, accounting
-- for both algebra and module insertions, including mixed degrees.
checkModuleGammaSquare = (LM, nMax) -> (
    A := LM#"dga";
    L := LM#"lInfinity";
    DGM := LM#"dgmodule";
    gradeLimit := L#"GradeLimit";
    requireMinimalAlgebraTerms A;
    (toA1, fromA1) := ensureMaps A;
    validateModulePresentation(A,DGM);
    all(0..rank(DGM.natural)-1, k -> (
        residual := diff(DGM, diff(DGM, DGM.natural_k));
        all(entries residual, coefficient ->
            not any(terms(toA1 coefficient), t -> gammaTermInRange(t, 2, nMax-1, gradeLimit)))
    ))
    )

isLInfinityModule = method(Options => {DividedPower => false})
isLInfinityModule (LInfinityModule, ZZ) := Boolean => opts -> (LM, nMax) -> (
    A := LM#"dga";
    L := LM#"lInfinity";
    DGM := LM#"dgmodule";
    gradeLimit := L#"GradeLimit";
    numK := rank DGM.natural;
    if opts.DividedPower then checkModuleGammaSquare(LM, nMax)
    else (
        validateMinimalModule(A,DGM);
        maybeWarnSmallChar(A, nMax);
        inputs := select(allgens A, u -> 1 <= homdeg u + 1 and homdeg u + 1 <= gradeLimit);
        numK == 0 or all(3..nMax, n -> allBasisTuples(inputs,n-1,args ->
            all(0..numK-1,k -> checkLnModuleIdentity(LM,args,k))))
    )
    )

///
restart
loadPackage "HomotopyLieAlgebra"

   kk = ZZ/101
   S = kk[x,y,z]
   R = S/ideal(x^2,y^2,z^2-x*y,x*z, y*z)
   lastCyclesDegree = 4
   KR = koszulComplexDGA(ideal R)
   A = acyclicClosure(KR, EndDegree => lastCyclesDegree);
   d = 1
   e = 1
   U = sum (Gd = allgens(A,d-1))
   ad(A,U,1)

   L = lInfinity A
   bracket(L, allgens(A, 1))
   isLInfinity(L, 3)
///

-* Documentation section *-
export {"HomotopyLie", "homotopyLieAlgebra", "HolonomyRepresentation", "holonomyRepresentation", "lieCoordinates", "lieRepresentative", "structureConstants", "lieAlgebra", "actionMatrix", "holonomyAction", "isHolonomyRepresentation", "coordinateSpace"}

-- Finite graded coordinates for the homotopy Lie algebra and its holonomy action.
hlaModelSnapshot = A -> (A.natural, A.diff)
hlaRequireSnapshot = G -> (
    if hlaModelSnapshot(G#"dga") =!= G#"snapshot" then error "the source differential has changed; reconstruct the HomotopyLie object";
    )
hlaRequireHolonomySnapshot = H -> (
    hlaRequireSnapshot(H#"lieAlgebra");
    DGM := H#"lInfinityModule"#"dgmodule";
    if moduleDataSignature(H#"lieAlgebra"#"dga",DGM) =!= H#"moduleSnapshot" then error "the module data have changed; reconstruct the HolonomyRepresentation";
    )
hlaHasInternalGrading = A -> all(allgens A, u -> (
    du := diff(A,u);
    du == 0 or (isHomogeneous du and drop(degree du,1) == drop(degree u,1))))
hlaModuleHasInternalGrading = DGM -> (
    all(toList(0..#DGM.diff-1), l -> (
        cs := entries DGM.diff#l;
        all(toList(0..#cs-1), k -> all(terms cs#k, t -> (
            dt := drop(degree t,1);
            dk := drop(DGM.Degrees#k,1);
            dl := drop(DGM.Degrees#l,1);
            apply(#dl,j -> dt#j+dk#j) == dl
        )))
    )))

-- Public basis representatives for evaluating higher operations.
-- Their native ring/module degrees describe the model; degrees L and degrees LM
-- report the cohomological degrees and any valid dual internal grading.
gens LInfinity := opts -> L -> (validateMinimalModel(L#"dga"); allgens(L#"dga"))
degrees LInfinity := L -> (
    A := L#"dga";
    validateMinimalModel A;
    internal := hlaHasInternalGrading A;
    apply(allgens A, u -> prepend(homdeg u + 1,
        if internal then apply(drop(degree u,1), d -> -d) else {})))
gens LInfinityModule := opts -> LM -> (
    DGM := LM#"dgmodule";
    validateMinimalModule(LM#"dga",DGM);
    apply(rank DGM.natural, i -> DGM.natural_i))
degrees LInfinityModule := LM -> (
    A := LM#"dga";
    DGM := LM#"dgmodule";
    validateMinimalModule(A,DGM);
    internal := hlaHasInternalGrading A and hlaModuleHasInternalGrading DGM;
    apply(DGM.Degrees, d -> prepend(first d,
        if internal then apply(drop(d,1), e -> -e) else {})))
lInfinity LInfinityModule := LInfinity => opts -> LM -> (
    validateMinimalModule(LM#"dga",LM#"dgmodule");
    lInfinity(LM#"dga",OrderLimit=>opts.OrderLimit,GradeLimit=>opts.GradeLimit))
net LInfinity := L -> net("LInfinity{" | toString(#allgens(L#"dga")) |
    " Lie generators; use bracket(L,{...})}")
net LInfinityModule := LM -> net("LInfinityModule{" | toString(rank(LM#"dgmodule").natural) |
    " module generators; use mu(LM,{...},e)}")

HomotopyLie = new Type of HashTable
HolonomyRepresentation = new Type of HashTable

homotopyLieAlgebra = method(Options => {GradeLimit => null})
homotopyLieAlgebra DGAlgebra := HomotopyLie => opts -> A -> (
    L := lInfinity A;
    (toA1, fromA1) := A.cache#"maps";
    kk := coefficientRing target toA1;
    if not isField kk then error "homotopyLieAlgebra requires a coefficient field";
    if char kk == 2 then error "homotopyLieAlgebra: characteristic 2 requires an odd squaring operation, which this type does not yet implement";
    all := allgens A;
    bound := if opts.GradeLimit === null then (if #all == 0 then 0 else max apply(all, u -> homdeg u + 1)) else opts.GradeLimit;
    if not instance(bound, ZZ) or bound < 0 then error "GradeLimit must be a nonnegative integer";
    bb := select(all, u -> homdeg u + 1 <= bound);
    internal := hlaHasInternalGrading A;
    new HomotopyLie from {
        "dga" => A, "lInfinity" => L, "field" => kk,
        "basis" => bb, "hasInternalGrading" => internal,
        "degrees" => apply(bb, u -> prepend(homdeg u + 1, if internal then apply(drop(degree u,1), d -> -d) else {})),
        "space" => kk^(#bb), "GradeLimit" => bound, "snapshot" => hlaModelSnapshot A, cache => new CacheTable}
    )

net HomotopyLie := G -> net("HomotopyLie{" | toString(#G#"basis") | " generators, Lie degrees <= " | toString G#"GradeLimit" | "}")
ring HomotopyLie := G -> (hlaRequireSnapshot G; G#"field")
gens HomotopyLie := opts -> G -> (hlaRequireSnapshot G; G#"basis")
degrees HomotopyLie := G -> (hlaRequireSnapshot G; G#"degrees")
dim HomotopyLie := G -> (hlaRequireSnapshot G; #G#"basis")
basis HomotopyLie := opts -> G -> (hlaRequireSnapshot G; id_(G#"space"))
basis (ZZ, HomotopyLie) := opts -> (d, G) -> (
    hlaRequireSnapshot G;
    idx := select(toList(0..#G#"basis"-1), i -> first(G#"degrees"#i) == d);
    if #idx == 0 then map(G#"space", (G#"field")^0, 0)
    else (id_(G#"space"))_idx
    )

lieRepresentative = method()
lieRepresentative (HomotopyLie, Vector) := RingElement => (G, v) -> (
    if not (module v === G#"space") then error "expected a coordinate vector in the Lie algebra's basis space";
    hlaRequireSnapshot G;
    A := G#"dga";
    ans := 0_(A.natural);
    cs := entries v;
    for i from 0 to #G#"basis"-1 do
        if cs#i != 0 then ans = ans + cs#i * G#"basis"#i;
    ans
    )

-- project=true is used only for the quotient by Lie degrees above GradeLimit.
hlaCoordinates = (G, u, project) -> (
    hlaRequireSnapshot G;
    A := G#"dga";
    (toA1, fromA1) := A.cache#"maps";
    u1 := if ring u === target toA1 then u else toA1 u;
    if not G.cache#?"basisIndices" then
        G.cache#"basisIndices" = hashTable apply(#G#"basis", i -> (toA1 G#"basis"#i) => i);
    indices := G.cache#"basisIndices";
    kk := G#"field";
    ans := 0_(G#"space");
    for t in terms u1 do (
        mon := leadMonomial t;
        if absdeg mon != 1 then error "expected a scalar linear combination of Lie basis representatives";
        if not indices#?mon then (
            if not project then error "Lie element lies outside the stored degree range";
        ) else ans = ans + lift(leadCoefficient t, kk) * (G#"space")_(indices#mon);
    );
    ans
    )
lieCoordinates = method()
lieCoordinates (HomotopyLie, RingElement) := Vector => (G, u) -> hlaCoordinates(G, u, false)
lieCoordinates (HomotopyLie, Vector) := Vector => (G, u) -> (
    hlaRequireSnapshot G;
    if not (module u === G#"space") then error "expected a coordinate vector in the Lie algebra's basis space";
    u)

bracket (HomotopyLie, List) := Vector => (G, args) -> (
    if #args != 2 then error "a HomotopyLie bracket takes exactly two arguments";
    vv := apply(args, u -> lieCoordinates(G, u));
    uu := apply(vv, v -> lieRepresentative(G, v));
    hlaCoordinates(G, bracket(G#"lInfinity", uu), true)
    )
structureConstants = method(Options => {DividedPower => false})
structureConstants HomotopyLie := HashTable => opts -> G -> (
    if opts.DividedPower then error "for divided powers, call structureConstants on an LInfinity algebra with an explicit arity";
    hlaRequireSnapshot G;
    if G.cache#?"structureConstants" then return G.cache#"structureConstants";
    pairs := new MutableHashTable;
    for i from 0 to dim G-1 do for j from 0 to dim G-1 do (
        v := bracket(G, {G#"basis"#i, G#"basis"#j});
        if v != 0 then pairs#(i,j) = v;
    );
    G.cache#"structureConstants" = new HashTable from pairs
    )
-- The term caches use flattened ring variables. Report each input in the
-- public order of gens L, which places the polynomial base variables first.
hlaOperationTable = (A, wordLength, termCache, evaluateEntry) -> (
    toA1 := first ensureMaps A;
    vars1 := apply(allgens A, u -> toA1 u);
    positions := apply(vars1, ind);
    result := new MutableHashTable;
    for termKey in keys termCache do (
        if termKey#1 != wordLength then continue;
        exps := (listForm(termKey#2))#0#0;
        indices := flatten apply(#positions, i -> toList(exps#(positions#i):i));
        args := apply(indices, i -> vars1#i);
        (key,val) := evaluateEntry(indices,args,termKey#0);
        if val != 0 then result#key = val;
    );
    new HashTable from result)

structureConstants (LInfinity, ZZ) := HashTable => opts -> (L,r) -> (
    if r < 2 then error "the arity must be at least two";
    A := L#"dga";
    validateMinimalModel A;
    fromA1 := last ensureMaps A;
    onBasis := if opts.DividedPower then lInfBracketBasisA1Gamma else lInfBracketBasisA1;
    hlaOperationTable(A, r, buildBracketTermCache L,
        (indices,args,j) -> (indices, fromA1 onBasis(L,args))))

structureConstants (LInfinityModule, ZZ) := HashTable => opts -> (LM,r) -> (
    if r < 2 then error "the arity must be at least two";
    A := LM#"dga";
    validateMinimalModule(A,LM#"dgmodule");
    onBasis := if opts.DividedPower then muBasisBasisA1Gamma else muBasisBasisA1;
    hlaOperationTable(A, r-1, buildMuTermCache LM,
        (indices,args,j) -> ((indices,j), onBasis(LM,args,j))))

bracketMatrix (HomotopyLie, ZZ, ZZ) := Matrix => (G, d, e) -> (
    hlaRequireSnapshot G;
    ii := select(toList(0..dim G-1), i -> first(G#"degrees"#i) == d);
    jj := select(toList(0..dim G-1), i -> first(G#"degrees"#i) == e);
    tt := select(toList(0..dim G-1), i -> first(G#"degrees"#i) == d+e);
    kk := G#"field";
    if #ii * #jj * #tt == 0 then return map(kk^(#tt), kk^(#ii * #jj), 0);
    columns := flatten apply(ii, i -> apply(jj, j -> entries bracket(G, {G#"basis"#i, G#"basis"#j})));
    matrix apply(tt, t -> apply(columns, c -> c#t))
    )
isLieSuperalgebra HomotopyLie := Boolean => G -> (
    bb := G#"basis";
    gg := apply(G#"degrees", first);
    for i from 0 to #bb-1 do for j from 0 to #bb-1 do (
        uv := bracket(G, {bb#i,bb#j});
        if uv + (-1)^(gg#i*gg#j)*bracket(G,{bb#j,bb#i}) != 0 then return false;
        for k from 0 to #bb-1 do (
            if bracket(G,{bb#i,bracket(G,{bb#j,bb#k})}) !=
               bracket(G,{uv,bb#k}) + (-1)^(gg#i*gg#j)*bracket(G,{bb#j,bracket(G,{bb#i,bb#k})}) then return false;
        );
        );
    if char ring G == 3 then for i from 0 to #bb-1 do
        if gg#i % 2 == 1 and bracket(G,{bb#i,bracket(G,{bb#i,bb#i})}) != 0 then return false;
    true
    )

holonomyRepresentation = method(Options => {EndDegree => 3, GradeLimit => null})
holonomyRepresentation LInfinityModule := HolonomyRepresentation => opts -> LM -> (
    A := LM#"dga";
    DGM := LM#"dgmodule";
    validateMinimalModule(A,DGM);
    G := homotopyLieAlgebra(A, GradeLimit => opts.GradeLimit);
    kk := ring G;
    omitted := select(allgens A, u -> homdeg u + 1 > G#"GradeLimit");
    for u in omitted do for j from 0 to rank DGM.natural-1 do
        if mu(LM,{u},j) != 0 then error "the holonomy action does not descend to this GradeLimit: an omitted Lie generator acts nontrivially";
    F := kk^(rank DGM.natural);
    matrices := apply(gens G, u -> (
        if rank F == 0 then map(F,F,0)
        else (
            columns := apply(rank F, j -> apply(entries mu(LM,{u},j), c -> lift(c,kk)));
            transpose matrix columns
        )
        ));
    internal := G#"hasInternalGrading" and hlaModuleHasInternalGrading DGM;
    H := new HolonomyRepresentation from {
        "lieAlgebra" => G, "lInfinityModule" => LM, "space" => F,
        "hasInternalGrading" => internal,
        "degrees" => apply(DGM.Degrees, d -> prepend(first d, if internal then apply(drop(d,1), e -> -e) else {})),
        "actionMatrices" => matrices, "moduleSnapshot" => moduleDataSignature(A,DGM), cache => new CacheTable};
    if not isHolonomyRepresentation H then error "quadratic action fails the graded representation identity; increase GradeLimit or check the differential/model";
    H
    )
holonomyRepresentation (DGAlgebra, Module) := HolonomyRepresentation => opts -> (A,M) -> (
    LM := lInfinityModule(A,M,EndDegree=>opts.EndDegree);
    holonomyRepresentation(LM,GradeLimit=>opts.GradeLimit))
holonomyRepresentation (HomotopyLie, Module) := HolonomyRepresentation => opts -> (G,M) -> (
    hlaRequireSnapshot G;
    H := holonomyRepresentation(G#"dga",M,EndDegree=>opts.EndDegree,GradeLimit=>G#"GradeLimit");
    H)

net HolonomyRepresentation := H -> net("HolonomyRepresentation{" | toString(rank H#"space") | " module generators, " | toString(dim H#"lieAlgebra") | " Lie generators}")
ring HolonomyRepresentation := H -> (hlaRequireHolonomySnapshot H; ring H#"lieAlgebra")
degrees HolonomyRepresentation := H -> (hlaRequireHolonomySnapshot H; H#"degrees")
dim HolonomyRepresentation := H -> (hlaRequireHolonomySnapshot H; rank H#"space")
basis HolonomyRepresentation := opts -> H -> (hlaRequireHolonomySnapshot H; id_(H#"space"))
basis (ZZ, HolonomyRepresentation) := opts -> (d,H) -> (
    hlaRequireHolonomySnapshot H;
    idx := select(toList(0..dim H-1), i -> first(H#"degrees"#i) == d);
    if #idx == 0 then map(H#"space", (ring H)^0, 0) else (id_(H#"space"))_idx)
lieAlgebra = method()
lieAlgebra LInfinityModule := LInfinity => LM -> (
    validateMinimalModule(LM#"dga",LM#"dgmodule");
    LM#"lInfinity")
lieAlgebra HolonomyRepresentation := H -> (hlaRequireHolonomySnapshot H; H#"lieAlgebra")
actionMatrix = method()
actionMatrix (HolonomyRepresentation, ZZ) := Matrix => (H,i) -> (
    hlaRequireHolonomySnapshot H;
    if i < 0 or i >= #H#"actionMatrices" then error "Lie basis index out of range";
    H#"actionMatrices"#i)
actionMatrix (HolonomyRepresentation, Vector) := Matrix => (H,u) -> (
    hlaRequireHolonomySnapshot H;
    c := entries lieCoordinates(H#"lieAlgebra",u);
    ans := map(H#"space",H#"space",0);
    for i from 0 to #c-1 do if c#i != 0 then ans = ans + c#i * H#"actionMatrices"#i;
    ans)
actionMatrix (HolonomyRepresentation, RingElement) := Matrix => (H,u) -> actionMatrix(H,lieCoordinates(H#"lieAlgebra",u))
actionMatrix (HolonomyRepresentation, ZZ, ZZ, ZZ) :=
actionMatrix (HolonomyRepresentation, Vector, ZZ, ZZ) :=
actionMatrix (HolonomyRepresentation, RingElement, ZZ, ZZ) := Matrix => (H,u,p,q) -> (
    matrixH := actionMatrix(H,u);
    moduleDegrees := apply(degrees H,first);
    rows := positions(moduleDegrees,d -> d == q);
    cols := positions(moduleDegrees,d -> d == p);
    (matrixH^rows)_cols)
holonomyAction = method()
holonomyAction (HolonomyRepresentation, Thing, Vector) := Vector => (H,u,v) -> (
    if not (module v === H#"space") then error "expected a vector in the holonomy module's coordinate space";
    actionMatrix(H,u) * v)
isHolonomyRepresentation = method()
isHolonomyRepresentation HolonomyRepresentation := Boolean => H -> (
    hlaRequireHolonomySnapshot H;
    G := H#"lieAlgebra";
    bb := gens G;
    dd := apply(degrees G,first);
    for i from 0 to #bb-1 do for j from i to #bb-1 do (
        X := actionMatrix(H,i);
        Y := actionMatrix(H,j);
        if X*Y-(-1)^(dd#i*dd#j)*Y*X != actionMatrix(H,bracket(G,{bb#i,bb#j})) then return false;
        );
    true)

coordinateSpace = method()
coordinateSpace HomotopyLie := G -> (hlaRequireSnapshot G; G#"space")
coordinateSpace HolonomyRepresentation := H -> (hlaRequireHolonomySnapshot H; H#"space")

beginDocumentation()


doc ///
Key
 HomotopyLieAlgebra
Headline
 Homotopy Lie algebras and module structures from a minimal DG model
Description
  Text
   This package computes the homotopy Lie algebra $\pi(R)$ of an augmented
   quotient $R=S/I$, where $S=k[x_1,\ldots,x_e]$ is a polynomial ring over a
   field. Choose a minimal DG model $A=S[X]$ of $R$. The duals
   $x_1^\vee,\ldots,x_e^\vee$ form a basis of $\pi^1(R)$. Each variable
   $T\in X$ of homological degree $d$ gives a basis element $T^\vee$ of
   $\pi^{d+1}(R)$. The relative construction for $S\twoheadrightarrow R$
   uses the fiber $k\otimes_S A$, whose variables are the elements of $X$.
   Briggs gives both constructions in §3.1 of his
   @HREF {"https://hdl.handle.net/1807/91805","doctoral thesis"}@ (pp. 46–47).

   @SUBSECTION "Reading the Lie bracket from the differential"@

   Write the minimal model as $A=k[V]$, with the polynomial variables
   $x_1,\ldots,x_e$ in $V_0$. On generators the differential decomposes
   by word length as
   $$\partial|_V=\partial_2+\partial_3+\cdots,\qquad
     \partial_r\colon V_n\longrightarrow(\operatorname{Sym}^r V)_{n-1}.$$
   For a generator $T$ of homological degree $n$, the terms of $\partial_r T$
   have $r$ factors and total homological degree $n-1$.
   Dualizing the quadratic component $\partial_2$ gives the Lie bracket
   $\pi^i(R)\otimes_k\pi^j(R)\to\pi^{i+j}(R)$ on the shifted dual
   $\pi^i(R)=V_{i-1}^\vee$.

   Take $R=k[x,y]/(x^2,y^2,xy)$ over $k=\mathbb F_{101}$.
   Calling @TO minimalModel@ with @TT "EndDegree=>2"@ kills cycles through
   degree two, giving the degree-three generators needed below.
  Example
   k = ZZ/101;
   S = k[x,y];
   R = S/ideal(x^2,y^2,x*y);
   A = minimalModel(R,EndDegree=>2);
   apply(allgens(A,1), t -> diff(A,t))
  Text
   Write $p,q,r$ for the three generators in the order displayed.
   Their differentials are already quadratic. Dualizing
   $\partial p=x^2$, $\partial q=y^2$, and $\partial r=xy$ gives
   $$[x^\vee,x^\vee]=2p^\vee,\qquad
     [y^\vee,y^\vee]=2q^\vee,\qquad [x^\vee,y^\vee]=r^\vee.$$
   The factor of two comes from the two ways to pair $x^\vee,x^\vee$
   with the factors of $x^2$, and likewise for $y^2$.

   The degree-one stage is the Koszul complex on $x^2,y^2,xy$.
   For instance, $xr-yp$ is a cycle because $x(xy)=y(x^2)$.
   The degree-two stage adjoins a variable whose differential is this cycle.
  Example
   apply(allgens(A,2), t -> diff(A,t))
  Text
   The second generator $v=T_{2,2}$ has $\partial v=xr-yp$.
   The term $-yp$ gives $[y^\vee,p^\vee]=-v^\vee$.
   Since $y^\vee$ has Lie degree one and $p^\vee$ has Lie degree two,
   graded skew-symmetry gives $[p^\vee,y^\vee]=v^\vee$.
  Example
   p = (allgens(A,1))#0; v = (allgens(A,2))#1;
   {bracket(A,{y_S,p},v), bracket(A,{p,y_S},v)}
  Text
   The two calls extract the $v^\vee$-coordinate in each order.

   @TO (bracketMatrix,DGAlgebra,ZZ,ZZ)@ lists $[u,v]$ in the row for
   $u$ and the column for $v$. An entry such as $2T_{1,1}$ stands for
   $2T_{1,1}^\vee$ in the Lie algebra.
  Example
   B11 = bracketMatrix(A,1,1)
   B21 = bracketMatrix(A,2,1)
   B22 = bracketMatrix(A,2,2)
  Text
   In $B_{11}$ the basis is $x^\vee,y^\vee$. Its first row contains the
   entries $2p$ and $r$.
   The rectangular table $B_{21}$ has a row for each of the three
   quadrics and a column for each of $x^\vee,y^\vee$. For example,
   its upper right entry says $[T_{1,1}^\vee,y^\vee]=T_{2,2}^\vee$.

   The degree-one inputs are odd, so $B_{11}$ is symmetric. Reversing
   two inputs introduces a minus sign whenever one is even.
  Example
   B11 - transpose B11
   B21 + transpose bracketMatrix(A,1,2)
   B22 + transpose B22
  Text
   Each calculation gives the zero matrix. To express the degree-one bracket
   as a linear map $\pi^1(R)\otimes\pi^1(R)\to\pi^2(R)$ over $k$,
   use the coordinate form of @TO bracketMatrix@.
  Example
   G = homotopyLieAlgebra(A,GradeLimit=>2);
   bracketMatrix(G,1,1)
  Text
   Its four columns correspond to the ordered pairs $(x^\vee,x^\vee)$,
   $(x^\vee,y^\vee)$, $(y^\vee,x^\vee)$ and $(y^\vee,y^\vee)$.
   Each column gives the three coefficients of the corresponding entry of
   $B_{11}$ in the basis dual to the quadrics.

   In @TO DGAlgebras@, @TT "minimalModel(R,EndDegree=>2)"@ builds
   @TT "acyclicClosure(koszulComplexDGA ideal R,EndDegree=>2)"@.
   Its degree-zero homology is $R$. Calling @TT "acyclicClosure(R,EndDegree=>2)"@
   starts over $R$ on its variables $x,y$ and has degree-zero homology $k$.

   @SUBSECTION "Higher brackets and module operations"@

   Dualizing the cubic component $\partial_3$ gives
   the ternary bracket $\ell_3$ on the shifted dual. More generally,
   $\partial_r$ gives an $r$-ary bracket $\ell_r$ of Lie degree $2-r$.
   See @TO (bracket,LInfinity,List)@ for the signs.

   Over $\mathbb Q$ these operations form a minimal $L_\infty$ algebra.
   Its identities come from sorting $\partial^2=0$ by word length.
   The length-three part, $\partial_2^2=0$, gives Jacobi for $\ell_2$.
   The length-four part, $\partial_2\partial_3+\partial_3\partial_2=0$,
   gives the first identity involving $\ell_2$ and $\ell_3$ together.

   Take the hypersurface $x^2-y^3=0$, with internal weights $3$ for $x$
   and $2$ for $y$. Its Koszul model has one adjoined generator $T$.
  Example
   S = QQ[x,y,Degrees=>{3,2}];
   A = koszulComplexDGA ideal(x^2-y^3);
   L = lInfinity A;
   (a,b,t) = toSequence gens L;
   diff(A,t)
   {bracket(L,{a,a}), bracket(L,{b,b,b}), bracket(L,{a,b,b})}
  Text
   Here $\partial_2T=x^2$ and $\partial_3T=-y^3$. The first two values
   are $\ell_2(x^\vee,x^\vee)=2T^\vee$ and
   $\ell_3(y^\vee,y^\vee,y^\vee)=-6T^\vee$. The factors are $2!$ and $3!$.
   The mixed ternary bracket vanishes because $\partial_3T$ contains no $xy^2$.
   To list all nonzero basis values of the ternary bracket, use
   @TO (structureConstants,LInfinity,ZZ)@.
   See @TO isLInfinity@ for a calculation in which the binary and ternary
   brackets have nonzero composites.

   For a module $M$, apply the same word-length decomposition to the
   differential of a minimal semifree resolution $F$ over $A$. A term
   with $r$ algebra factors times a module generator contributes to
   $\mu_{r+1}$ on $(k\otimes_A F)^\vee$. Take the residue field of
   $\mathbb Q[x]/(x^3)$.
  Example
   S = QQ[x];
   A = koszulComplexDGA ideal(x^3);
   M = coker vars S;
   LM = lInfinityModule(A, M, EndDegree => 1, GradeLimit => 2)
   a = first gens lieAlgebra LM;
   e = gens LM;
   degrees LM
   mu(LM, {a}, e#0)
   mu(LM, {a,a}, e#1)
  Text
   These calls give $\mu_2(x^\vee,e_0^\vee)=e_1^\vee$ and
   $\mu_3(x^\vee,x^\vee,e_1^\vee)=2e_2^\vee$. The module slot
   follows the algebra list, so $r$ list entries select $\mu_{r+1}$.
   Use @TO (structureConstants,LInfinityModule,ZZ)@ to find all the basis inputs
   on which a given module operation is nonzero. The @TO mu@ page explains how to read these coordinate vectors
   and recover the operations from the semifree differential.

   To work only with the binary module action, construct
   @TT "H = holonomyRepresentation LM"@ and use @TO actionMatrix@.

   @SUBSECTION "Worked examples"@

   @UL {
    LI {TO "Holonomy over a non-complete-intersection ring",
        ": a module over a square-zero ring, with every action matrix interpreted."},
    LI {TO "Computing higher brackets and module operations",
        ": a complete guide using the cubic hypersurface and its residue field."},
    LI {TO "L-infinity module on a higher matrix factorization",
        ": module operations over the complete intersection $k[a,b,c]/(a^4,b^4,c^4)$."},
    LI {TO "L-infinity module on a hypersurface MF",
        ": a module presented by a matrix factorization of $x^4-y^2$."},
    LI {TO "L-infinity module on an Avramov-Buchweitz support variety",
        ": a module over a complete intersection defined by pure squares."},
    LI {TO "L-infinity module over an ADE singularity",
        ": the residue field of the $A_2$ cusp $k[x,y]/(x^3+y^2)$."},
    LI {TO "L-infinity module over a Golod / Burch ring",
        ": higher module operations over a ring that is not a complete intersection."}
   }@

   @SUBSECTION "Characteristic"@

   The model built by @TO minimalModel@ uses ordinary polynomial powers of
   even variables. In characteristic $p$, an even model variable $t$
   satisfies $\partial(t^p)=p\,t^{p-1}\partial t=0$. In a Tate construction
   with divided powers one has $\partial(t^{(p)})=t^{(p-1)}\partial t$.
   Briggs discusses this distinction on p. 75 of his thesis.

   In characteristic zero, the symmetric-coalgebra and divided-power
   formulations of the $L_\infty$ identities are related by factorial
   normalizations. In positive characteristic, those factorials may vanish.
   The divided-power formulation uses
   $$\Gamma\bigl((L[1])_{\mathrm{even}}\bigr)
     \otimes\bigwedge\bigl((L[1])_{\mathrm{odd}}\bigr).$$

   The functions @TO isLInfinity@ and @TO isLInfinityModule@ use
   multilinear bracket and module identities by default. With
   @TT "DividedPower => true"@, they instead check the coefficients of
   $d_A^2$ and $d_F^2$ in the stored free models. These are the dual
   coalgebra and comodule identities, including mixed degrees and
   repeated even generators. This mode works without dividing by
   factorials and validates the structure encoded by the differentials
   independently of the public operation evaluators. Both modes are
   limited by the chosen arity, @TO GradeLimit@, and available generators.
References
 Benjamin Briggs,
 @HREF {"https://hdl.handle.net/1807/91805","Local Commutative Algebra and Hochschild Cohomology Through the Lens of Koszul Duality"}@,
 Ph.D. thesis, University of Toronto, 2018. See §2.8, pp. 42–43, for the quadratic differential
 and §3.1, pp. 46–47, for the homotopy Lie algebra of a ring or a homomorphism.

 Keller VandeBogert,
 @HREF {"https://drive.google.com/file/d/16-h3pH2XMjBObrxOTwmLiiD_nXiPcMp_/view","Koszul Duality in Commutative Ring Theory"}@,
 §4.6, Lemma 4.91.

 Iyengar S., "Free Resolutions and Change of Rings", Journal of Algebra
 190 (1997), 195–213. @HREF {"https://doi.org/10.1006/jabr.1996.6901", "DOI"}@;
 Avramov L. L., "Infinite Free Resolutions", in Six Lectures on
 Commutative Algebra (Bellaterra, 1996), Progress in Mathematics 166,
 Birkhäuser, 1998, pp. 1–118;
 Eisenbud D., Peeva I., "Minimal Free Resolutions over Complete Intersections"
 (LNM 2152, 2016);
 Tate J., "Homology of Noetherian rings and local rings" (Illinois J. Math. 1957).
SeeAlso
 bracket
 bracketMatrix
 lInfinity
 isLInfinity
 lInfinityModule
 isLInfinityModule
 mu
 bracketGamma
 muGamma
 DividedPower
///

doc ///
Key
 "Computing higher brackets and module operations"
Headline
 A worked guide to selecting inputs and reading higher operations
Description
 Text
  The package evaluates higher operations from the differential stored
  in a model. There is one evaluator for algebra operations and one
  for module operations:

  @UL {
   LI {TT "bracket(L,{u,v})", " evaluates $\\ell_2(u,v)$; ",
       TT "bracket(L,{u,v,w})", " evaluates $\\ell_3(u,v,w)$."},
   LI {TT "mu(LM,{u},m)", " evaluates $\\mu_2(u,m)$; ",
       TT "mu(LM,{u,v},m)", " evaluates $\\mu_3(u,v,m)$."}
  }@

  Thus @TT "bracket"@ has as many inputs as entries in its list,
  while @TT "mu"@ has one additional input: the module element after
  the list. There is no separate arity argument.

  @SUBSECTION "1. Construct a small model and inspect its Lie generators"@

  Work over $\mathbb{Q}$ with the cubic hypersurface
  $R=\mathbb{Q}[x]/(x^3)$. Its Koszul model has a single adjoined
  generator $T$ with $d(T)=x^3$:
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A, GradeLimit => 2)
  g = gens L
  degrees L
  a = g#0; t = g#1;
 Text
  The representatives @TT "a"@ and @TT "t"@ denote the dual Lie
  generators $x^\vee$ and $T^\vee$. Their cohomological degrees are
  $1$ and $2$. The second entries in @TT "degrees L"@ are their
  internal degrees, $-1$ and $-3$.

  The code reuses model variables to label dual Lie generators.
  Consequently @TT "degree a"@ is the degree of the model variable,
  whereas @TT "(degrees L)#0"@ is the degree of the Lie generator
  it represents. Use the latter when interpreting the operations.

  @SUBSECTION "2. Evaluate a nonzero higher bracket"@
 Example
  bracket(L, {a,a})
  bracket(L, {a,a,a})
 Text
  These return $0$ and $6T$, respectively. In Lie notation,
  $$\ell_2(x^\vee,x^\vee)=0,\qquad
    \ell_3(x^\vee,x^\vee,x^\vee)=6T^\vee.$$
  The differential has a cubic term but no quadratic term. The factor
  $6=3!$ comes from evaluating on three repeated inputs. This example
  shows why a zero binary bracket does not imply that all higher
  operations vanish.

  The list length selects the operation. To give a particular
  operation a convenient local name, define a function:
 Example
  ell3 = (u,v,w) -> bracket(L, {u,v,w});
  ell3(2*a, a, a)
 Text
  The result is $12T$. Inputs may be scalar linear combinations of
  the representatives in @TT "gens L"@; a product such as
  @TT "a^2"@ is not a Lie input.

  @SUBSECTION "3. Construct the module and select its dual basis"@

  Let $M=k$, regarded as an $S$-module. Compute enough of its semifree
  resolution to see the first three module generators:
 Example
  M = coker vars S;
  LM = lInfinityModule(A, M, EndDegree => 1, GradeLimit => 2)
  e = gens LM
  degrees LM
 Text
  The list @TT "e"@ contains representatives of
  $e_0^\vee,e_1^\vee,e_2^\vee$ in cohomological degrees $0,1,2$.
  These are the generators of $(k\otimes_A F)^\vee$, where $F$ is
  the computed semifree resolution. They are not generators of the
  original module $M$.

  @TT "EndDegree => 1"@ asks the resolution routine to kill cycles
  through degree $1$, which introduces generators in degree $2$.
  Inspect @TT "degrees LM"@ to see the actual computed range.

  @SUBSECTION "4. Evaluate the binary and ternary module operations"@
 Example
  mu(LM, {a}, e#0)
  mu(LM, {t}, e#0)
  v3 = mu(LM, {a,a}, e#1)
  entries v3
 Text
  The outputs represent
  $$\begin{aligned}
    \mu_2(x^\vee,e_0^\vee)&=e_1^\vee,\\
    \mu_2(T^\vee,e_0^\vee)&=e_2^\vee,\\
    \mu_3(x^\vee,x^\vee,e_1^\vee)&=2e_2^\vee.
  \end{aligned}$$
  The output of @TT "entries v3"@ is @TT "{0,0,2}"@.
  Entry $j$ is the coefficient of the basis element @TT "e#j"@.
  The displayed column is simply a coordinate vector for the dual
  basis; no further dualization is required.

  Instead of a basis vector, the final argument can be its zero-based
  index. The following equality compares the two input forms:
 Example
  mu(LM, {a,a}, 1) == v3
  mu(LM, {a,a}, 3*e#1)
 Text
  The second call returns $6e_2^\vee$. In the first call, the integer
  @TT "1"@ means the module basis element $e_1^\vee$; it does not
  specify the operation's arity. The two algebra entries select
  $\mu_3$.

  @SUBSECTION "5. Read the operation from the differential"@

  For inspection, the underlying DG module is stored in
  @TT "LM#\"dgmodule\""@. Its basis vectors $f_j$ correspond to
  the dual labels $e_j^\vee$ above:
 Example
  F = LM#"dgmodule";
  F.diff
 Text
  In this example,
  $$D(f_0)=0,\qquad D(f_1)=x f_0,\qquad
    D(f_2)=-T f_0+x^2f_1.$$
  The term $x f_0$ gives $\mu_2(x^\vee,e_0^\vee)=e_1^\vee$.
  The term $x^2f_1$ gives
  $\mu_3(x^\vee,x^\vee,e_1^\vee)=2e_2^\vee$; the factor $2$
  accounts for the repeated algebra input. The sign in the $T$ action
  includes the suspension sign described on the @TO mu@ page.

  @SUBSECTION "6. Check identities and extract the strict action"@
 Example
  isLInfinity(L, 4)
  isLInfinityModule(LM, 4)
  H = holonomyRepresentation LM
  isHolonomyRepresentation H
 Text
  These checks return @TT "true"@. The holonomy representation $H$
  retains $\mu_2$; the nonzero $\mu_3$ above remains accessible through
  $LM$. See @TO actionMatrix@ for matrices of the binary action.

  The integer passed to an identity checker is its maximum total
  arity. @TO GradeLimit@ bounds the Lie degrees of its test inputs;
  it does not restrict direct calls to @TO bracket@ or @TO mu@.
  All results concern the generators present in the supplied finite
  model and resolution. A zero output is a result in that computed
  range, not a certificate that missing higher generators are absent.

  For divided-power input conventions, including what happens when
  the characteristic divides a factorial, see @TO bracketGamma@ and
  @TO muGamma@.
SeeAlso
 LInfinity
 lInfinity
 (bracket, LInfinity, List)
 LInfinityModule
 lInfinityModule
 mu
 holonomyRepresentation
///

doc ///
Key
 bracket
Headline
 Evaluate a Lie bracket or a higher operation
Usage
 bracket(A, {u,v})
 bracket(A, {u,v}, F)
 bracket(A, d, e)
 bracket(G, {u,v})
 bracket(L, {u_1, ..., u_r})
Inputs
 A:DGAlgebra
 G:HomotopyLie
 L:LInfinity
Description
  Text
   The Lie bracket is dual to the quadratic part $\partial_2$ of the
   minimal model differential. For $u\in\pi^d(R)$ and $v\in\pi^e(R)$,
   its value on a model generator $T$ is
   $$\langle[u,v],T\rangle=(-1)^{d-1}\langle uv,\partial_2T\rangle.$$
   Macaulay2 uses the name $T$ for the Lie basis element $T^\vee$,
   so this value is the coefficient of $T$ in the output.
   For the pairing with the quadratic differential, see Briggs,
   @HREF {"https://hdl.handle.net/1807/91805","§2.8, Theorem 12 and its proof"}@
   (pp. 42–43).
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x*y);
   b = allgens(A,0);
   bracket(A,{b#0,b#0})
   bracket(A,{b#0,b#1})
   bracket(A,{sum b,sum b})
  Text
   Write $a=x^\vee$, $b=y^\vee$ and $t=T^\vee$, where $dT=xy$.
   The three outputs represent $[a,a]=0$, $[a,b]=t$, and $[a+b,a+b]=2t$.
   Both generators have Lie degree one, so $[b,a]=[a,b]$ and the two
   mixed terms in the last bracket add.

   Now take
   $R=\mathbb F_{101}[x,y,z]/(x^2,y^2,z^2-xy,xz,yz)$. Take $f$ to be
   the sum of the displayed basis representatives in $\pi^2$ and $g$ the
   corresponding sum in $\pi^3$. Their bracket belongs to $\pi^5$.
   Killing cycles through homological degree three supplies the degree-four
   model generators that pair with this bracket.
  Example
   k = ZZ/101;
   S = k[x,y,z];
   R = S/ideal(x^2,y^2,z^2-x*y,x*z,y*z);
   A = acyclicClosure(koszulComplexDGA ideal R,EndDegree=>3);
   p2 = allgens(A,1); p3 = allgens(A,2); targets = allgens(A,4);
   f = sum p2; g = sum p3;
   h = bracket(A,{f,g});
   {#targets,#(terms h)}
   {bracket(A,{f,g},first targets),bracket(A,{f,g},sum targets)}
  Text
   The first output counts $24$ possible coordinates in Lie degree five and
   $21$ nonzero coefficients in $h$. The next output reads off the coefficient
   $2$ on the first target and the sum $8$ of all the coefficients in
   $\mathbb F_{101}$. The third argument can thus be any linear combination
   of target generators. To look up a single structure constant for a pair
   of basis elements, use the table for these input degrees.
  Example
   C = bracket(A,2,3);
   u = first p2; v = first p3; T0 = first targets;
   {#keys C,#select(values C,c -> c != 0)}
   {C#({u,v},diff(A,T0)),bracket(A,{u,v},T0)}
  Text
   The table contains $5\cdot5\cdot24=600$ coefficients, of which $40$ are
   nonzero. Both lookups give $1$ for the coefficient of $T_0^\vee$ in
   $[u,v]$, where $T_0$ denotes the first target in the list.

   With a @TO HomotopyLie@ input, @TO bracket@ returns the coordinate vector
   over the coefficient field. For an @TO LInfinity@ input, a list of length
   $r$ selects the higher bracket $\ell_r$.
SeeAlso
 (bracket, DGAlgebra, List)
 (bracket, DGAlgebra, List, RingElement)
 (bracket, DGAlgebra, ZZ, ZZ)
 (bracket, HomotopyLie, List)
 (bracket, LInfinity, List)
///

doc ///
Key
 (bracket, DGAlgebra, List)
Headline
 Evaluate a binary bracket in model coordinates
Usage
 bracket(A, {u,v})
Inputs
 A:DGAlgebra
   a minimal free DG model
 :List
   two scalar linear combinations of model-generator representatives
Outputs
 :RingElement
   the representative of the binary bracket
Description
  Text
   The quadratic differential determines the bracket on the dual generators. A generator of
   homological degree $d-1$ represents an element of $\pi^d$. This method uses the same
   binary operation as @TO LInfinity@.
  Example
   S = QQ[x,y,z];
   A = koszulComplexDGA ideal(x*y-z^2);
   b = allgens(A,0);
   u = sum b;
   bracket(A,{u,u})
   bracket(A,{u,b#2})
  Text
   Let $a,b,c$ be dual to $x,y,z$ and $t$ dual to the relation.
   The term $xy$ gives $[a,b]=[b,a]=t$ and $-z^2$ gives $[c,c]=-2t$.
   For $u=a+b+c$ the three contributions to $[u,u]$ cancel.
   The second output represents $[u,c]=-2t$, so $u$ has zero
   self-bracket and still acts nontrivially by the bracket.
SeeAlso
 bracket
 (bracket, HomotopyLie, List)
 (bracket, LInfinity, List)
///

doc ///
Key
 (bracket, DGAlgebra, List, RingElement)
Headline
 Evaluate one coefficient of a binary bracket
Usage
 bracket(A, {u,v}, T)
Inputs
 A:DGAlgebra
 :List
   two Lie representatives
 T:RingElement
   a target generator, or a scalar linear form in target generators
Outputs
 :RingElement
   the scalar pairing with the target
Description
  Text
   This method pairs the representative of $[u,v]$ with the specified target linear form.
   For a target generator $T$, it extracts the coefficient of $T^\vee$ in the bracket.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2+y^2,x*y);
   b = allgens(A,0);
   t = allgens(A,1);
   u = sum b;
   bracket(A,{u,u})
   apply(t, T -> bracket(A,{u,u},T))
  Text
   Let $p,q$ be dual to the relations $x^2+y^2$ and $xy$ in that order.
   For $u=x^\vee+y^\vee$ the full bracket is $4p+2q$. Pairing with the two
   target generators returns $\{4,2\}$. The two squares each contribute
   $2p$ and the two ordered mixed terms each contribute $q$.
SeeAlso
 (bracket, DGAlgebra, List)
///

doc ///
Key
 (bracket, DGAlgebra, ZZ, ZZ)
Headline
 Tabulate bracket coefficients in two Lie degrees
Usage
 bracket(A, d, e)
Inputs
 A:DGAlgebra
 d:ZZ
   first Lie degree
 e:ZZ
   second Lie degree
Outputs
 :HashTable
   bracket coefficients indexed by input representatives and a target differential
Description
  Text
   For bases of $\pi^d$, $\pi^e$, and $\pi^{d+e}$, this method records the scalar
   coefficient of each target generator in each binary bracket. A key has the form @TT
   "({u,v},diff(A,T))"@.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2+y^2,x*y);
   b = allgens(A,0);
   t = allgens(A,1);
   C = bracket(A,1,1)
   apply(t, T -> C#({b#0,b#1},diff(A,T)))
   apply(t, T -> C#({b#1,b#1},diff(A,T)))
  Text
   The coefficient lists are $\{0,1\}$ and $\{2,0\}$. The mixed bracket
   is dual to the second relation $xy$, while the self-bracket of
   $y^\vee$ is twice the dual of the first relation $x^2+y^2$.
   The table uses the actual differential $dT$ in each key and includes
   zero coefficients.
SeeAlso
 structureConstants
 (bracket, DGAlgebra, List, RingElement)
///

doc ///
Key
 (bracket, HomotopyLie, List)
Headline
 Evaluate a bracket in finite Lie coordinates
Usage
 bracket(G, {u,v})
Inputs
 G:HomotopyLie
 :List
   two coordinate vectors or model-generator representatives
Outputs
 :Vector
   the bracket in the coordinate space of G
Description
  Text
   This method computes the binary bracket in the finite graded Lie algebra $G$. It accepts
   either vectors from @TO coordinateSpace@ or representatives returned by @TT "gens G"@,
   and projects the result to the Lie degrees retained by $G$.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2+y^2,x*y);
   G = homotopyLieAlgebra A;
   V = coordinateSpace G;
   u = V_0 + V_1;
   v = V_0 - V_1;
   bracket(G,{u,v})
   bracket(G,{u,u})
   bracket(G,{v,v})
  Text
   With $p,q$ dual to $x^2+y^2,xy$, respectively, the three vectors
   represent $0$, $4p+2q$, and $4p-2q$. Thus $u=x^\vee+y^\vee$
   and $v=x^\vee-y^\vee$ have zero mutual bracket even though both
   self-brackets are nonzero. The last two coordinates record the
   coefficients of $p$ and $q$.
SeeAlso
 lieCoordinates
 lieRepresentative
 homotopyLieAlgebra
///

doc ///
Key
 bracketMatrix
Headline
 Display binary brackets in matrix form
Usage
 bracketMatrix(A, d, e)
 bracketMatrix(G, d, e)
Inputs
 A:DGAlgebra
 G:HomotopyLie
 d:ZZ
 e:ZZ
Description
  Text
   For a DG model $A$, the rows of the multiplication table are the chosen
   basis of $\pi^d$ and the columns are the basis of $\pi^e$. Each entry is
   a linear form in the model variables of homological degree $d+e-1$, read
   as an element of $\pi^{d+e}$ in the dual basis.

   For $R=\mathbb F_{101}[x,y,z]/(x^2,y^2,z^2-xy,xz,yz)$, the five quadrics
   index the rows of the table for $[\pi^2,\pi^1]$. The columns correspond
   to $x^\vee,y^\vee,z^\vee$. Killing cycles through homological degree
   one gives the output basis in Lie degree three.
  Example
   k = ZZ/101;
   S = k[x,y,z];
   R = S/ideal(x^2,y^2,z^2-x*y,x*z,y*z);
   A = minimalModel(R,EndDegree=>1);
   B = bracketMatrix(A,2,1)
   B + transpose bracketMatrix(A,1,2) == 0
  Text
   Read across the first row. If $p=T_{1,1}^\vee$ is dual to the relation $x^2$,
   then $[p,x^\vee]=0$, $[p,y^\vee]=-T_{2,1}^\vee$, and
   $[p,z^\vee]=T_{2,2}^\vee$. Swapping the input degrees transposes the table
   and reverses its sign, since $p$ has even Lie degree.

   For a @TO HomotopyLie@ object $G$, the output gives the same linear map
   $\pi^d\otimes\pi^e\to\pi^{d+e}$ in tensor-product coordinates. A smaller
   example makes the two matrix layouts easy to compare.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2+y^2,x*y);
   G = homotopyLieAlgebra A;
   bracketMatrix(A,1,1)
   bracketMatrix(G,1,1)
  Text
   Write $P,Q$ for the model generators with $dP=x^2+y^2$ and $dQ=xy$.
   The first matrix has diagonal entries $2P$ and off-diagonal entries
   $Q$. The scalar matrix has rows $(2,0,0,2)$ and $(0,1,1,0)$.
   Reading its columns in the order $(x,x),(x,y),(y,x),(y,y)$ recovers
   the same four brackets in the dual basis $(P^\vee,Q^\vee)$.
SeeAlso
 (bracketMatrix, DGAlgebra, ZZ, ZZ)
 (bracketMatrix, HomotopyLie, ZZ, ZZ)
///

doc ///
Key
 (bracketMatrix, DGAlgebra, ZZ, ZZ)
Headline
 Tabulate bracket representatives by two Lie bases
Usage
 bracketMatrix(A, d, e)
Inputs
 A:DGAlgebra
 d:ZZ
   first Lie degree
 e:ZZ
   second Lie degree
Outputs
 :Matrix
   bracket representatives over A.natural
Description
  Text
   Rows are indexed by @TT "allgens(A,d-1)"@ and columns by @TT "allgens(A,e-1)"@. Entry
   $(i,j)$ is the linear form representing the bracket of those two dual generators.
  Example
   S = QQ[x,y,z];
   A = koszulComplexDGA ideal(x*y-z^2);
   bracketMatrix(A,1,1)
  Text
   Write $T$ for the generator with $dT=xy-z^2$. In the order $x,y,z$,
   the two off-diagonal entries indexed by $x,y$ both equal $T$.
   The $z,z$ entry is $-2T$. Thus the mixed term gives
   $[x^\vee,y^\vee]=T^\vee$ and the signed square gives
   $[z^\vee,z^\vee]=-2T^\vee$.
SeeAlso
 (bracketMatrix, HomotopyLie, ZZ, ZZ)
 allgens
///

doc ///
Key
 (bracketMatrix, HomotopyLie, ZZ, ZZ)
Headline
 Form the coefficient matrix of a graded bracket
Usage
 bracketMatrix(G, d, e)
Inputs
 G:HomotopyLie
 d:ZZ
   first Lie degree
 e:ZZ
   second Lie degree
Outputs
 :Matrix
   a coefficient-field matrix for the tensor-product bracket map
Description
  Text
   This is the matrix of $\pi^d\otimes\pi^e\to\pi^{d+e}$ in the stored ordered bases.
   Columns list input pairs with the first input varying slowest. Rows are the target
   generators of degree $d+e$.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
   G = homotopyLieAlgebra A;
   bracketMatrix(G,1,2)
  Text
   Let $p,q,r$ be dual to the three quadratic relations in the order
   $x^2,xy,y^2$, and $u,v$ dual to the next two generators. The
   columns for $x^\vee$ come first, followed by those for $y^\vee$.
   The first row $(0,1,0,-1,0,0)$ says that $[x^\vee,q]=u$ and
   $[y^\vee,p]=-u$. Their opposite signs come from the syzygy
   $x(xy)-y(x^2)=0$. The second row $(0,0,1,0,-1,0)$ reads the
   next syzygy $x(y^2)-y(xy)=0$ in the same way.
SeeAlso
 structureConstants
 (basis, ZZ, HomotopyLie)
///

doc ///
Key
 allgens
Headline
 List model representatives of homotopy Lie generators
Usage
 allgens A
 allgens(A, h)
Inputs
 A:DGAlgebra
 h:ZZ
Description
  Text
   The Lie basis is dual to all generators of the flattened free DG model, including the
   polynomial variables of its base ring. A model generator of homological degree $h$
   represents a Lie generator of degree $h+1$.
  Example
   S = QQ[x];
   A = koszulComplexDGA ideal(x^2);
   allgens A
   allgens(A, 1)
  Text
   The full list is $\{x,T\}$, with $d(T)=x^2$. The filtered list is $\{T\}$: the filter
   uses homological degree $1$, which corresponds to Lie degree $2$.
  Text
   Take $S=\mathbb F_{101}[x,y,z]$. The list @TT "gens A.natural"@
   gives the variables adjoined to $S$, while @TT "allgens A"@ includes
   $x,y,z$ as well. The order within each degree gives the basis order
   used by @TO bracketMatrix@.
  Example
   S = (ZZ/101)[x,y,z];
   R = S/ideal(x^2,y^2,z^2-x*y,x*z,y*z);
   A = acyclicClosure(koszulComplexDGA ideal R, EndDegree=>1);
   {#gens A.natural, #allgens A}
   allgens(A,0)
   apply(allgens(A,2), degree)
  Text
   The model has ten adjoined generators. The full list has thirteen
   entries because $x,y,z$ also represent Lie generators. Killing cycles
   through homological degree one introduces five generators in degree
   two, each of internal degree three. Their duals form a basis of
   $\pi^3$, the target of the products listed in @TT "bracketMatrix(A,1,2)"@.
 Text
  For a polynomial tower such as $k[x][y]$, the list begins with $x,y$
  in the order the variables were adjoined, followed by the DG generators.
SeeAlso
 (allgens, DGAlgebra)
 (allgens, DGAlgebra, ZZ)
///

doc ///
Key
 (allgens, DGAlgebra)
Headline
 List all dual-generator representatives of a DG model
Usage
 allgens A
Inputs
 A:DGAlgebra
Outputs
 :List
   polynomial base variables followed by the adjoined DG generators
Description
  Text
   This list supplies representatives for the dual generators of the absolute homotopy Lie
   algebra. It includes the base-ring variables, which represent Lie degree-one elements,
   in addition to the adjoined DG generators.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^3,y^2);
   gens A.natural
   b = allgens A
   apply(b, u -> diff(A,u))
  Text
   The first list contains the two Koszul generators $P,Q$.
   The second adds $x,y$ at the front. In this order the differential
   list is $0,0,x^3,y^2$. Thus the first two representatives are dual
   to Lie degree-one elements and the last two are dual to the relations,
   both in Lie degree two.
 Text
  Over a polynomial tower the variables of the innermost polynomial
  ring come first, followed by the variables of each successive extension.
SeeAlso
 (allgens, DGAlgebra, ZZ)
 (gens, HomotopyLie)
///

doc ///
Key
 (allgens, DGAlgebra, ZZ)
Headline
 Select model generators of one homological degree
Usage
 allgens(A, h)
Inputs
 A:DGAlgebra
 h:ZZ
   homological degree in the DG model
Outputs
 :List
   representatives of Lie degree h+1
Description
  Text
   The integer $h$ is a model homological degree. The selected generators represent the
   dual basis of $\pi^{h+1}$; to obtain Lie degree $d$, use @TT "allgens(A,d-1)"@.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
   apply({0,1,2}, h -> #allgens(A,h))
   g = allgens(A,2)
   apply(g, T -> diff(A,T))
  Text
   The counts $2,3,2$ correspond to Lie degrees $1,2,3$. The three
   degree-one model generators $P,Q,R$ have differentials $x^2,xy,y^2$.
   The displayed pair of degree-two generators has differentials $xQ-yP$ and $xR-yQ$; these are the two
   syzygies killed by the new degree-two generators.
SeeAlso
 (allgens, DGAlgebra)
///

doc ///
Key
 ad
Headline
 Compute the adjoint action in one Lie degree
Usage
 ad(A, u, e)
Inputs
 A:DGAlgebra
 u:RingElement
 e:ZZ
Description
  Text
   For homogeneous $u\in\pi^d$, the adjoint map is
   $\operatorname{ad}(u)\colon\pi^e\to\pi^{d+e}$, $v\mapsto[u,v]$. The result expresses
   this map in the model's ordered dual bases.
  Example
   S = QQ[x,y,z];
   A = koszulComplexDGA ideal(x*y-z^2);
   b = allgens(A,0);
   u = sum b;
   ad(A,u,1)
   bracket(A,{u,u})
  Text
   For $u=x^\vee+y^\vee+z^\vee$ the adjoint row is $(1,1,-2)$
   in the target basis $T^\vee$. In particular $[u,z^\vee]=-2T^\vee$.
   Multiplying the row by the column $(1,1,1)^{\mathsf T}$ gives zero,
   as the self-bracket confirms. Thus $u$ belongs to the kernel of
   its own adjoint map, which has rank one.
  Text
   For $u=x^\vee+y^\vee+z^\vee$, bilinearity gives
   $[u,v]=[x^\vee,v]+[y^\vee,v]+[z^\vee,v]$. Thus adding three rows
   of the bracket table gives the action of $u$. Write $p_i$ for the
   duals of the five relations in the order given below.
  Example
   S = (ZZ/101)[x,y,z];
   R = S/ideal(x^2,y^2,z^2-x*y,x*z,y*z);
   A = acyclicClosure(koszulComplexDGA ideal R, EndDegree=>1);
   u = sum allgens(A,0);
   C = ad(A,u,1)
   rowSum = matrix{{1,1,1}} * bracketMatrix(A,1,1)
   matrix{allgens(A,1)} * C == rowSum
  Text
   The first column of $C$ reads $[u,x^\vee]=2p_1-p_3+p_4$.
   The same value appears as a linear form in the first entry of
   @TT "rowSum"@. Multiplying $C$ on the left by the row of model
   generators writes each of its three columns in that form.
SeeAlso
 (ad, DGAlgebra, RingElement, ZZ)
 bracketMatrix
///

doc ///
Key
 (ad, DGAlgebra, RingElement, ZZ)
Headline
 Form one homogeneous adjoint-action matrix
Usage
 ad(A, u, e)
Inputs
 A:DGAlgebra
 u:RingElement
   a homogeneous scalar linear form representing a Lie element
 e:ZZ
   Lie degree of the source
Outputs
 :Matrix
   the matrix of v mapping to [u,v]
Description
  Text
   If $u$ has model homological degree $d-1$, columns correspond to @TT "allgens(A,e-1)"@
   and rows to @TT "allgens(A,d+e-1)"@. Thus the matrix represents $\pi^e\to\pi^{d+e}$.
   The terms of $u$ must have the same homological degree but may have different
   internal degrees. Polynomial variables have homological degree zero in the model.
   For $u=0$, the method returns the zero endomorphism of $\pi^e$.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
   b = allgens(A,0);
   ad(A,b#0,2)
   ad(A,b#1,2)
  Text
   The columns are indexed by the three relation duals $p,q,r$ and
   the rows by the two syzygy duals $u,v$. The first map sends
   $p,q,r$ to $0,u,v$ and the second sends them to $-u,-v,0$.
   Their kernels are distinct lines even though both maps are surjective.
SeeAlso
 ad
 (bracket, DGAlgebra, List)
///

doc ///
Key
 isGradedSkewSymmetric
Headline
 Check graded skew-symmetry in two Lie degrees
Usage
 isGradedSkewSymmetric(A, d, e)
Inputs
 A:DGAlgebra
 d:ZZ
   first Lie degree
 e:ZZ
   second Lie degree
Outputs
 :Boolean
Description
  Text
   For $u\in\pi^d$ and $v\in\pi^e$, graded skew-symmetry is $[u,v]=-(-1)^{de}[v,u]$. In
   particular, the bracket of two odd Lie elements is symmetric, so their self-brackets
   need not vanish.
  Text
   Over $R=\mathbb Q[x,y]/(x^2,xy)$, write $a=x^\vee$, $b=y^\vee$.
   The relation $xy$ gives their bracket $q=T_{1,2}^\vee$ in Lie degree two.
   What happens if we exchange the inputs, first in $[a,b]$ and then in $[a,q]$?
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y), EndDegree=>1);
   a = x_S; b = y_S;
   q = bracket(A,{a,b});
   {q, bracket(A,{b,a})}
   {bracket(A,{a,q}), bracket(A,{q,a})}
   {isGradedSkewSymmetric(A,1,1), isGradedSkewSymmetric(A,1,2)}
  Text
   The first pair is $q,q$. Both inputs have odd Lie degree, so exchanging them
   preserves the sign. The second pair is $T_{2,1}^\vee,-T_{2,1}^\vee$.
   Now one input has even degree, and exchanging them reverses the sign.
   The two Boolean values check these signs on every basis pair in the indicated degrees.
SeeAlso
 isGradedJacobi
 isLieSuperalgebra
///

doc ///
Key
 (isGradedSkewSymmetric, DGAlgebra, ZZ, ZZ)
Headline
 Check graded skew-symmetry in two Lie degrees
Usage
 isGradedSkewSymmetric(A, d, e)
Inputs
 A:DGAlgebra
 d:ZZ
   first Lie degree
 e:ZZ
   second Lie degree
Outputs
 :Boolean
Description
  Text
   For $u\in\pi^d$ and $v\in\pi^e$, graded skew-symmetry is $[u,v]=-(-1)^{de}[v,u]$. In
   particular, the bracket of two odd Lie elements is symmetric, so their self-brackets
   need not vanish.
  Text
   For $R=\mathbb Q[x,y]/(x,y)^2$ there are two generators in Lie degree one
   and three in degree two. Swapping the inputs to the bracket therefore changes the
   shape of its table as well as the signs of its nonzero entries.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree=>1);
   B = bracketMatrix(A,1,2)
   C = bracketMatrix(A,2,1);
   C + transpose B == 0
   isGradedSkewSymmetric(A,1,2)
  Text
   The rows of $B$ are indexed by $x^\vee,y^\vee$ and the columns by the
   three duals of the quadratic relations. Each nonzero entry is a Lie element of
   degree three. The reverse table has three rows and two columns.
   Since $-(-1)^{1\cdot 2}=-1$, the sum $C+B^{\mathsf T}$ vanishes entry by entry.
SeeAlso
 isGradedJacobi
 isLieSuperalgebra
///

doc ///
Key
 isGradedJacobi
Headline
 Check the graded Jacobi identity in three Lie degrees
Usage
 isGradedJacobi(A, d, e, f)
Inputs
 A:DGAlgebra
 d:ZZ
 e:ZZ
 f:ZZ
Outputs
 :Boolean
Description
  Text
   The graded Jacobi identity can be written $[u,[v,w]]=[[u,v],w]+(-1)^{de}[v,[u,w]]$ for
   $u\in\pi^d$, $v\in\pi^e$, and $w\in\pi^f$. This test checks every basis triple in those
   degrees.
  Text
   In $R=\mathbb Q[x,y]/(x^2,xy)$ the syzygy $x(xy)-y(x^2)=0$
   gives a generator $u=T_{2,1}^\vee$ in Lie degree three. Take $a=x^\vee$ and
   $b=y^\vee$. The cyclic Jacobi sum on $(a,a,b)$ has three terms of the same sign
   because all three inputs are odd.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y), EndDegree=>1);
   a = x_S; b = y_S;
   j = {bracket(A,{a,bracket(A,{a,b})}), bracket(A,{a,bracket(A,{b,a})}), bracket(A,{b,bracket(A,{a,a})})}
   sum j
   isGradedJacobi(A,1,1,1)
  Text
   The terms are $u,u,-2u$. The factor $2$ comes from $[a,a]=2T_{1,1}^\vee$.
   Their sum vanishes, and @TT "true"@ checks the identity for every degree-one basis triple.
SeeAlso
 isGradedSkewSymmetric
 isLieSuperalgebra
///

doc ///
Key
 (isGradedJacobi, DGAlgebra, ZZ, ZZ, ZZ)
Headline
 Check the graded Jacobi identity in three Lie degrees
Usage
 isGradedJacobi(A, d, e, f)
Inputs
 A:DGAlgebra
 d:ZZ
 e:ZZ
 f:ZZ
Outputs
 :Boolean
Description
  Text
   The graded Jacobi identity can be written $[u,[v,w]]=[[u,v],w]+(-1)^{de}[v,[u,w]]$ for
   $u\in\pi^d$, $v\in\pi^e$, and $w\in\pi^f$. This test checks every basis triple in those
   degrees.
  Text
   To see the sign for an even first input, take the degree-two dual
   $q=T_{1,2}^\vee$ of the relation $xy$ and use $a=x^\vee$ twice.
   Building one more stage of the model supplies the degree-four target.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y), EndDegree=>2);
   q = (allgens(A,1))#1; a = x_S;
   lhs = bracket(A,{q,bracket(A,{a,a})});
   firstTerm = bracket(A,{bracket(A,{q,a}),a});
   secondTerm = bracket(A,{a,bracket(A,{q,a})});
   {lhs, firstTerm, secondTerm}
   isGradedJacobi(A,2,1,1)
  Text
   Writing $s=T_{3,1}^\vee$, the three displayed values are $-2s,-s,-s$.
   Here $(-1)^{2\cdot 1}=1$, so the two terms on the right add to $-2s$.
   The checker verifies the same formula for every basis triple of degrees $(2,1,1)$.
SeeAlso
 isGradedSkewSymmetric
 isLieSuperalgebra
///

doc ///
Key
 isLieSuperalgebra
Headline
 Check the binary Lie-superalgebra identities
Usage
 isLieSuperalgebra(A, n)
 isLieSuperalgebra G
Inputs
 A:DGAlgebra
 n:ZZ
 G:HomotopyLie
Outputs
 :Boolean
Description
  Text
   This function checks graded skew-symmetry and Jacobi for the binary bracket. The
   DG-model method restricts the degrees of its inputs by $n$; the finite-coordinate method
   checks the entire stored Lie algebra $G$. Small-characteristic bracket identities are
   included where required.
  Text
   Take $R=\mathbb Q[x,y]/(x^2,xy)$. The relation between the two equations,
   $x(xy)-y(x^2)=0$, gives a degree-three Lie generator $u=T_{2,1}^\vee$.
   We can see Jacobi in that one-dimensional space before running the checker.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y), EndDegree=>1);
   a = x_S; b = y_S;
   {bracket(A,{a,a}), bracket(A,{a,b})}
   leftTerm = bracket(A,{a,bracket(A,{a,b})});
   rightTerm = bracket(A,{bracket(A,{a,a}),b});
   {leftTerm, rightTerm}
   2*leftTerm == rightTerm
  Text
   The first line of output reads $[a,a]=2T_{1,1}^\vee$ and $[a,b]=T_{1,2}^\vee$.
   Since $a$ is odd, graded Jacobi gives $2[a,[a,b]]=[[a,a],b]$.
   The second output shows $u$ on the left before doubling and $2u$ on the right.
  Example
   isLieSuperalgebra(A,1)
   G = homotopyLieAlgebra(A,GradeLimit=>3);
   isLieSuperalgebra G
  Text
   The first @TT "true"@ checks every pair and triple of degree-one inputs in $A$.
   Their brackets can reach degrees two and three, as the displayed calculation shows.
   The second checks every stored basis degree in the quotient $G$ through degree three.
   For identities involving higher brackets, see @TO isLInfinity@.
SeeAlso
 (isLieSuperalgebra, DGAlgebra, ZZ)
 (isLieSuperalgebra, HomotopyLie)
 isLInfinity
///

doc ///
Key
 (isLieSuperalgebra, DGAlgebra, ZZ)
Headline
 Check binary Lie identities within a model input bound
Usage
 isLieSuperalgebra(A, n)
Inputs
 A:DGAlgebra
 n:ZZ
   largest Lie degree of the input generators
Outputs
 :Boolean
Description
  Text
   The test checks skew-symmetry for every pair of input degrees from $1$ through $n$, and
   Jacobi for every such triple. Intermediate and output brackets use all generators
   available in $A$.
  Text
   In characteristic two the test also checks $[u,u]=0$ on even basis elements.
   It tests the binary bracket only. The full homotopy Lie algebra in this
   characteristic also has a squaring operation on odd elements.
  Text
   In characteristic three an odd element $z$ must also satisfy $[z,[z,z]]=0$.
   Jacobi on $(z,z,z)$ gives only $3[z,[z,z]]=0$. Take $z=x^\vee+y^\vee$
   over $\mathbb F_3[x,y]/(x^2,xy)$ and split the outer bracket into its two summands.
  Example
   S = (ZZ/3)[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y), EndDegree=>1);
   a = x_S; b = y_S; z = a+b;
   zz = bracket(A,{z,z})
   {bracket(A,{a,zz}), bracket(A,{b,zz})}
   bracket(A,{z,zz})
   isLieSuperalgebra(A,1)
  Text
   The self-bracket is $-T_{1,1}^\vee-T_{1,2}^\vee$, since $2=-1$ in this field.
   The two outer terms are $-T_{2,1}^\vee$ and $T_{2,1}^\vee$, so they cancel.
   The bound $1$ selects degree-one basis inputs for the checker, including the
   additional odd-element test on the basis. The displayed sum illustrates it on $a+b$.
SeeAlso
 isGradedJacobi
 isGradedSkewSymmetric
 (isLieSuperalgebra, HomotopyLie)
///

doc ///
Key
 (isLieSuperalgebra, HomotopyLie)
Headline
 Check all identities in a finite Lie algebra
Usage
 isLieSuperalgebra G
Inputs
 G:HomotopyLie
Outputs
 :Boolean
Description
  Text
   This method checks graded skew-symmetry and Jacobi on the full stored basis of $G$. The
   computation uses the bracket projected to $G$'s chosen degree range. In characteristic
   $3$ it also tests $[u,[u,u]]=0$ on odd basis elements.
  Text
   Compare the quotients through Lie degrees two and three for
   $R=\mathbb Q[x,y]/(x^2,xy)$. The relation $xy$ survives in both quotients.
   Its syzygy with $x^2$ first appears in degree three.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y), EndDegree=>1);
   G = homotopyLieAlgebra(A,GradeLimit=>3);
   Q = homotopyLieAlgebra(A,GradeLimit=>2);
   a = x_S; b = y_S;
   lieRepresentative(G,bracket(G,{a,bracket(G,{a,b})}))
   lieRepresentative(Q,bracket(Q,{a,bracket(Q,{a,b})}))
   {isLieSuperalgebra G, isLieSuperalgebra Q}
  Text
   In $G$ the iterated bracket $[a,[a,b]]$ is $T_{2,1}^\vee$.
   In $Q$ the inner bracket still represents $T_{1,2}^\vee$, but the outer
   bracket projects to zero. Each Boolean verifies the identities with the
   bracket and basis of its own finite quotient.
SeeAlso
 homotopyLieAlgebra
 (bracket, HomotopyLie, List)
///

doc ///
Key
 HomotopyLie
Headline
 A finite graded homotopy Lie algebra over a field
Description
  Text
   A @TO HomotopyLie@ object stores an ordered dual-generator basis, its mathematical
   degrees, and the binary bracket over the coefficient field. It is built from a minimal
   DG model by @TO homotopyLieAlgebra@. A model generator of homological degree $h$
   represents a Lie generator of degree $h+1$.
  Example
   S = QQ[x];
   A = koszulComplexDGA ideal(x^2);
   G = homotopyLieAlgebra A
   gens G
   degrees G
   V = coordinateSpace G;
   bracket(G, {V_0,V_0})
  Text
   The basis is $(x^\vee,T^\vee)$, the degree vectors are $(1,-1)$ and $(2,-2)$, and the
   bracket output is $2e_1$. Internal degrees are negated under duality when the
   differential preserves them; otherwise only cohomological degrees are recorded. A degree
   bound gives the quotient by higher Lie degrees. The object records its source
   differential and must be reconstructed after that differential changes. Characteristic
   $2$ is excluded because this type does not implement odd squaring.
SeeAlso
 homotopyLieAlgebra
 lieCoordinates
 (gens, HomotopyLie)
 (basis, HomotopyLie)
///

doc ///
Key
 homotopyLieAlgebra
Headline
 Construct a finite graded homotopy Lie algebra
Usage
 G = homotopyLieAlgebra A
 Q = homotopyLieAlgebra(A, GradeLimit => n)
Inputs
 A:DGAlgebra
   a minimal free DG model
Outputs
 :HomotopyLie
Description
  Text
   The quadratic differential gives the bracket on the dual generators, including the
   polynomial base variables. By default all available generators are retained. With @TO
   GradeLimit@ equal to $n$, the result is the quotient by Lie degrees above $n$.
  Example
   S = QQ[x];
   A = koszulComplexDGA ideal(x^2);
   G = homotopyLieAlgebra A
   dim G
   Q = homotopyLieAlgebra(A, GradeLimit => 1)
   dim Q
   q = (gens Q)#0;
   bracket(Q, {q,q})
  Text
   The full algebra has dimension $2$ and its degree-one quotient has dimension $1$. The
   displayed quotient bracket is zero because $T^\vee$, the target of $[x^\vee,x^\vee]$,
   has degree $2$. The constructor requires a free graded-commutative model over a field, a
   degree $-1$ differential with square zero and no constant or linear terms, and
   characteristic different from $2$. It stores a finite snapshot of the supplied model.
  Text
   @SUBSECTION "A ring with square-zero maximal ideal"@

   The quotient $R=\mathbb{Q}[x,y]/(x^2,xy,y^2)$ is not a complete
   intersection. Its first syzygies give new Lie generators in degree
   $3$, and its degree-two Lie elements need not be central.
   Compute a model through those generators:
  Example
   S = QQ[x,y];
   I = ideal(x^2,x*y,y^2);
   A = acyclicClosure(koszulComplexDGA I, EndDegree => 1);
   G = homotopyLieAlgebra(A, GradeLimit => 3);
   g = gens G
   apply({1,2,3}, d -> numcols basis(d,G))
  Text
   The dimensions are $2,3,2$. In the displayed basis order, write
   $\alpha,\beta$ for the degree-one elements, $p,q,r$ for the
   degree-two elements, and $u,v$ for the degree-three elements.
   Their model representatives are
   $x,y,T_{1,1},T_{1,2},T_{1,3},T_{2,1},T_{2,2}$.
   The new differentials are
   $$d(T_{2,1})=xT_{1,2}-yT_{1,1},\qquad
     d(T_{2,2})=xT_{1,3}-yT_{1,2}.$$
   These syzygies supply the brackets with the degree-two elements.
  Example
   bracketMatrix(G, 1, 1)
   bracketMatrix(G, 1, 2)
  Text
   The first matrix is
   $$\begin{pmatrix}2&0&0&0\\0&1&1&0\\0&0&0&2\end{pmatrix}.$$
   Its columns correspond to
   $(\alpha,\alpha),(\alpha,\beta),(\beta,\alpha),(\beta,\beta)$,
   and its rows to $p,q,r$. Thus
   $[\alpha,\alpha]=2p$, $[\alpha,\beta]=q$, and $[\beta,\beta]=2r$.

   The second matrix has rows $u,v$ and columns
   $(\alpha,p),(\alpha,q),(\alpha,r),(\beta,p),(\beta,q),(\beta,r)$:
   $$\begin{pmatrix}0&1&0&-1&0&0\\0&0&1&0&-1&0\end{pmatrix}.$$
   In particular, $[\alpha,q]=u$ and $[\beta,p]=-u$ are nonzero.
   The same coefficients can be retrieved by their zero-based basis
   indices; converting back to model representatives keeps the output
   compact:
  Example
   C = structureConstants G;
   lieRepresentative(G, C#(0,3))
   lieRepresentative(G, C#(1,2))
  Text
   These outputs are $T_{2,1}$ and $-T_{2,1}$, representing $u$ and $-u$.
   Graded Jacobi relates the nonzero iterated brackets by
   $2[\alpha,[\alpha,\beta]]=[[\alpha,\alpha],\beta]$:
  Example
   z = bracket(G, {g#0,bracket(G,{g#0,g#1})});
   lieRepresentative(G, z)
   2*z == bracket(G, {bracket(G,{g#0,g#0}),g#1})
   isLieSuperalgebra G
  Text
   The iterated bracket is $u\ne0$, and both checks return
   @TT "true"@. This object retains Lie degrees at most $3$.
   Brackets with targets above that bound are projected to zero;
   the example makes no claim that the full homotopy Lie algebra
   vanishes in higher degrees.
SeeAlso
 "Holonomy over a non-complete-intersection ring"
 HomotopyLie
 (bracket, HomotopyLie, List)
 GradeLimit
///

doc ///
Key
 (homotopyLieAlgebra, DGAlgebra)
Headline
 Construct a finite graded homotopy Lie algebra
Usage
 G = homotopyLieAlgebra A
 Q = homotopyLieAlgebra(A, GradeLimit => n)
Inputs
 A:DGAlgebra
   a minimal free DG model
Outputs
 :HomotopyLie
Description
  Text
   The quadratic differential gives the bracket on the dual generators, including the
   polynomial base variables. By default all available generators are retained. With @TO
   GradeLimit@ equal to $n$, the result is the quotient by Lie degrees above $n$.
  Example
   S = QQ[x];
   A = koszulComplexDGA ideal(x^2);
   G = homotopyLieAlgebra A
   dim G
   Q = homotopyLieAlgebra(A, GradeLimit => 1)
   dim Q
   q = (gens Q)#0;
   bracket(Q, {q,q})
  Text
   The full algebra has dimension $2$ and its degree-one quotient has dimension $1$. The
   displayed quotient bracket is zero because $T^\vee$, the target of $[x^\vee,x^\vee]$,
   has degree $2$. The constructor requires a free graded-commutative model over a field, a
   degree $-1$ differential with square zero and no constant or linear terms, and
   characteristic different from $2$. It stores a finite snapshot of the supplied model.
  Text
   @SUBSECTION "A non-complete-intersection model"@

   For $R=\mathbb{Q}[x,y]/(x,y)^2$, the syzygies among the three
   quadratic relations add Lie degree-three generators. This gives
   a small example where a degree-two Lie element is not central:
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
   G = homotopyLieAlgebra(A, GradeLimit => 3);
   g = gens G;
   apply({1,2,3}, d -> numcols basis(d,G))
   lieRepresentative(G, bracket(G,{g#0,g#3}))
   lieRepresentative(G, bracket(G,{g#1,g#2}))
   isLieSuperalgebra G
  Text
   The dimensions are $2,3,2$. The basis is ordered as
   $x,y,T_{1,1},T_{1,2},T_{1,3},T_{2,1},T_{2,2}$ in model
   representatives. The two brackets return $T_{2,1}$ and
   $-T_{2,1}$ because
   $d(T_{2,1})=xT_{1,2}-yT_{1,1}$.
   The identity check returns @TT "true"@ for this finite Lie quotient.
   The bound $3$ discards higher Lie degrees; it does not assert
   that the complete homotopy Lie algebra is concentrated below it.
SeeAlso
 "Holonomy over a non-complete-intersection ring"
 HomotopyLie
 (bracket, HomotopyLie, List)
 GradeLimit
///

doc ///
Key
 lieCoordinates
Headline
 Convert Lie representatives to coefficient-field vectors
Usage
 lieCoordinates(G, u)
Inputs
 G:HomotopyLie
 u:Thing
   a model linear form or a vector in the coordinate space of G
Outputs
 :Vector
Description
  Text
   The ordered basis of $G$ identifies its elements with vectors over the coefficient
   field. This function converts a scalar linear form in model representatives to those
   coordinates; an existing vector in the same coordinate space is returned unchanged.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2+y^2,x*y);
   G = homotopyLieAlgebra A;
   b = gens G;
   u = b#0+b#1;
   c = bracket(A,{u,u})
   lieCoordinates(G,c)
  Text
   The DG bracket returns $4P+2Q$, with $P,Q$ the model generators for
   $x^2+y^2$ and $xy$. In the ordered basis $(x^\vee,y^\vee,P^\vee,Q^\vee)$
   the coordinates are $(0,0,4,2)^{\mathsf T}$. This converts a bracket
   computed in the model directly to the vector used by $G$.
SeeAlso
 (lieCoordinates, HomotopyLie, RingElement)
 (lieCoordinates, HomotopyLie, Vector)
 lieRepresentative
///

doc ///
Key
 (lieCoordinates, HomotopyLie, RingElement)
Headline
 Convert a model linear form to Lie coordinates
Usage
 lieCoordinates(G, u)
Inputs
 G:HomotopyLie
 u:RingElement
   a scalar linear combination of representatives in gens G
Outputs
 :Vector
Description
  Text
   This method reads the coefficients of a linear form in the representatives returned by
   @TT "gens G"@. The coordinates are ordered exactly as that generator list.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
   G = homotopyLieAlgebra A;
   b = gens G;
   c = bracket(A,{b#1,b#2})
   lieCoordinates(G,c)
  Text
   The model returns $-T_{2,1}$, since its differential is
   $xT_{1,2}-yT_{1,1}$. Its coordinate column has a single $-1$ at
   index $5$ (the sixth position) in the seven-generator basis. Thus
   $[y^\vee,T_{1,1}^\vee]=-T_{2,1}^\vee$.
SeeAlso
 (lieRepresentative, HomotopyLie, Vector)
///

doc ///
Key
 (lieCoordinates, HomotopyLie, Vector)
Headline
 Validate a vector in a finite Lie coordinate space
Usage
 lieCoordinates(G, v)
Inputs
 G:HomotopyLie
 v:Vector
   a vector in coordinateSpace G
Outputs
 :Vector
   the unchanged coordinate vector
Description
  Text
   A vector already expressed in the ordered coefficient-field basis needs no conversion.
   This method checks that it belongs to the coordinate module associated with $G$ and
   returns it unchanged.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2+y^2,x*y);
   G = homotopyLieAlgebra A;
   V = coordinateSpace G;
   u = V_0 - V_1;
   c = bracket(G,{u,u});
   lieCoordinates(G,c)
   lieCoordinates(G,lieRepresentative(G,c)) == c
  Text
   The bracket already has coordinates $(0,0,4,-2)^{\mathsf T}$.
   The last equality gives a second route to that column by converting
   the representative $4P-2Q$ back to coordinates.
SeeAlso
 (lieCoordinates, HomotopyLie, RingElement)
 (coordinateSpace, HomotopyLie)
///

doc ///
Key
 lieRepresentative
Headline
 Convert a Lie coordinate vector to a model representative
Usage
 lieRepresentative(G, v)
Inputs
 G:HomotopyLie
 v:Vector
   a vector in coordinateSpace G
Outputs
 :RingElement
   the corresponding linear form in model generators
Description
  Text
   This is the inverse of @TO lieCoordinates@. It replaces each coordinate basis vector by
   the matching model-generator representative from @TT "gens G"@.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2+y^2,x*y);
   G = homotopyLieAlgebra A;
   V = coordinateSpace G;
   c = bracket(G,{V_0-V_1,V_0-V_1})
   lieRepresentative(G,c)
  Text
   The last two entries of the column are $4$ and $-2$. Converting
   it gives $4P-2Q$, where $dP=x^2+y^2$ and $dQ=xy$. This identifies
   which relations occur in the self-bracket of $x^\vee-y^\vee$.
SeeAlso
 lieCoordinates
 (gens, HomotopyLie)
///

doc ///
Key
 (lieRepresentative, HomotopyLie, Vector)
Headline
 Convert a Lie coordinate vector to a model representative
Usage
 lieRepresentative(G, v)
Inputs
 G:HomotopyLie
 v:Vector
   a vector in coordinateSpace G
Outputs
 :RingElement
   the corresponding linear form in model generators
Description
  Text
   This is the inverse of @TO lieCoordinates@. It replaces each coordinate basis vector by
   the matching model-generator representative from @TT "gens G"@.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
   G = homotopyLieAlgebra A;
   V = coordinateSpace G;
   c = bracket(G,{V_1,V_2});
   u = lieRepresentative(G,c)
   diff(A,u)
  Text
   The bracket of the duals of $y$ and $T_{1,1}$ returns the negative
   of the first syzygy dual. Its representative is $-T_{2,1}$.
   Applying the model differential gives $yT_{1,1}-xT_{1,2}$, the
   signed syzygy corresponding to this choice of representative.
SeeAlso
 lieCoordinates
 (gens, HomotopyLie)
///

doc ///
Key
 structureConstants
Headline
 List nonzero brackets and module actions on basis elements
Usage
 structureConstants G
 structureConstants(L,r)
 structureConstants(LM,r)
Outputs
 :HashTable
   a table of nonzero basis values
Description
  Text
   For an ordered basis $b_0,\ldots,b_{m-1}$, the entry with key $(i,j)$ is the full
   coordinate vector of $[b_i,b_j]$. Zero brackets are omitted. The basis indices begin at
   zero.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2+y^2,x*y);
   G = homotopyLieAlgebra A;
   C = structureConstants G
   sort keys C
   apply({(0,0),(0,1),(1,0),(1,1)}, k -> lieRepresentative(G,C#k))
  Text
   Only four ordered pairs survive. Their values in the displayed
   order are $2P,Q,Q,2P$ with $dP=x^2+y^2$ and $dQ=xy$. The two
   mixed entries agree because the input Lie degrees are both odd.
   No key involves a degree-two generator in this complete intersection.
  Text
   For higher operations, specify the total arity. In the quartic hypersurface
   the fourth bracket and fourth module action both involve the dual of $x$.
  Example
   S = QQ[x];
   A = koszulComplexDGA ideal(x^4);
   LM = lInfinityModule(A,coker vars S,EndDegree=>1);
   L = lieAlgebra LM;
   structureConstants(L,4)
   structureConstants(LM,4)
  Text
   The Lie key @TT "{0,0,0,0}"@ represents four copies of $x^\vee$.
   In the module key @TT "({0,0,0},1)"@ the last index selects the second
   module generator. The values are $24T$ and $6e_2$, respectively.
SeeAlso
 (structureConstants,LInfinity,ZZ)
 (structureConstants,LInfinityModule,ZZ)
 [structureConstants,DividedPower]
 (bracketMatrix, HomotopyLie, ZZ, ZZ)
 (bracket, HomotopyLie, List)
///

doc ///
Key
 (structureConstants, HomotopyLie)
Headline
 List the nonzero binary brackets in finite coordinates
Usage
 structureConstants G
Inputs
 G:HomotopyLie
Outputs
 :HashTable
   nonzero bracket vectors keyed by ordered basis-index pairs
Description
  Text
   For an ordered basis $b_0,\ldots,b_{m-1}$, the entry with key $(i,j)$ is the full
   coordinate vector of $[b_i,b_j]$. Zero brackets are omitted. The basis indices begin at
   zero.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
   G = homotopyLieAlgebra A;
   C = structureConstants G
   apply({(0,3),(3,0),(1,2),(2,1)}, k -> lieRepresentative(G,C#k))
   C#?(0,2)
  Text
   These four brackets have representatives $U,-U,-U,U$, where
   $dU=xT_{1,2}-yT_{1,1}$. The two coefficients in $dU$ give
   opposite signs; switching an input of degree one with an input of
   degree two changes the sign again. The last output is false because
   $[x^\vee,T_{1,1}^\vee]=0$, so the sparse table omits that key.
SeeAlso
 (bracketMatrix, HomotopyLie, ZZ, ZZ)
 (bracket, HomotopyLie, List)
///

doc ///
Key
 (gens, HomotopyLie)
Headline
 List the ordered Lie-basis representatives
Description
  Text
   Call @TT "gens G"@ to use this method.

   The returned model generators represent the ordered dual basis of $G$. Polynomial base
   variables represent Lie degree-one elements; adjoined generators of homological degree
   $h$ represent Lie degree $h+1$.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
   G = homotopyLieAlgebra(A,GradeLimit=>3);
   b = gens G
   apply(take(b,-2), T -> diff(A,T))
  Text
   Write $P,Q,R$ for the three generators following $x,y$. The last
   two entries represent the degree-three dual basis. Their differentials
   $xQ-yP$ and $xR-yQ$ identify which first syzygy each basis element
   comes from. This is the same order used by the rows of the degree
   $(1,2)$ bracket matrix.
SeeAlso
 (basis, HomotopyLie)
 (degrees, HomotopyLie)
///

doc ///
Key
 (degrees, HomotopyLie)
Headline
 Read the mathematical degrees of the ordered Lie basis
Usage
 degrees G
Inputs
 G:HomotopyLie
Outputs
 :List
   cohomological degrees and any preserved dual internal multidegrees
Description
  Text
   Each entry begins with the cohomological Lie degree. If the model differential preserves
   its internal multigrading, the remaining entries are the negatives of the corresponding
   model degrees. Otherwise only the cohomological degree is returned.
  Example
   S = QQ[x,y,Degrees=>{{1,0},{0,1}}];
   A = koszulComplexDGA ideal(x*y);
   G = homotopyLieAlgebra A;
   degrees G
   b = gens G;
   lieRepresentative(G,bracket(G,{b#0,b#1}))
  Text
   The generator degrees are $(1,-1,0)$, $(1,0,-1)$, and $(2,-1,-1)$.
   The first coordinate records Lie degree, and the last two record the
   dual internal bidegree. The mixed bracket returns $T$, whose degree
   in the Lie algebra is precisely the sum of the first two triples.
SeeAlso
 (gens, HomotopyLie)
 (basis, ZZ, HomotopyLie)
///

doc ///
Key
 (ring, HomotopyLie)
Headline
 Return the coefficient field of a finite Lie algebra
Usage
 ring G
Inputs
 G:HomotopyLie
Outputs
 :Ring
   the coefficient field
Description
  Text
   A finite homotopy Lie algebra is a graded vector space over the coefficient field of its
   minimal DG model. This accessor returns that field, which is also the coefficient ring
   of its coordinate vectors and scalar bracket matrices.
  Example
   k = ZZ/3;
   S = k[x];
   A = koszulComplexDGA ideal(x^2);
   G = homotopyLieAlgebra A;
   ring G
   V = coordinateSpace G;
   bracket(G,{V_0,V_0}) == -V_1
  Text
   The field is $\mathbb F_3$. The odd self-bracket is twice the
   relation dual, and $2=-1$ in this field, which gives the displayed
   equality. The field returned by this method controls the arithmetic
   of the bracket coordinates.
SeeAlso
 (coordinateSpace, HomotopyLie)
 (bracketMatrix, HomotopyLie, ZZ, ZZ)
///

doc ///
Key
 (dim, HomotopyLie)
Headline
 Count the stored Lie-basis generators
Usage
 dim G
Inputs
 G:HomotopyLie
Outputs
 :ZZ
   total vector-space dimension
Description
  Text
   The dimension is the number of generators in the stored finite Lie basis, summed over
   its retained degrees. It is not the Krull dimension of the presentation ring and does
   not count omitted higher-degree generators.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2,y^2);
   G = homotopyLieAlgebra A;
   dim G
   apply({1,2}, d -> numcols basis(d,G))
  Text
   The total is $4$, split as $2$ generators in Lie degree $1$ and $2$ in degree $2$. The
   basis order is $(x^\vee,y^\vee,T_1^\vee,T_2^\vee)$.
SeeAlso
 (basis, ZZ, HomotopyLie)
 homotopyLieAlgebra
///

doc ///
Key
 (basis, HomotopyLie)
Headline
 Return all coordinate basis columns of a finite Lie algebra
Description
  Text
   Call @TT "basis G"@ to use this method.

   The ordered Lie basis identifies $G$ with a finite-dimensional coefficient-field vector
   space. This method returns its basis as the columns of an identity matrix; the order
   matches @TT "gens G"@.
  Example
   S = QQ[x];
   A = koszulComplexDGA ideal(x^2);
   G = homotopyLieAlgebra A;
   basis G
   lieRepresentative(G, (basis G)_1)
  Text
   The matrix is $I_2$. Its second column is $(0,1)^{\mathsf T}$ and converts to the
   representative $T$ of $T^\vee$. The grading of these columns is recorded separately by
   @TT "degrees G"@.
SeeAlso
 (basis, ZZ, HomotopyLie)
 (gens, HomotopyLie)
///

doc ///
Key
 (basis, ZZ, HomotopyLie)
Headline
 Select coordinate basis columns of one Lie degree
Description
  Text
   Call @TT "basis(d, G)"@ to use this method.

   This method selects the stored generators of Lie degree $d$. Its columns retain their
   positions in the full coordinate space, so it is an inclusion matrix for the degree-$d$
   component.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x^2+y^2,x*y);
   G = homotopyLieAlgebra A;
   D = basis(2,G)
   V = coordinateSpace G;
   bracket(G,{V_0+V_1,V_0+V_1}) == 4*D_0+2*D_1
   numcols basis(3,G)
  Text
   The two columns of $D$ are the third and fourth ambient basis
   vectors, representing $P^\vee$ and $Q^\vee$. The equality expresses a
   self-bracket entirely in the basis of its degree-two component. The
   last output is zero because the complete intersection has no
   degree-three generator.
SeeAlso
 (basis, HomotopyLie)
 (degrees, HomotopyLie)
///

doc ///
Key
 (net, HomotopyLie)
Headline
 Display the size and degree bound of a finite Lie algebra
Usage
 net G
Inputs
 G:HomotopyLie
Outputs
 :Net
   the formatted summary
Description
  Text
   The displayed summary reports the number of retained Lie generators and the upper
   Lie-degree bound. It is a description of the stored finite object; it does not assert
   that the full homotopy Lie algebra vanishes above that bound.
  Example
   S = QQ[x,y];
   A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
   G = homotopyLieAlgebra(A,GradeLimit=>2);
   H = homotopyLieAlgebra(A,GradeLimit=>3);
   net G
   net H
  Text
   Raising the bound from $2$ to $3$ changes the summary from five
   generators to seven. The two new entries are dual to the first
   syzygies among $x^2,xy,y^2$. The bound in each display records
   how much of the Lie algebra the object retains.
SeeAlso
 (dim, HomotopyLie)
 (degrees, HomotopyLie)
///

doc ///
Key
 (coordinateSpace, HomotopyLie)
Headline
 Return the vector space of finite Lie coordinates
Usage
 coordinateSpace G
Inputs
 G:HomotopyLie
Outputs
 :Module
   the coefficient-field coordinate space
Description
  Text
   For $m$ stored generators, this accessor returns the free coefficient-field module $k^m$
   used for Lie coordinate vectors. Its ordered basis is identified with @TT "gens G"@; the
   mathematical grading is supplied separately by @TT "degrees G"@.
  Example
   S = QQ[x,y];
   A = koszulComplexDGA ideal(x*y);
   G = homotopyLieAlgebra A;
   V = coordinateSpace G
   u = V_0+V_1;
   v = V_0-V_1;
   bracket(G,{u,u})
   bracket(G,{u,v})
  Text
   The basis of $V=\mathbb Q^3$ represents $x^\vee,y^\vee,T^\vee$.
   The self-bracket of $u$ gives $2e_2$ because $dT=xy$. Pairing
   $u$ with $v$ instead gives the zero vector because the two mixed
   terms cancel.
SeeAlso
 (basis, HomotopyLie)
 lieCoordinates
///

doc ///
Key
 LInfinity
Headline
 Higher brackets carried by a minimal DG model
Description
 Text
  An @TO LInfinity@ represents a minimal $L_\infty$ algebra
  $(\pi,\ell_2,\ell_3,\ldots)$ with $\ell_1=0$.
  Its operations are obtained by dualizing the word-length components
  of a minimal DG-model differential. An operation $\ell_r$ has
  cohomological degree $2-r$.

  The model variables label dual Lie generators. To find inputs, use
  @TO (gens,LInfinity)@; to interpret their Lie degrees, use
  @TO (degrees,LInfinity)@. For the cubic hypersurface, the only
  nonzero generator differential is $d(T)=x^3$:
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,GradeLimit=>2)
  g = gens L;
  a = g#0; t = g#1;
  class L
  degrees L
  bracket(L,{a,a,a})
 Text
  The type is @TT "LInfinity"@. The degree list is
  @TT "{{1,-1},{2,-3}}"@: $a=x^\vee$ has Lie degree $1$ and
  $t=T^\vee$ has Lie degree $2$, with negated internal degrees.
  The last output is $6T$, representing $\ell_3(a,a,a)=6t$.
  The factor $3!$ records three repeated algebra inputs.
SeeAlso
 lInfinity
 (bracket,LInfinity,List)
 bracketGamma
///

doc ///
Key
 lInfinity
Headline
 Construct the higher Lie operations of a DG model
Usage
 L = lInfinity A
 L = lInfinity LM
Description
 Text
  The decomposition $d=\sum_{r\geq2}d_r$ of a minimal differential
  determines higher brackets $\ell_r$ on the shifted dual generators.
  @TO lInfinity@ packages those operations for evaluation by
  @TO (bracket,LInfinity,List)@.

  The input may be a DG algebra or an existing @TO LInfinityModule@.
  The two input forms are documented separately at
  @TO (lInfinity,DGAlgebra)@ and @TO (lInfinity,LInfinityModule)@.
 Example
  S = QQ[x,y,Degrees=>{3,2}];
  A = koszulComplexDGA ideal(x^2-y^3);
  L = lInfinity A
  (a,b,t) = toSequence gens L;
  diff(A,t)
  bracket(L,{a,a})
  bracket(L,{b,b,b})
 Text
  The equation of the cusp gives $d(T)=x^2-y^3$. Its two terms
  land in the same dual basis direction in different arities.
  Writing $a=x^\vee$, $b=y^\vee$ and $t=T^\vee$, the last two
  outputs give $\ell_2(a,a)=2t$ and $\ell_3(b,b,b)=-6t$.
  The list length chooses the word length in the differential.
  Repeated inputs account for the factors $2!$ and $3!$.
SeeAlso
 (lInfinity,DGAlgebra)
 (lInfinity,LInfinityModule)
 LInfinity
///

doc ///
Key
 (lInfinity,DGAlgebra)
Headline
 Construct higher brackets directly from a minimal DG algebra
Usage
 L = lInfinity(A,GradeLimit=>d)
Description
 Text
  For a minimal free DG algebra $A$, this method constructs the
  $L_\infty$ operations dual to its differential. The homological
  grading is nonnegative, the differential has degree $-1$ and
  squares to zero, and its generator images have no constant or
  linear terms. These conditions give the minimal uncurved case
  $\ell_0=\ell_1=0$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,GradeLimit=>2)
  g = gens L;
  a = g#0;
  gens L
  bracket(L,{a,a,a})
  isLInfinity(L,4)
 Text
  The representatives are $x,T$, with $d(T)=x^3$.
  The ternary bracket is $6T$, and the identity check returns
  @TT "true"@ through total arity $4$ on inputs of Lie degree at most $2$.

  @TO GradeLimit@ bounds identity-checking inputs; it does not
  truncate direct bracket evaluation. @TO OrderLimit@ is stored
  as metadata. All operations use the generators present in $A$.
 Text
  The underlying algebra must be free and graded-commutative over a
  coefficient field. Quotient presentations and nonfield bases are
  rejected; the differential must have degree $-1$, square to zero,
  and have no constant or linear part in the flattened generators.
SeeAlso
 lInfinity
 LInfinity
 (bracket,LInfinity,List)
///

doc ///
Key
 (lInfinity,LInfinityModule)
Headline
 Recover algebra-side higher brackets from module data
Usage
 L = lInfinity LM
 L = lInfinity(LM,GradeLimit=>d)
Description
 Text
  An $L_\infty$ module has an underlying $L_\infty$ algebra.
  This method constructs algebra-side data for the same DG model
  used by @TT "LM"@, without rebuilding its semifree resolution.
  Options passed to this call apply to the returned algebra object.
  Use @TO (lieAlgebra,LInfinityModule)@ to retrieve the algebra with the
  bounds already stored in the module.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  LM = lInfinityModule(A,M,EndDegree=>1,GradeLimit=>2);
  L = lInfinity(LM,GradeLimit=>2)
  g = gens L; e = gens LM;
  a = g#0;
  gens L
  bracket(L,{a,a,a})
  mu(LM,{a,a},e#1)
 Text
  The Lie representatives are $x,T$. The algebra operation is
  $\ell_3(x^\vee,x^\vee,x^\vee)=6T^\vee$; the module operation is
  $\mu_3(x^\vee,x^\vee,e_1^\vee)=2e_2^\vee$.
  The list in @TO bracket@ contains three Lie inputs. In @TO mu@ the
  list contains two Lie inputs, and @TT "e#1"@ supplies the module input.
SeeAlso
 (lieAlgebra,LInfinityModule)
 lInfinity
 LInfinityModule
 (bracket,LInfinity,List)
 mu
///

doc ///
Key
 (bracket,LInfinity,List)
Headline
 Evaluate a higher Lie bracket on a list of inputs
Usage
 v = bracket(L,{u_1,...,u_r})
Description
 Text
  With $r\geq2$ list entries, this method evaluates
  $\ell_r(u_1,\ldots,u_r)$. Each input is a coefficient-field
  linear combination of the representatives in @TT "gens L"@.
  The output is another linear form in those representatives,
  interpreted as a dual Lie element.
 Example
  S = QQ[x,y];
  A = koszulComplexDGA ideal(x^2*y-x*y^2);
  L = lInfinity A;
  (a,b,t) = toSequence gens L;
  bracket(L,{a,a,b})
  bracket(L,{a,b,b})
  bracket(L,{a+b,a+b,a+b})
 Text
  Write $t=T^\vee$ for the dual of the Koszul generator.
  The monomials $x^2y$ and $-xy^2$ give $2t$ and $-2t$,
  respectively. The inputs $a,b$ have odd Lie degree, so permutations
  of either triple give the same value. Expanding the last bracket
  gives $3(2t)+3(-2t)=0$. Thus a zero on the diagonal can conceal
  two nonzero mixed brackets.

  For homogeneous model representatives, evaluation includes
  the suspension sign
  $$(-1)^\varepsilon,\qquad
  \varepsilon=|u_{r-1}|+|u_{r-3}|+\cdots,$$
  where $|\cdot|$ denotes homological degree.
  Graded reordering signs and multiplicity factors $\prod_v e_v!$
  are included. The operation has cohomological degree $2-r$.
SeeAlso
 LInfinity
 (gens,LInfinity)
 bracketGamma
///

doc ///
Key
 bracketGamma
Headline
 Evaluate divided-power higher Lie operations
Usage
 v = bracketGamma(L,{u_1,...,u_r})
Description
 Text
  Divided-power coordinates retain coefficients that ordinary
  repeated-input evaluation multiplies by factorials.
  @TO bracketGamma@ evaluates a divided-power basis monomial
  encoded by a list of model-generator representatives.
  For multiplicities $e_v$, ordinary @TO bracket@ is
  $\prod_v e_v!$ times this normalized value on the same basis list.
 Example
  S = QQ[x,y];
  A = koszulComplexDGA ideal(x^2*y);
  L = lInfinity A;
  (a,b,t) = toSequence gens L;
  bracketGamma(L,{a,a,b})
  bracket(L,{a,a,b})
 Text
  The first output represents $t=T^\vee$, since the coefficient
  of $x^2y$ in $d(T)$ is $1$. The ordinary bracket returns $2t$.
  Although three inputs occur, only $a$ repeats, so the factor
  is $2!\,1!$. The list records a divided-power basis monomial.
  Each algebra slot accepts a scalar multiple of one basis generator.
SeeAlso
 (bracketGamma,LInfinity,List)
 (bracket,LInfinity,List)
 DividedPower
///

doc ///
Key
 (bracketGamma,LInfinity,List)
Headline
 Read a divided-power bracket from its basis multiplicities
Usage
 v = bracketGamma(L,{u_1,...,u_r})
Description
 Text
  A list with $r\geq2$ entries specifies a degree-$r$ divided-power
  basis input. Each nonzero entry must be a scalar multiple of a
  single Lie basis representative. Its repetitions determine the
  multiplicities; scalar coefficients multiply the resulting value.
  Suspension and graded reordering signs are retained, but repeated
  inputs carry no factorial multiplier.

  In characteristic $3$, the cubic coefficient survives this
  normalization even though the ordinary ternary bracket vanishes:
 Example
  S = ZZ/3[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,GradeLimit=>2);
  g = gens L;
  a = g#0; t = g#1;
  bracket(L,{a,a,a})
  bracketGamma(L,{a,a,a})
 Text
  The outputs are $0$ and $T$.
  Since $3!=0$ in the coefficient field, the ordinary operation
  loses this repeated-input coefficient. The divided-power output
  records the coefficient $1$ of $x^3$ in $d(T)$.
  General linear combinations in the algebra slots would require
  the divided-power addition law and are not accepted by this method.
SeeAlso
 bracketGamma
 (gens,LInfinity)
 DividedPower
///

doc ///
Key
 isLInfinity
Headline
 Check the higher Lie identities in a computed model
Usage
 isLInfinity(L,n)
Description
 Text
  The $L_\infty$ identities relate composites of the operations
  $\ell_r$. For a minimal model, the arity-$3$ identity is graded
  Jacobi for $\ell_2$; larger arities involve higher operations.
  @TO isLInfinity@ checks a finite range of those identities.
 Example
  S = QQ[x,y];
  A = freeDGAlgebra(S,{{1,0},{1,0},{2,0}},Variable=>{u,v,w});
  (U,V,W) = toSequence gens A.natural;
  setDiff(A,{x_S^2,x_S*y_S^2,y_S^2*U-x_S*V},InitializeDegreeZeroHomology=>false,InitializeComplex=>false);
  L = lInfinity(A,GradeLimit=>1);
  (a,b) = toSequence take(gens L,2);
  bracket(L,{bracket(L,{a,a}),b,b})
  bracket(L,{bracket(L,{a,b,b}),a})
  isLInfinity(L,4)
 Text
  Here $d(u)=x^2$, $d(v)=xy^2$ and $d(w)=y^2u-xv$.
  Writing $a=x^\vee$ and $b=y^\vee$, the first two outputs give
  $$\ell_3(\ell_2(a,a),b,b)=4w^\vee,\qquad
  \ell_2(\ell_3(a,b,b),a)=2w^\vee.$$
  On the tuple $(a,a,b,b)$ there is one way to select both copies of
  $a$ for the inner binary bracket. There are two ways to select
  one $a$ and both copies of $b$ for the inner ternary bracket.
  The arity-four identity subtracts the latter two contributions,
  giving $4w^\vee-2w^\vee-2w^\vee=0$. This is the bracket form of
  $d^2(w)=y^2x^2-x(xy^2)=0$. The final @TT "true"@ includes all
  tuples of Lie-degree-one basis elements through arity $4$.
 Text
  In small positive characteristic, factorials in the multilinear
  operations may vanish. A successful classical check can therefore
  miss information in the divided-power structure even within these
  bounds. Use @TT "DividedPower=>true"@ for the intrinsic check
  described on the @TO DividedPower@ page.
SeeAlso
 (isLInfinity,LInfinity,ZZ)
 LInfinity
 DividedPower
///

doc ///
Key
 (isLInfinity,LInfinity,ZZ)
Headline
 Verify generalized Jacobi identities through a given arity
Usage
 isLInfinity(L,n)
 isLInfinity(L,n,DividedPower=>true)
Description
 Text
  For each $3\leq q\leq n$, the classical mode checks the
  generalized Jacobi sum on all available basis tuples whose Lie
  grades are between $1$ and the stored @TO GradeLimit@.
  Terms combine $\ell_j(\ell_i(\cdots),\ldots)$ with $i+j=q+1$,
  signed unshuffles, and the factor $(-1)^{i(j-1)}$.
 Example
  S = ZZ/2[x,y];
  A = freeDGAlgebra(S,{{1,0},{1,0},{2,0}},Variable=>{u,v,w});
  (U,V,W) = toSequence gens A.natural;
  setDiff(A,{x_S^2,x_S*y_S^2,y_S^2*U-x_S*V},InitializeDegreeZeroHomology=>false,InitializeComplex=>false);
  L = lInfinity(A,GradeLimit=>1);
  (a,b) = toSequence take(gens L,2);
  bracket(L,{a,a})
  bracketGamma(L,{a,a})
  {diff(A,b^2*U),diff(A,a*V)}
  isLInfinity(L,4,DividedPower=>true)
 Text
  The differential $d(u)=x^2$ gives $\ell_2(a,a)=0$ in
  characteristic two, while its divided-power value is $u^\vee$.
  The two displayed differentials are both $x^2y^2$. They occur
  in $d^2(w)=d(y^2u-xv)$ with opposite signs and cancel.
  Their common word length is $4$. With @TT "DividedPower=>true"@,
  the checker tests these word-length coefficients in the square
  of the differential. Here it returns @TT "true"@ through
  arity $4$ with inputs of Lie degree $1$.

  These checks concern only the requested arities, input grades,
  and generators present in $A$. They do not certify an uncomputed
  part of an infinite resolution.
 Text
  In small positive characteristic, factorials in the multilinear
  operations may vanish. A successful classical check can therefore
  miss information in the divided-power structure even within these
  bounds. Use @TT "DividedPower=>true"@ for the intrinsic check
  described on the @TO DividedPower@ page.
SeeAlso
 isLInfinity
 (bracket,LInfinity,List)
 DividedPower
///

doc ///
Key
 (gens,LInfinity)
Headline
 List representatives of the dual Lie basis
Description
 Text
  Call @TT "g = gens L"@ to use this method.

  The homotopy Lie basis is dual to all model generators, including
  the original polynomial variables of the base ring.
  This method returns those model variables as representatives
  of the dual elements. A model variable of homological degree
  $p-1$ represents a Lie generator of degree $p$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,GradeLimit=>2);
  g = gens L;
  g
  bracket(L,{g#0,g#0,g#0}) == 6*g#1
 Text
  The list is $\{x,T\}$, in that order.
  Its first entry represents $x^\vee$ and its second represents
  $T^\vee$. The equality returns @TT "true"@ and identifies the
  output of the ternary bracket with the second dual basis element.
  Use @TO (degrees,LInfinity)@ for Lie degrees; the native degrees
  of the returned ring elements belong to the DG model.
SeeAlso
 LInfinity
 (degrees,LInfinity)
 (bracket,LInfinity,List)
///

doc ///
Key
 (degrees,LInfinity)
Headline
 Read cohomological and internal degrees of Lie generators
Usage
 degrees L
Description
 Text
  Dualizing a model generator of homological degree $h$ gives a
  Lie generator of cohomological degree $h+1$.
  Each returned degree list starts with this cohomological degree.
  When the differential preserves the internal multigrading,
  its remaining entries are the negated internal degrees.
 Example
  S = QQ[x,y,Degrees=>{3,2}];
  A = koszulComplexDGA ideal(x^2-y^3);
  L = lInfinity A;
  apply(gens L,degree)
  degrees L
 Text
  The first list gives the model degrees $(0,3),(0,2),(1,6)$
  of $x,y,T$. The weights $3$ and $2$ make $d(T)=x^2-y^3$
  homogeneous of internal degree $6$. In the same order, the dual
  degrees are $(1,-3),(1,-2),(2,-6)$. Both the binary bracket
  on two copies of $x^\vee$ and the ternary bracket on three copies
  of $y^\vee$ therefore have the internal degree of $T^\vee$.
SeeAlso
 LInfinity
 (gens,LInfinity)
///

doc ///
Key
 (net,LInfinity)
Headline
 Display the size of a higher Lie model
Usage
 net L
Description
 Text
  A finite DG model specifies higher operations on its stored
  dual Lie generators. This method gives a compact display of
  that generator count and the operation-evaluation syntax.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,GradeLimit=>2);
  g = gens L;
  a = g#0;
  net L
  bracket(L,{a,a,a})
 Text
  The display reads
  @TT "LInfinity{2 Lie generators; use bracket(L,{...})}"@.
  The number $2$ counts $x^\vee,T^\vee$; it is not a maximum arity.
  The next output, $6T$, demonstrates the nonzero ternary operation.
SeeAlso
 LInfinity
 (gens,LInfinity)
 (bracket,LInfinity,List)
///

doc ///
Key
 LInfinityModule
Headline
 Higher module operations on a dual semifree basis
Description
 Text
  A minimal semifree DG module $F$ over $A$ determines operations
  $$\mu_i:\bigwedge^{i-1}\pi\otimes_k M_\infty\longrightarrow M_\infty,
  \qquad M_\infty=(k\otimes_A F)^\vee,\quad i\geq2.$$
  Their cohomological degree is $2-i$, and $\mu_1=0$.
  An @TO LInfinityModule@ stores these operations on the generators
  available in its computed semifree model.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  LM = lInfinityModule(A,M,EndDegree=>1,GradeLimit=>2)
  L = lieAlgebra LM;
  g = gens L; e = gens LM;
  a = g#0;
  class LM
  degrees LM
  entries mu(LM,{a,a},e#1)
 Text
  The type is @TT "LInfinityModule"@.
  The dual module degrees are $(0,0),(1,-1),(2,-3)$, and the
  last list is @TT "{0,0,2}"@. It means
  $\mu_3(x^\vee,x^\vee,e_1^\vee)=2e_2^\vee$.
  The vectors returned by @TO (gens,LInfinityModule)@ label
  the dual semifree generators, not generators of the original
  residue-field module $M$.
SeeAlso
 lInfinityModule
 mu
 (gens,LInfinityModule)
 holonomyRepresentation
///

doc ///
Key
 lInfinityModule
Headline
 Construct higher module operations from a minimal semifree differential
Usage
 LM = lInfinityModule(A,M,EndDegree=>n)
 LM = lInfinityModule F
Description
 Text
  The differential of a minimal semifree resolution separates
  into algebra word-length components. Their duals give the
  operations $\mu_i$ on $(k\otimes_A F)^\vee$.
  Given @TT "(A,M)"@, the constructor builds a finite semifree resolution.
  Given a minimal semifree DG module @TT "F"@, it reads the differential
  in the supplied basis. In either case @TO mu@ evaluates the resulting actions.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker matrix{{x^2}};
  LM = lInfinityModule(A,M,EndDegree=>1)
  a = first gens lieAlgebra LM;
  degrees LM
  entries mu(LM,{a},0)
  entries mu(LM,{a,a},0)
  entries mu(LM,{a},1)
 Text
  Take $M=S/(x^2)$ over $R=S/(x^3)$. The displayed degrees belong
  to $e_0,e_1,e_2$, the three computed dual generators. The first
  relation $D(f_1)=x^2 f_0$ has word length two. Consequently
  $\mu_2(a,e_0)=0$ and $\mu_3(a,a,e_0)=2e_1$.
  The next relation is $D(f_2)=-Tf_0+xf_1$ and gives
  $\mu_2(a,e_1)=e_2$. Thus changing the module from the residue
  field to $S/(x^2)$ changes which arity detects its first relation.
SeeAlso
 (lInfinityModule,DGAlgebra,Module)
 (lInfinityModule,DGModule)
 LInfinityModule
 mu
///

doc ///
Key
 (lInfinityModule,DGAlgebra,Module)
Headline
 Resolve a module over a minimal DG model
Usage
 LM = lInfinityModule(A,M,EndDegree=>n,GradeLimit=>d)
Description
 Text
  The input $M$ is presented over @TT "A.ring"@ and must descend
  to $H_0(A)$: each degree-one boundary $d(t)$ must annihilate $M$.
  The method constructs a minimal semifree DG module $F$ and
  stores the operations dual to its differential.
  Both differentials must square to zero, and nonzero unary
  algebra or module operations are not supported.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  LM = lInfinityModule(A,M,EndDegree=>1,GradeLimit=>2)
  degrees LM
  dF = differential(LM#"dgmodule");
  entries(dF#1)
  entries(dF#2)
 Text
  The dual basis degrees are $(0,0),(1,-1),(2,-3)$.
  The columns express
  $$D(f_1)=xf_0,\qquad D(f_2)=-Tf_0+x^2f_1.$$
  These terms determine both binary actions and the nonzero
  ternary operation on the dual basis.

  @TO EndDegree@ is a cycle-killing depth, not a hard upper bound
  on generated degrees. @TO GradeLimit@ bounds identity-checking
  inputs; direct evaluation can use any available model generator.
  @TO OrderLimit@ is stored as metadata.
SeeAlso
 (lInfinityModule,DGModule)
 lInfinityModule
 LInfinityModule
 mu
 minimalSemifreeResolution
///

doc ///
Key
 mu
Headline
 Evaluate binary and higher module operations
Usage
 mu(LM,{u_1,...,u_r},j)
 mu(LM,{u_1,...,u_r},m)
Description
 Text
  For a list of $r\geq1$ Lie inputs, @TO mu@ evaluates $\mu_{r+1}$.
  The final argument is either a zero-based dual module basis
  index or a coordinate vector in the basis @TT "gens LM"@.
  Thus a one-entry list gives the binary action and a two-entry
  list gives the ternary operation.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  LM = lInfinityModule(A,M,EndDegree=>1,GradeLimit=>2);
  L = lInfinity LM;
  g = gens L; e = gens LM;
  a = g#0;
  v = mu(LM,{a},0)
  entries v
  entries mu(LM,{a,a},e#1)
 Text
  The coordinate lists are @TT "{0,1,0}"@ and @TT "{0,0,2}"@:
  $$\mu_2(x^\vee,e_0^\vee)=e_1^\vee,\qquad
  \mu_3(x^\vee,x^\vee,e_1^\vee)=2e_2^\vee.$$
  Entry $j$ is the coefficient of @TT "e#j"@.
  The integer in the module slot chooses a basis element;
  it does not specify the arity.
SeeAlso
 (mu,LInfinityModule,List,ZZ)
 (mu,LInfinityModule,List,Vector)
 muGamma
///

doc ///
Key
 (mu,LInfinityModule,List,ZZ)
Headline
 Evaluate a module operation on one dual basis index
Usage
 v = mu(LM,{u_1,...,u_r},j)
Description
 Text
  The integer $j$ denotes the dual of the $j$th free generator
  of the stored semifree module; indexing starts at zero.
  A list with $r$ algebra entries computes $\mu_{r+1}$.
  The coefficient of $f_l^\vee$ is read from the
  word-length-$r$ part of the $f_j$-coefficient of $D(f_l)$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  LM = lInfinityModule(A,M,EndDegree=>1,GradeLimit=>2);
  L = lInfinity LM;
  g = gens L; e = gens LM;
  a = g#0; t = g#1;
  mu(LM,{a},0) == e#1
  mu(LM,{t},0) == e#2
  w = mu(LM,{a,a},1)
  entries w
  dF = differential(LM#"dgmodule");
  entries(dF#2)
 Text
  The equalities are @TT "true"@, and the ternary output has entries
  @TT "{0,0,2}"@. The final column represents
  $D(f_2)=-Tf_0+x^2f_1$.
  Its $-T$ coefficient gives the positive binary value because
  $\mu_2(T^\vee,f_0^\vee)$ includes $(-1)^{|T|}=-1$.
  Its $x^2$ coefficient gives $2f_2^\vee$ after the factor $2!$.

  In general the suspension exponent is
  $\varepsilon=|u_r|+|u_{r-2}|+\cdots$ in homological degrees.
  Graded reordering signs and repeated-input factorials are
  included. The index must be less than @TT "#gens LM"@.
SeeAlso
 mu
 (mu,LInfinityModule,List,Vector)
 (gens,LInfinityModule)
///

doc ///
Key
 (mu,LInfinityModule,List,Vector)
Headline
 Extend a higher module operation linearly in its module input
Usage
 v = mu(LM,{u_1,...,u_r},m)
Description
 Text
  A vector $m=\sum_j c_j f_j^\vee$ in the representative space
  of @TT "gens LM"@ specifies a dual module element.
  This method evaluates $\mu_{r+1}$ and extends linearly in
  $m$ and in all $r$ algebra arguments.
  The coefficients $c_j$ must lie in the coefficient field.
 Example
  S = QQ[x,y];
  A = koszulComplexDGA ideal(x^2,y^2);
  LM = lInfinityModule(A,coker vars S,EndDegree=>1);
  a = first gens lieAlgebra LM; e = gens LM;
  w = mu(LM,{a},e#1)
  entries mu(LM,{a},e#2)
  entries mu(LM,{a},2*e#1-3*e#2)
 Text
  The module has two dual generators $e_1,e_2$ of cohomological
  degree $1$. The first two vectors give $\mu_2(x^\vee,e_1)=e_4$
  and $\mu_2(x^\vee,e_2)=e_5$. In the last output the entries
  $2,-3$ in positions $4,5$ record the image
  $2e_4-3e_5$ of the specified degree-one vector. The coefficients
  of a dual coordinate vector belong to the coefficient field.
SeeAlso
 mu
 (mu,LInfinityModule,List,ZZ)
 (gens,LInfinityModule)
///

doc ///
Key
 muGamma
Headline
 Evaluate module operations in divided-power coordinates
Usage
 muGamma(LM,{u_1,...,u_r},j)
 muGamma(LM,{u_1,...,u_r},m)
Description
 Text
  The divided-power normalization of a higher module operation
  omits the factorials attached to repeated algebra inputs.
  For a basis list with multiplicities $e_v$, the ordinary
  @TO mu@ value is $\prod_v e_v!$ times the @TO muGamma@ value.
  The final module argument may be a basis index or a
  coefficient-field coordinate vector.
 Example
  S = QQ[x,y];
  A = koszulComplexDGA ideal(x^3*y);
  M = coker matrix{{x^2*y}};
  LM = lInfinityModule(A,M,EndDegree=>1);
  (a,b) = toSequence take(gens lieAlgebra LM,2);
  v = muGamma(LM,{a,a,b},0)
  entries v
  entries mu(LM,{a,a,b},0)
 Text
  Here $x^2y$ defines the first module relation $D(f_1)=x^2y f_0$.
  The three algebra entries and one module entry give an operation
  of arity four. The divided-power value is $e_1=f_1^\vee$ and
  the ordinary value is $2e_1$. Only $x$ occurs twice in the
  monomial, giving the factor $2!\,1!$.
SeeAlso
 (muGamma,LInfinityModule,List,ZZ)
 (muGamma,LInfinityModule,List,Vector)
 mu
///

doc ///
Key
 (muGamma,LInfinityModule,List,ZZ)
Headline
 Read a divided-power module operation on a basis index
Usage
 v = muGamma(LM,{u_1,...,u_r},j)
Description
 Text
  The list records multiplicities of algebra basis inputs, and
  the integer $j$ specifies the dual module generator $f_j^\vee$.
  Each nonzero algebra entry must be a scalar multiple of a
  single model-generator representative.
  The suspension and graded reordering signs are retained,
  but multiplicity factorials are omitted.

  In characteristic $2$, this retains a quadratic coefficient of the module differential
  that ordinary repeated-input evaluation cannot detect:
 Example
  S = ZZ/2[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  LM = lInfinityModule(A,M,EndDegree=>1,GradeLimit=>2);
  L = lInfinity LM;
  g = gens L; e = gens LM;
  a = g#0;
  entries mu(LM,{a,a},1)
  w = muGamma(LM,{a,a},1)
  entries w
 Text
  The two coordinate lists are @TT "{0,0,0}"@ and @TT "{0,0,1}"@.
  The differential still contains $x^2f_1$ in $D(f_2)$.
  Ordinary evaluation multiplies its coefficient by $2!=0$;
  the divided-power value remains $f_2^\vee$.
  The index $1$ selects $f_1^\vee$, while two algebra entries
  select the operation of total arity $3$.
SeeAlso
 muGamma
 (muGamma,LInfinityModule,List,Vector)
 DividedPower
///

doc ///
Key
 (muGamma,LInfinityModule,List,Vector)
Headline
 Evaluate divided-power operations on a module linear combination
Usage
 v = muGamma(LM,{u_1,...,u_r},m)
Description
 Text
  For a fixed divided-power algebra input, the module operation
  is linear in the final argument. This method accepts a
  coefficient-field combination of the vectors in @TT "gens LM"@.
  Linearity in this module slot does not turn the divided-power
  algebra slots into ordinary multilinear slots.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker diagonalMatrix{x_S,x_S};
  LM = lInfinityModule(A,M,EndDegree=>1);
  a = first gens lieAlgebra LM; e = gens LM;
  degrees LM
  w = muGamma(LM,{a,a},e#2-2*e#3)
  entries w
  entries mu(LM,{a,a},e#2-2*e#3)
 Text
  The input module is the direct sum of two copies of the residue
  field. Each summand supplies one dual generator in each of the
  three computed degrees. The two degree-one inputs $e_2,e_3$
  map to $e_4,e_5$ under the divided-power operation, giving
  $e_4-2e_5$ on their indicated combination. Ordinary evaluation
  multiplies the whole vector by $2!$ because the algebra input
  $a$ occurs twice.
SeeAlso
 muGamma
 (muGamma,LInfinityModule,List,ZZ)
 (gens,LInfinityModule)
///

doc ///
Key
 isLInfinityModule
Headline
 Check identities relating algebra and module operations
Usage
 isLInfinityModule(LM,n)
Description
 Text
  An $L_\infty$ module satisfies identities involving both
  algebra insertions $\mu_j(\ell_i(\cdots),\ldots,m)$ and nested
  module operations $\mu_j(\ldots,\mu_i(\cdots,m))$.
  @TO isLInfinityModule@ checks these identities in a finite
  range of total arities on the computed module basis.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  LM = lInfinityModule(A,coker vars S,EndDegree=>1,GradeLimit=>1);
  L = lInfinity LM; a = first gens L; e = gens LM;
  entries mu(LM,{bracket(L,{a,a,a})},e#0)
  entries mu(LM,{a,a},mu(LM,{a},e#0))
  isLInfinityModule(LM,4)
 Text
  The first vector is $6e_2$ and the second is $2e_2$. The
  arity-four identity on $(a,a,a,e_0)$ reads
  $$\mu_2(\ell_3(a,a,a),e_0)
  =3\,\mu_3(a,a,\mu_2(a,e_0)).$$
  There are three choices for the $a$ that first acts on $e_0$.
  All three have the same sign. The other terms vanish because
  $\ell_2(a,a)=0$ and $\mu_3(a,a,e_0)=0$. The final @TT "true"@
  checks through arity $4$ for all stored module basis vectors
  and algebra inputs of Lie degree $1$.
 Text
  In small positive characteristic, factorials in the multilinear
  operations may vanish. A successful classical check can therefore
  miss information in the divided-power structure even within these
  bounds. Use @TT "DividedPower=>true"@ for the intrinsic check
  described on the @TO DividedPower@ page.
SeeAlso
 (isLInfinityModule,LInfinityModule,ZZ)
 LInfinityModule
 mu
///

doc ///
Key
 (isLInfinityModule,LInfinityModule,ZZ)
Headline
 Verify the module identities through a specified total arity
Usage
 isLInfinityModule(LM,n)
 isLInfinityModule(LM,n,DividedPower=>true)
Description
 Text
  For $3\leq q\leq n$, the classical mode tests every basis tuple
  with $q-1$ algebra inputs and one module input.
  Algebra Lie grades range from $1$ to the stored @TO GradeLimit@.
  The identities use both $\ell_i$ and $\mu_i$, with their graded
  unshuffle and suspension signs, and assume $\ell_1=\mu_1=0$.
 Example
  S = ZZ/2[x];
  A = koszulComplexDGA ideal(x^3);
  LM = lInfinityModule(A,coker vars S,EndDegree=>1,GradeLimit=>1);
  a = first gens lieAlgebra LM;
  D = differential(LM#"dgmodule");
  entries(D#2)
  entries mu(LM,{a,a},1)
  entries muGamma(LM,{a,a},1)
  isLInfinityModule(LM,4,DividedPower=>true)
 Text
  The column gives $D(f_2)=Tf_0+x^2f_1$ in characteristic two.
  Here $d(T)=x^3$ and $D(f_1)=xf_0$, so
  $D^2(f_2)=x^3f_0+x^3f_0=0$. The ordinary ternary operation
  on $(a,a,e_1)$ returns zero because $2!=0$ in the field. Its
  divided-power value is $e_2$. The final @TT "true"@ includes
  the cancellation at $x^3f_0$, the word-length-three coefficient
  corresponding to total module arity four.

  The divided-power test concerns the encoded differential structure
  independently of the @TO mu@ and @TO muGamma@ evaluators.
  Its result is restricted to the requested input grades,
  arities, and available generators; it makes no claim about
  uncomputed resolution degrees.
 Text
  In small positive characteristic, factorials in the multilinear
  operations may vanish. A successful classical check can therefore
  miss information in the divided-power structure even within these
  bounds. Use @TT "DividedPower=>true"@ for the intrinsic check
  described on the @TO DividedPower@ page.
SeeAlso
 isLInfinityModule
 mu
 DividedPower
///

doc ///
Key
 (gens,LInfinityModule)
Headline
 List vectors representing the dual semifree module basis
Description
 Text
  Call @TT "e = gens LM"@ to use this method.

  If $f_0,\ldots,f_{s-1}$ are the computed free generators of $F$,
  then $(k\otimes_A F)^\vee$ has the corresponding dual basis.
  This method returns vectors representing those dual elements
  in the same order. Position $j$ is the basis index accepted
  by the integer overloads of @TO mu@ and @TO muGamma@.
 Example
  S = QQ[x,y];
  A = koszulComplexDGA ideal(x^2,y^2);
  LM = lInfinityModule(A,coker vars S,EndDegree=>1);
  (a,b) = toSequence take(gens lieAlgebra LM,2);
  e = gens LM
  degrees LM
  {mu(LM,{a},e#0)==e#2, mu(LM,{b},e#0)==e#1}
  entries mu(LM,{a},2)
 Text
  The list @TT "e"@ follows the computed module basis. The two
  degree-one entries $e_1,e_2$ have the same degree. Applying
  the two Lie generators to $e_0$ distinguishes them. The
  equalities identify $e_2$ as $\mu_2(x^\vee,e_0)$ and $e_1$
  as $\mu_2(y^\vee,e_0)$. Using index $2$ in the last call
  then gives $\mu_2(x^\vee,e_2)=e_5$.
SeeAlso
 LInfinityModule
 (degrees,LInfinityModule)
 (mu,LInfinityModule,List,ZZ)
///

doc ///
Key
 (degrees,LInfinityModule)
Headline
 Read the degrees of the dual module basis
Usage
 degrees LM
Description
 Text
  A free generator of $F$ in homological degree $h$ yields a
  dual module generator in cohomological degree $h$.
  No extra cohomological shift is made on the module basis.
  When the differentials preserve internal multigradings,
  the remaining degree entries are their negatives.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  LM = lInfinityModule(A,M,EndDegree=>1,GradeLimit=>2);
  L = lInfinity LM;
  g = gens L; e = gens LM;
  a = g#0;
  degrees LM
  degrees L
  entries mu(LM,{a,a},e#1)
 Text
  The module degrees are $(0,0),(1,-1),(2,-3)$; the Lie
  degrees are $(1,-1),(2,-3)$.
  The last output is $2e_2^\vee$.
  Its cohomological degree is
  $1+1+1+(2-3)=2$, consistent with $\mu_3$ having degree $-1$.
  If the internal grading is not preserved, only the
  cohomological entries are reported.
SeeAlso
 LInfinityModule
 (gens,LInfinityModule)
 mu
///

doc ///
Key
 (net,LInfinityModule)
Headline
 Display the size of the computed higher module
Usage
 net LM
Description
 Text
  A finite semifree model supplies a finite dual module basis
  on which the higher operations can be evaluated.
  This method displays that basis size and the syntax for
  evaluating the module operations.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  LM = lInfinityModule(A,M,EndDegree=>1,GradeLimit=>2);
  L = lInfinity LM;
  g = gens L; e = gens LM;
  a = g#0;
  net LM
  entries mu(LM,{a,a},e#1)
 Text
  The display reads
  @TT "LInfinityModule{3 module generators; use mu(LM,{...},e)}"@.
  The number $3$ counts the dual semifree generators, although
  the original residue-field module $M$ is cyclic.
  The next output, @TT "{0,0,2}"@, is the coordinate list of
  the nonzero ternary operation $2e_2^\vee$.
SeeAlso
 LInfinityModule
 (gens,LInfinityModule)
 mu
///

doc ///
Key
 HolonomyRepresentation
Headline
 A finite graded module with the binary holonomy action
Description
 Text
  A holonomy representation is the binary action $\rho=\mu_2$ on
  $M_\infty=(k\otimes_A F)^\vee$, where $F$ is a minimal semifree
  model of a module over the DG algebra $A$. Its matrices satisfy
  $\rho([u,v])=\rho(u)\rho(v)-(-1)^{|u||v|}\rho(v)\rho(u)$.
  The object records the Lie algebra, a module basis, its degrees,
  and the action of every retained Lie generator.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(S_0^2);
  M = coker vars S;
  H = holonomyRepresentation(A, M, EndDegree => 1);
  H
  degrees H
  actionMatrix(H, 0)
 Text
  Here $H$ has three module coordinates and two Lie generators. The
  module basis has degrees $(0,0),(1,-1),(2,-2)$: cohomological degree
  comes first, followed by the dual internal degree. The displayed
  matrix sends the first basis vector to the second, the second to
  the third, and the third to zero; its columns are these images.

  This is the action on the finite model that was computed. It does
  not assert that the resolution stops in degree two. Higher operations
  $\mu_3,\mu_4,\ldots$ remain accessible through @TO LInfinityModule@.
  The object is a snapshot; reconstruct it after changing a source
  differential.
SeeAlso
 holonomyRepresentation
 actionMatrix
 holonomyAction
 degrees
 LInfinityModule
///

doc ///
Key
 holonomyRepresentation
Headline
 Construct the binary action on dual resolution generators
Usage
 H = holonomyRepresentation(A, M)
 H = holonomyRepresentation LM
 H = holonomyRepresentation(G, M)
Description
 Text
  The holonomy representation retains $\mu_2$ from the higher module
  structure attached to a minimal semifree resolution. It can be
  constructed from a DG model and module, an existing
  @TO LInfinityModule@, or a @TO HomotopyLie@ and module.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(S_0^2);
  M = coker vars S;
  H = holonomyRepresentation(A, M, EndDegree => 1);
  H
  isHolonomyRepresentation H
 Text
  The result has three module basis vectors and two Lie generators;
  the final @TT "true"@ verifies their graded matrix identities.
  The computation uses a finite portion of the resolution.

  Choose @TO (holonomyRepresentation, DGAlgebra, Module)@ to compute
  from a model, @TO (holonomyRepresentation, LInfinityModule)@ to
  reuse higher module data, or
  @TO (holonomyRepresentation, HomotopyLie, Module)@ to use a specified
  finite Lie quotient. A proposed quotient is accepted only when
  omitted Lie generators act trivially and the retained action
  satisfies the representation identity. Characteristic two is not
  supported by @TO HomotopyLie@ because its odd squaring operation
  has not been implemented.
 Text
  @SUBSECTION "A non-complete-intersection example"@

  A non-complete-intersection example has noncentral degree-two
  Lie actions. Let $R=\mathbb Q[x,y]/(x^2,xy,y^2)$ and $M=R/(x)$,
  presented over $S=\mathbb Q[x,y]$ as $S/(x,y^2)$.
  The following small finite DG model already gives module
  dimensions $1,1,2,4$ through degree $3$:
 Example
  S = QQ[x,y];
  R = S/ideal(x^2,x*y,y^2);
  A = acyclicClosure(koszulComplexDGA ideal R,EndDegree=>1);
  M = coker matrix{{x_S,y_S^2}};
  LM = lInfinityModule(A,M,EndDegree=>2,GradeLimit=>3);
  H = holonomyRepresentation LM;
  apply(toList(0..3),d->numcols basis(d,H))
  X = actionMatrix(H,0); Y = actionMatrix(H,1);
  Q2 = actionMatrix(H,3); U3 = actionMatrix(H,5);
  X*Y+Y*X == Q2
  X*Q2-Q2*X == U3
  U3 != 0
 Text
  The dimension list is @TT "{1,1,2,4}"@, and all three comparisons
  return @TT "true"@. The first identity is an anticommutator
  because the Lie inputs both have degree $1$.
  The second is a commutator because their degrees are $1$ and $2$;
  its nonzero result is the action of a degree-three Lie element.
  See @TO "Holonomy over a non-complete-intersection ring"@ for
  every nonzero matrix column, degree-block calculations, an
  independent Betti-number comparison, and the finite-range
  interpretation using a larger algebra model.
SeeAlso
 "Holonomy over a non-complete-intersection ring"
 HolonomyRepresentation
 (holonomyRepresentation, DGAlgebra, Module)
 (holonomyRepresentation, LInfinityModule)
 (holonomyRepresentation, HomotopyLie, Module)
///

doc ///
Key
 (holonomyRepresentation, DGAlgebra, Module)
Headline
 Compute a holonomy representation from a DG model and a module
Usage
 H = holonomyRepresentation(A, M, EndDegree => n, GradeLimit => d)
Inputs
 A:DGAlgebra
  a minimal free DG model over a polynomial ring over a field
 M:Module
  a module over the base ring of A, annihilated by the defining boundaries
Outputs
 H:HolonomyRepresentation
Description
 Text
  For a minimal DG model $A$ over a polynomial ring $S$ and an
  $S$-module $M$, this method computes a minimal semifree resolution
  $F$ and extracts the action $\mu_2$ on $(k\otimes_A F)^\vee$.
  The boundaries defining $H_0(A)$ must annihilate $M$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(S_0^2);
  M = coker vars S;
  H = holonomyRepresentation(A, M, EndDegree => 1);
  H
  actionMatrix(H, 0)
 Text
  Here $M=k$ over $S=\mathbb Q[x]$, with $A$ the Koszul model of
  $S/(x^2)$. The stored module has three basis vectors in
  cohomological degrees zero, one, and two. The action matrix for
  $x^\vee$ has a $1$ directly below each of the first two diagonal
  positions, so it advances each of the first two basis vectors by
  one degree.

  @TO EndDegree@ controls how far the semifree resolution is computed.
  @TO GradeLimit@ selects the finite Lie quotient; with its default,
  all available Lie generators are retained. The constructor checks
  that the action descends: every omitted generator must act trivially,
  and all retained matrices must satisfy the graded representation
  identity. The differentials must be minimal and square to zero.
 Text
  @SUBSECTION "A non-complete-intersection example"@

  A non-complete-intersection example has noncentral degree-two
  Lie actions. Let $R=\mathbb Q[x,y]/(x^2,xy,y^2)$ and $M=R/(x)$,
  presented over $S=\mathbb Q[x,y]$ as $S/(x,y^2)$.
  The following small finite DG model already gives module
  dimensions $1,1,2,4$ through degree $3$:
 Example
  S = QQ[x,y];
  R = S/ideal(x^2,x*y,y^2);
  A = acyclicClosure(koszulComplexDGA ideal R,EndDegree=>1);
  M = coker matrix{{x_S,y_S^2}};
  H = holonomyRepresentation(A,M,EndDegree=>2,GradeLimit=>3);
  apply(toList(0..3),d->numcols basis(d,H))
  X = actionMatrix(H,0); Y = actionMatrix(H,1);
  Q2 = actionMatrix(H,3); U3 = actionMatrix(H,5);
  X*Y+Y*X == Q2
  X*Q2-Q2*X == U3
  U3 != 0
 Text
  The dimension list is @TT "{1,1,2,4}"@, and all three comparisons
  return @TT "true"@. The first identity is an anticommutator
  because the Lie inputs both have degree $1$.
  The second is a commutator because their degrees are $1$ and $2$;
  its nonzero result is the action of a degree-three Lie element.
  See @TO "Holonomy over a non-complete-intersection ring"@ for
  every nonzero matrix column, degree-block calculations, an
  independent Betti-number comparison, and the finite-range
  interpretation using a larger algebra model.
SeeAlso
 "Holonomy over a non-complete-intersection ring"
 holonomyRepresentation
 lInfinityModule
 actionMatrix
 isHolonomyRepresentation
///

doc ///
Key
 (holonomyRepresentation, LInfinityModule)
Headline
 Extract the binary action from an existing higher module
Usage
 H = holonomyRepresentation LM
 H = holonomyRepresentation(LM, GradeLimit => d)
Inputs
 LM:LInfinityModule
  higher module data with zero unary operation
Outputs
 H:HolonomyRepresentation
Description
 Text
  An $L_\infty$ module has operations $\mu_2,\mu_3,\ldots$.
  This method retains $\mu_2$ as a matrix representation of the
  homotopy Lie algebra and reuses the semifree model already stored
  in the input.

  For $R=\mathbb Q[x]/(x^3)$, compare applying the action of
  $x^\vee$ twice with inserting $x^\vee$ twice in a higher operation.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  LM = lInfinityModule(A,coker vars S,EndDegree => 2);
  H = holonomyRepresentation LM
  B = actionMatrix(H,0)
  entries (B*(coordinateSpace H)_0)
  B^2 == 0
  entries mu(LM,{x_S,x_S},1)
 Text
  The first column of $B$ gives the image $e_1$, while $B^2$ vanishes on all four coordinates.
  The last column is $(0,0,2,0)^{\mathsf T}$, so
  $\mu_3(x^\vee,x^\vee,e_1)=2e_2$. Passing from $LM$ to $H$
  retains the binary action on the same four coordinates. Use @TO mu@
  on the original $LM$ to recover the ternary operation.

  The @TO EndDegree@ option applies when constructing the resolution.
  On an existing higher module, @TO GradeLimit@ selects the retained Lie degrees
  provided the action descends to that quotient.
SeeAlso
 "Holonomy over a non-complete-intersection ring"
 holonomyRepresentation
 lInfinityModule
 mu
 actionMatrix
///

doc ///
Key
 (holonomyRepresentation, HomotopyLie, Module)
Headline
 Compute an action of a specified finite homotopy Lie algebra
Usage
 H = holonomyRepresentation(G, M, EndDegree => n)
Inputs
 G:HomotopyLie
  the finite Lie algebra and its underlying DG model
 M:Module
  a module over that model's base ring
Outputs
 H:HolonomyRepresentation
Description
 Text
  A finite homotopy Lie algebra $G$ records a DG model and a Lie
  degree bound. This method resolves a module over that model and
  attempts to make its binary action descend to the same finite
  quotient.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(S_0^2);
  M = coker vars S;
  G = homotopyLieAlgebra(A, GradeLimit => 2);
  H = holonomyRepresentation(G, M, EndDegree => 1)
  degrees lieAlgebra H
  actionMatrix(H, 1)
 Text
  The acting Lie algebra has generators of degrees $(1,-1)$ and
  $(2,-2)$, matching $G$. The displayed matrix is the action of the
  second Lie generator: its only nonzero entry lies in row two,
  column zero, so it sends $e_0^\vee$ to $e_2^\vee$.

  The bound recorded by $G$ is used; a separate @TO GradeLimit@
  option does not replace it in this signature. @TO EndDegree@
  controls the newly computed semifree resolution. Construction fails
  if an omitted Lie generator acts nontrivially or if the resulting
  matrices do not satisfy the quotient's representation identity.
  Here a Lie bound of one would omit the nonzero action just displayed.
SeeAlso
 holonomyRepresentation
 homotopyLieAlgebra
 lieAlgebra
 actionMatrix
///

doc ///
Key
 lieAlgebra
Headline
 Retrieve the Lie algebra acting on a module
Usage
 G = lieAlgebra H
 L = lieAlgebra LM
Description
 Text
  For a holonomy representation @TT "H"@, the result is the graded Lie
  algebra whose ordered basis indexes the action matrices. For an
  $L_\infty$ module @TT "LM"@, the result is its stored $L_\infty$ algebra.

  Over $\mathbb Q[x]/(x^2)$, identify the Lie element whose action
  advances the module basis by two degrees.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^2);
  H = holonomyRepresentation(A,coker vars S,EndDegree => 1);
  G = lieAlgebra H
  b = gens G
  entries bracket(G,{b#0,b#0})
  X = actionMatrix(H,0);
  actionMatrix(H,1) == X^2
 Text
  In the basis $(x^\vee,T^\vee)$, the coordinate list
  @TT "{0,2}"@ means $[x^\vee,x^\vee]=2T^\vee$.
  The graded commutator of the odd action with itself is twice its square,
  so $T^\vee$ acts by the square of the $x^\vee$ action.
SeeAlso
 (lieAlgebra, HolonomyRepresentation)
 (lieAlgebra, LInfinityModule)
 actionMatrix
 HomotopyLie
///

doc ///
Key
 (lieAlgebra, HolonomyRepresentation)
Headline
 Return the finite Lie algebra acting on the module coordinates
Usage
 G = lieAlgebra H
Inputs
 H:HolonomyRepresentation
Outputs
 G:HomotopyLie
Description
 Text
  For a representation $\rho\colon G\to\operatorname{End}_k(V)$,
  this method returns $G$. It preserves the basis order used by
  the action matrices.

  Over $R=\mathbb Q[x]/(x^3)$ the degree-two generator
  $T^\vee$ is independent of the bracket of the degree-one generator.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  LM = lInfinityModule(A,coker vars S,EndDegree => 2);
  H = holonomyRepresentation LM;
  G = lieAlgebra H
  b = gens G
  entries bracket(G,{b#0,b#0})
  entries (actionMatrix(H,b#1)*(coordinateSpace H)_0)
 Text
  The self-bracket of $x^\vee$ has coordinates @TT "{0,0}"@ in $G$
  because $dT=x^3$ has no quadratic part. Nevertheless $T^\vee$ acts
  nontrivially. Its image on $e_0$ is $e_2$, the third of the four
  module coordinates.
SeeAlso
 lieAlgebra
 actionMatrix
 (degrees, HomotopyLie)
///

doc ///
Key
 actionMatrix
Headline
 Read the matrix of a Lie element acting on module coordinates
Usage
 B = actionMatrix(H, u)
 B = actionMatrix(H, u, p, q)
Description
 Text
  For a holonomy representation $\rho$, this function returns
  $\rho(u)$. A Lie element may be specified by a zero-based basis
  index, a Lie coordinate vector, or a model representative.

  Take $M=R/(x)$ over $R=\mathbb Q[x,y]/(x,y)^2$.
  On this module the degree-one generators have different kernels.
 Example
  S = QQ[x,y];
  A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
  M = coker matrix{{x_S,y_S^2}};
  H = holonomyRepresentation(A,M,EndDegree => 1);
  X = actionMatrix(H,0)
  Y = actionMatrix(H,1)
  V = coordinateSpace H;
  apply({V_0,V_1},v -> {entries(X*v),entries(Y*v)})
 Text
  Read the matrices by columns, starting at $e_0$.
  In the first column, $X=\rho(x^\vee)$ has a $1$ in row $1$ and
  $Y=\rho(y^\vee)$ has only zeros. Thus $Xe_0=e_1$ and $Ye_0=0$.
  The second columns give $Xe_1=e_3$ and $Ye_1=e_2$. Thus a zero first
  column can occur in a nonzero matrix. The two images of $e_1$
  span the degree-two piece.

  The Lie input may also be supplied in coordinates or as a model
  representative. In each form the matrix acts on column vectors of
  @TT "coordinateSpace H"@. To restrict to source degree $p$ and
  read the component in target degree $q$, give the two degrees in that order.
 Example
  actionMatrix(H,0,1,2)
 Text
  The column $(0,1)^{\mathsf T}$ gives the image of $e_1$ in the degree-two
  basis $(e_2,e_3)$.
SeeAlso
 "Holonomy over a non-complete-intersection ring"
 (actionMatrix, HolonomyRepresentation, ZZ, ZZ, ZZ)
 (actionMatrix, HolonomyRepresentation, Vector, ZZ, ZZ)
 (actionMatrix, HolonomyRepresentation, RingElement, ZZ, ZZ)
 (actionMatrix, HolonomyRepresentation, ZZ)
 (actionMatrix, HolonomyRepresentation, Vector)
 (actionMatrix, HolonomyRepresentation, RingElement)
 holonomyAction
///

doc ///
Key
 (actionMatrix, HolonomyRepresentation, ZZ)
Headline
 Read the action matrix of a Lie basis generator
Usage
 B = actionMatrix(H, i)
Inputs
 H:HolonomyRepresentation
 i:ZZ
  an index with 0 <= i < dim lieAlgebra H
Outputs
 B:Matrix
  an endomorphism of coordinateSpace H
Description
 Text
  Let $b_0,\ldots,b_{s-1}$ be the basis listed by
  @TT "gens lieAlgebra H"@. This method returns $\rho(b_i)$,
  with zero-based indexing.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(S_0^2);
  M = coker vars S;
  H = holonomyRepresentation(A, M, EndDegree => 1);
  actionMatrix(H, 0)
  actionMatrix(H, 1)
 Text
  The first matrix sends $e_0^\vee$ to $e_1^\vee$ and $e_1^\vee$
  to $e_2^\vee$. The second sends only $e_0^\vee$ to $e_2^\vee$.
  The integer zero selects the first Lie generator; it does not
  request the zero action. Each column records the image of the
  corresponding module basis vector.
SeeAlso
 "Holonomy over a non-complete-intersection ring"
 actionMatrix
 lieAlgebra
 holonomyAction
///

doc ///
Key
 (actionMatrix, HolonomyRepresentation, Vector)
Headline
 Evaluate an action matrix from Lie coordinates
Usage
 B = actionMatrix(H, u)
Inputs
 H:HolonomyRepresentation
 u:Vector
  a vector in coordinateSpace lieAlgebra H
Outputs
 B:Matrix
  an endomorphism of coordinateSpace H
Description
 Text
  A vector $u=\sum_i c_i b_i$ in the coordinate space of the acting
  Lie algebra has action $\rho(u)=\sum_i c_i\rho(b_i)$.
  The coefficients belong to the ground field.

  In $M=R/(x)$ over $R=\mathbb Q[x,y]/(x,y)^2$, evaluate the
  difference of the two degree-one actions.
 Example
  S = QQ[x,y];
  A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
  M = coker matrix{{x_S,y_S^2}};
  H = holonomyRepresentation(A,M,EndDegree => 1);
  U = coordinateSpace lieAlgebra H;
  B = actionMatrix(H,U_0-U_1)
  entries (B*(coordinateSpace H)_1)
 Text
  The input $U_0-U_1$ gives $B=\rho(x^\vee)-\rho(y^\vee)$.
  The second column is $(0,0,-1,1)^{\mathsf T}$ because the two
  actions take $e_1$ to $e_3$ and $e_2$, respectively. The first
  column is $e_1$ because only $x^\vee$ acts nontrivially on $e_0$.
SeeAlso
 "Holonomy over a non-complete-intersection ring"
 actionMatrix
 coordinateSpace
 lieCoordinates
 holonomyAction
///

doc ///
Key
 (actionMatrix, HolonomyRepresentation, RingElement)
Headline
 Evaluate an action matrix from a model representative
Usage
 B = actionMatrix(H, u)
Inputs
 H:HolonomyRepresentation
 u:RingElement
  a coefficient-field linear form in gens lieAlgebra H
Outputs
 B:Matrix
  an endomorphism of coordinateSpace H
Description
 Text
  The model generators serve as labels for Lie basis elements.
  This method converts a coefficient-field linear form in those
  labels to Lie coordinates and returns its action matrix.

  Over $\mathbb Q[x,y]/(x^2,y^2)$, use the linear form $x+y$ to
  compute the action of $x^\vee+y^\vee$ on the residue field's
  dual resolution generators.
 Example
  S = QQ[x,y];
  A = koszulComplexDGA ideal(x^2,y^2);
  H = holonomyRepresentation(A,coker vars S,EndDegree => 1);
  B = actionMatrix(H,x_S+y_S)
  z = (coordinateSpace H)_0;
  entries (B*z)
  entries (B^2*z)
 Text
  The first image is $e_1+e_2$ and the second is $e_3+e_5$.
  Expanding $B^2$ gives two mixed products whose values on $z$
  cancel: $\rho(x^\vee)\rho(y^\vee)z=e_4$ and
  $\rho(y^\vee)\rho(x^\vee)z=-e_4$. The two surviving terms
  come from the individual squares. Use matrix multiplication to
  compose actions once the linear form has been converted.
SeeAlso
 "Holonomy over a non-complete-intersection ring"
 actionMatrix
 lieCoordinates
 holonomyAction
///

doc ///
Key
 holonomyAction
Headline
 Apply a Lie element to a module coordinate vector
Usage
 w = holonomyAction(H, u, v)
Description
 Text
  The holonomy action evaluates $\rho(u)v=\mu_2(u,v)$.
  It multiplies the action matrix of the Lie element $u$ by the
  module column vector $v$.

  Does interchanging two odd operators change their action on a
  cyclic generator? Take $M=R/(x)$ over $R=\mathbb Q[x,y]/(x,y)^2$.
 Example
  S = QQ[x,y];
  A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
  M = coker matrix{{x_S,y_S^2}};
  H = holonomyRepresentation(A,M,EndDegree => 1);
  z = (coordinateSpace H)_0;
  w = holonomyAction(H,1,holonomyAction(H,0,z))
  entries w
  entries holonomyAction(H,0,holonomyAction(H,1,z))
 Text
  The two compositions have coordinate lists @TT "{0,0,1,0}"@ and @TT "{0,0,0,0}"@.
  Applying $x^\vee$ first gives $e_1$, on which $y^\vee$ acts by
  $e_2$. Applying $y^\vee$ first already annihilates $z$.
  The sum of the two compositions is the action of the odd bracket
  $[x^\vee,y^\vee]$ on $z$.
SeeAlso
 (holonomyAction, HolonomyRepresentation, Thing, Vector)
 actionMatrix
 coordinateSpace
///

doc ///
Key
 (holonomyAction, HolonomyRepresentation, Thing, Vector)
Headline
 Evaluate the binary action on a module vector
Usage
 w = holonomyAction(H, u, v)
Inputs
 H:HolonomyRepresentation
 u:Thing
  a Lie basis index, Lie coordinate vector, or model representative
 v:Vector
  a vector in coordinateSpace H
Outputs
 w:Vector
  the image of v in coordinateSpace H
Description
 Text
  For a Lie element $u$ and a vector $v\in M_\infty$, this method
  computes $\rho(u)v$. The Lie input may be a zero-based basis index,
  a Lie coordinate vector, or a model representative.

  Let $R=\mathbb Q[x,y]/(x^2,y^2)$. An action on a vector with two
  nonzero coordinates can cancel contributions to a single output coordinate.
 Example
  S = QQ[x,y];
  A = koszulComplexDGA ideal(x^2,y^2);
  H = holonomyRepresentation(A,coker vars S,EndDegree => 1);
  U = coordinateSpace lieAlgebra H; V = coordinateSpace H;
  v = V_1+V_2;
  w = holonomyAction(H,0,v)
  entries holonomyAction(H,1,v)
  entries holonomyAction(H,U_0+U_1,v)
 Text
  The first two images are $e_4+e_5$ and $e_3-e_4$.
  Their sum $e_3+e_5$ is the action of $x^\vee+y^\vee$ on $v$.
  Here @TT "U_0+U_1"@ supplies the Lie coefficients and
  @TT "V_1+V_2"@ supplies the module coefficients.
SeeAlso
 holonomyAction
 actionMatrix
 coordinateSpace
///

doc ///
Key
 isHolonomyRepresentation
Headline
 Check the graded representation identity
Usage
 isHolonomyRepresentation H
Description
 Text
  For homogeneous Lie elements of degrees $p$ and $q$, a graded
  representation satisfies
  $$\rho([u,v])=\rho(u)\rho(v)-(-1)^{pq}\rho(v)\rho(u).$$
  This predicate checks that identity on the retained Lie basis.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(S_0^2);
  M = coker vars S;
  H = holonomyRepresentation(A, M, EndDegree => 1);
  G = lieAlgebra H;
  X = (gens G)#0;
  B = actionMatrix(H, X);
  B*B
  actionMatrix(H, bracket(G, {X, X})) == 2*B*B
  isHolonomyRepresentation H
 Text
  The square $B^2$ is nonzero: it sends $e_0^\vee$ to $e_2^\vee$.
  Since $X$ has odd Lie degree, its self-commutator is $2B^2$,
  matching the action of $[X,X]$. Both Boolean outputs are
  @TT "true"@. The predicate checks the finite matrices stored in
  $H$; it does not certify an entire infinite resolution or test
  higher operations $\mu_i$ for $i\geq 3$.
SeeAlso
 (isHolonomyRepresentation, HolonomyRepresentation)
 holonomyRepresentation
 actionMatrix
///

doc ///
Key
 (isHolonomyRepresentation, HolonomyRepresentation)
Headline
 Verify the stored action matrices against the Lie bracket
Usage
 isHolonomyRepresentation H
Inputs
 H:HolonomyRepresentation
Outputs
 :Boolean
  whether every retained homogeneous Lie basis pair satisfies the identity
Description
 Text
  For homogeneous Lie elements of degrees $p$ and $q$, a graded
  representation satisfies
  $$\rho([u,v])=\rho(u)\rho(v)-(-1)^{pq}\rho(v)\rho(u).$$
  This predicate checks that identity on the retained Lie basis.

  For two distinct odd generators with zero bracket, the identity
  requires their matrices to anticommute. Both products can be nonzero.
 Example
  S = QQ[x,y];
  A = koszulComplexDGA ideal(x^2,y^2);
  H = holonomyRepresentation(A,coker vars S,EndDegree => 1);
  G = lieAlgebra H; b = gens G;
  X = actionMatrix(H,0); Y = actionMatrix(H,1);
  z = (coordinateSpace H)_0;
  entries bracket(G,{b#0,b#1})
  {entries(X*Y*z),entries(Y*X*z)}
  isHolonomyRepresentation H
 Text
  There is no $xy$ term in either defining equation, so
  $[x^\vee,y^\vee]$ has four zero coordinates. The displayed products
  on $z$ are $e_4$ and $-e_4$ and their sum vanishes, as the
  graded representation identity requires. The final @TT "true"@
  checks the identity on every retained Lie basis pair.
SeeAlso
 isHolonomyRepresentation
 holonomyRepresentation
 actionMatrix
///

doc ///
Key
 (ring, HolonomyRepresentation)
Headline
 Return the coefficient field of a holonomy representation
Usage
 k = ring H
Inputs
 H:HolonomyRepresentation
Outputs
 :Ring
  the coefficient field
Description
 Text
  The action matrices of $\rho\colon G\to\operatorname{End}_k(V)$
  have coefficients in $k$. This method returns that field, not
  the polynomial base ring used to build the resolution.

  Work in characteristic three and read the coefficient of an odd
  self-bracket directly from its action.
 Example
  S = (ZZ/3)[x];
  A = koszulComplexDGA ideal(x^2);
  H = holonomyRepresentation(A,coker vars S,EndDegree => 1);
  G = lieAlgebra H; a = first gens G;
  ring H
  entries (actionMatrix(H,bracket(G,{a,a}))*(coordinateSpace H)_0)
 Text
  The returned field is $\mathbb F_3$. The odd self-bracket acts as
  $2\rho(a)^2$, sending $e_0$ to $2e_2=-e_2$. Thus the coefficient
  printed as @TT "-1"@ is twice the unit of the matrix field.
SeeAlso
 HolonomyRepresentation
 actionMatrix
 coordinateSpace
///

doc ///
Key
 (degrees, HolonomyRepresentation)
Headline
 Read the degrees of the dual module basis
Usage
 degrees H
Inputs
 H:HolonomyRepresentation
Outputs
 :List
  one degree list per module basis vector, with cohomological degree first
Description
 Text
  The vector dual to a semifree generator of homological degree $d$
  has cohomological degree $d$. When the algebra and module
  differentials preserve internal multidegrees, those degrees are
  negated on the dual basis.

  On the cubic hypersurface, the two degree shifts of the
  action give different cohomological and internal increments.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  LM = lInfinityModule(A,coker vars S,EndDegree => 2);
  H = holonomyRepresentation LM;
  degrees H
  degrees lieAlgebra H
  entries (actionMatrix(H,1)*(coordinateSpace H)_1)
 Text
  The four module degrees are $(0,0),(1,-1),(2,-3),(3,-4)$.
  The second Lie generator $T^\vee$ has degree $(2,-3)$, so it takes
  $e_1$ of degree $(1,-1)$ to $e_3$ of degree $(3,-4)$.
  The displayed last coordinate is $1$, verifying this action.
  When the differentials preserve only cohomological degree, the
  output lists only that component.
SeeAlso
 HolonomyRepresentation
 (basis, ZZ, HolonomyRepresentation)
 actionMatrix
///

doc ///
Key
 (dim, HolonomyRepresentation)
Headline
 Count the stored module coordinates
Usage
 dim H
Inputs
 H:HolonomyRepresentation
Outputs
 :ZZ
  the dimension of the stored module coordinate space
Description
 Text
  The dimension of a holonomy representation is the dimension of
  $M_\infty=(k\otimes_A F)^\vee$ for the stored finite semifree
  model. It counts module generators, not Lie generators.

  Keep $R=\mathbb Q[x,y]/(x,y)^2$ fixed and change $M$ from
  $R/(x)$ to the residue field.
 Example
  S = QQ[x,y];
  A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
  M = coker matrix{{x_S,y_S^2}};
  H = holonomyRepresentation(A,M,EndDegree => 1);
  K = holonomyRepresentation(A,coker vars S,EndDegree => 1);
  apply({H,K},h -> {dim h,apply(3,d -> numcols basis(d,h))})
 Text
  For $R/(x)$ the four coordinates occur in dimensions $1,1,2$.
  For $k$ the seven coordinates occur in dimensions $1,2,4$.
  The first syzygies explain the difference: the kernel of $R\to R/(x)$
  is $(x)$, while the kernel of $R\to k$ is $(x,y)$.
  Since $(x,y)^2=0$, each first syzygy generator contributes two
  generators in the next degree. Both totals refer to the retained
  three degrees.
SeeAlso
 HolonomyRepresentation
 coordinateSpace
 (basis, HolonomyRepresentation)
///

doc ///
Key
 (basis, HolonomyRepresentation)
Headline
 Return the full module coordinate basis
Description
 Text
  Call @TT "E = basis H"@ to use this method.

  The dual semifree generators give an ordered basis of the holonomy
  module. This method returns the identity matrix on its coordinate
  space, with those basis vectors as columns.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(S_0^2);
  M = coker vars S;
  H = holonomyRepresentation(A, M, EndDegree => 1);
  E = basis H
  E_0
  holonomyAction(H, 0, E_0)
 Text
  The matrix $E$ is the $3\times3$ identity. The expression
  @TT "E_0"@ selects its first column, a module vector. The final
  output is its image under the first Lie generator, namely the
  second column of $E$. The basis is ordered as in @TT "degrees H"@.
SeeAlso
 (basis, ZZ, HolonomyRepresentation)
 coordinateSpace
 holonomyAction
///

doc ///
Key
 (basis, ZZ, HolonomyRepresentation)
Headline
 Select module basis vectors in one cohomological degree
Description
 Text
  Call @TT "B = basis(d, H)"@ to use this method.

  For the graded holonomy module $V=\bigoplus_d V^d$, this method
  returns the columns of the standard coordinate basis that span
  $V^d$. It uses cohomological degree, the first entry of each
  list in @TT "degrees H"@.

  Use the inclusions of two graded pieces to write an action as a
  small matrix. Take $M=R/(x)$ over $R=\mathbb Q[x,y]/(x,y)^2$.
 Example
  S = QQ[x,y];
  A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2), EndDegree => 1);
  M = coker matrix{{x_S,y_S^2}};
  H = holonomyRepresentation(A,M,EndDegree => 1);
  E = basis(1,H)
  F = basis(2,H)
  transpose F * actionMatrix(H,0) * E
  transpose F * actionMatrix(H,1) * E
 Text
  The columns of $E$ select $e_1$ and the columns of $F$ select
  $e_2,e_3$. Since these are standard coordinate columns,
  $F^{\mathsf T}$ reads off the two target coefficients. The final two
  columns are $(0,1)^{\mathsf T}$ and $(1,0)^{\mathsf T}$:
  $x^\vee e_1=e_3$ and $y^\vee e_1=e_2$.
SeeAlso
 (basis, HolonomyRepresentation)
 (degrees, HolonomyRepresentation)
 coordinateSpace
///

doc ///
Key
 (coordinateSpace, HolonomyRepresentation)
Headline
 Return the vector space on which the holonomy matrices act
Usage
 V = coordinateSpace H
Inputs
 H:HolonomyRepresentation
Outputs
 V:Module
  the coefficient-field module carrying the action
Description
 Text
  A holonomy representation acts on a coefficient-field vector space
  with basis dual to the stored semifree generators. This method
  returns that coordinate module so its vectors can be supplied to
  @TO holonomyAction@.

  Use the coordinate space to find the part of the module killed
  by the degree-one generator over $\mathbb Q[x]/(x^2)$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^2);
  H = holonomyRepresentation(A,coker vars S,EndDegree => 1);
  V = coordinateSpace H
  X = actionMatrix(H,0);
  entries (X^2*V_0)
  gens ker X
 Text
  The final two columns give $e_2$ in $V=\mathbb Q^3$. The first displays
  $\rho(x^\vee)^2e_0=e_2$ and the second spans the kernel of
  $\rho(x^\vee)$ in the computed three-degree representation.
  The degrees of these coordinates appear in @TT "degrees H"@.
SeeAlso
 coordinateSpace
 holonomyAction
 (degrees, HolonomyRepresentation)
 (basis, HolonomyRepresentation)
///

doc ///
Key
 (net, HolonomyRepresentation)
Headline
 Display the size of a holonomy representation
Usage
 net H
Inputs
 H:HolonomyRepresentation
Outputs
 :Net
  the compact display
Description
 Text
  The display of a holonomy representation reports the dimensions
  of its module space and acting Lie algebra. It summarizes the
  stored finite model without printing all action matrices.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(S_0^2);
  M = coker vars S;
  H = holonomyRepresentation(A, M, EndDegree => 1);
  net H
 Text
  The display reads
  @TT "HolonomyRepresentation{3 module generators, 2 Lie generators}"@.
  The first count is @TT "dim H"@ and the second is
  @TT "dim lieAlgebra H"@. Use @TO actionMatrix@ to inspect the
  action itself and @TO (degrees, HolonomyRepresentation)@ for the
  grading.
SeeAlso
 HolonomyRepresentation
 actionMatrix
 (dim, HolonomyRepresentation)
///

doc ///
Key
 coordinateSpace
Headline
 Obtain the field-valued coordinate module
Usage
 V = coordinateSpace G
 V = coordinateSpace H
Description
 Text
  The finite objects @TO HomotopyLie@ and @TO HolonomyRepresentation@
  store their coordinates in free modules over the coefficient field $k$.
  For a Lie object this is $k^{\dim G}$; for a representation it is the
  space $M_\infty$ on which the Lie algebra acts. The ordered bases are
  the columns returned by @TO basis@. The two overloads are documented
  separately below.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^2);
  G = homotopyLieAlgebra A;
  V = coordinateSpace G
  rank V
  u = V_0;
  entries bracket(G,{u,u})
 Text
  Here $V=\mathbb{Q}^2$, with coordinates ordered as
  $(x^\vee,T^\vee)$, where $d(T)=x^2$.
  The result @TT "{0,2}"@ represents $[x^\vee,x^\vee]=2T^\vee$.
  The native free module is an ungraded coordinate container; use
  @TO (degrees, HomotopyLie)@ or @TO (degrees, HolonomyRepresentation)@
  for the mathematical grading.
SeeAlso
 (coordinateSpace, HomotopyLie)
 (coordinateSpace, HolonomyRepresentation)
///

doc ///
Key
 GradeLimit
Headline
 Distinguish checker bounds from finite Lie quotients
Usage
 GradeLimit => n
Description
 Text
  This option has two meanings, depending on the constructor. For
  @TO lInfinity@ and @TO lInfinityModule@ it bounds the Lie degrees
  of inputs used by identity checkers. For @TO homotopyLieAlgebra@
  and @TO holonomyRepresentation@ it selects a finite Lie quotient.
  In particular, a checker bound does not truncate direct evaluations.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^2);
  L = lInfinity(A,GradeLimit=>1);
  a = (gens L)#0;
  bracket(L,{a,a})
  G = homotopyLieAlgebra(A,GradeLimit=>1);
  degrees G
  entries bracket(G,{(gens G)#0,(gens G)#0})
 Text
  The first result is $2T$, even though its Lie degree is $2$:
  @TT "GradeLimit=>1"@ restricts only the test inputs for $L$.
  The object $G$ instead has the sole basis element $x^\vee$ in
  degree $1$, and its bracket returns @TT "{0}"@ because $T^\vee$
  is killed in the quotient. This does not assert that the original
  homotopy Lie algebra vanishes in degree $2$.
SeeAlso
 [lInfinity, GradeLimit]
 [lInfinityModule, GradeLimit]
 [homotopyLieAlgebra, GradeLimit]
 [holonomyRepresentation, GradeLimit]
///

doc ///
Key
 OrderLimit
Headline
 Record an arity preference as metadata
Usage
 OrderLimit => r
Description
 Text
  The arity of $\ell_r$ is the number of its algebra inputs; the arity
  of $\mu_r$ includes its final module input. In the current interface,
  this option records metadata and does not restrict either evaluator.
  The integer argument to an identity checker is its actual arity bound.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,OrderLimit=>2);
  L#"OrderLimit"
  a = (gens L)#0;
  bracket(L,{a,a,a})
  isLInfinity(L,4)
 Text
  The stored value is $2$, but the ternary bracket is still $6T$,
  representing $\ell_3(x^\vee,x^\vee,x^\vee)=6T^\vee$.
  The final @TT "true"@ reports a successful check through arity $4$.
  To evaluate an operation, specify its inputs; to bound a check,
  supply the integer to the checker itself.
SeeAlso
 [lInfinity, OrderLimit]
 [lInfinityModule, OrderLimit]
 isLInfinity
 isLInfinityModule
///

doc ///
Key
 DividedPower
Headline
 Use divided powers for identity checks and higher-operation tables
Usage
 DividedPower => true
Description
 Text
  Over a field of positive characteristic, factorials in the ordinary
  multilinear operations can vanish. With this option the identity
  checkers test the relevant word-length coefficients of $d_A^2$ and
  $D_F^2$ directly. These are the identities for the dual coalgebra
  and comodule, without factorial division. This validates the structure
  encoded by the differentials; it does not change the evaluator being used.
 Example
  S = (ZZ/3)[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,GradeLimit=>2);
  a = (gens L)#0;
  bracket(L,{a,a,a})
  bracketGamma(L,{a,a,a})
  isLInfinity(L,4,DividedPower=>true)
 Text
  The ordinary value is zero because $3!=0$ in $\mathbb{F}_3$.
  The divided-power value is $T$, representing the nonzero coefficient
  of $x^3$ in $d(T)$. The final @TT "true"@ verifies the differential-
  encoded identities through arity $4$ in the specified input grades.
  Use @TO bracketGamma@ or @TO muGamma@ to evaluate these normalized
  basis operations; the checker option itself returns only a Boolean.
  @TO [structureConstants,DividedPower]@ lists the corresponding nonzero basis values.
SeeAlso
 [structureConstants,DividedPower]
 [isLInfinity, DividedPower]
 [isLInfinityModule, DividedPower]
 bracketGamma
 muGamma
///

doc ///
Key
 [lInfinity, GradeLimit]
Headline
 Bound the Lie degrees tested in algebra identities
Usage
 lInfinity(A,GradeLimit=>n)
Description
 Text
  The generalized Jacobi identities are tested on basis tuples.
  This positive integer limits the cohomological Lie degree of each
  input in @TO isLInfinity@. A model generator of homological degree
  $d$ has Lie degree $d+1$. The default is $3$; the bound does not
  remove generators from $L$ or truncate @TO bracket@.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,GradeLimit=>1);
  degrees L
  a = (gens L)#0;
  bracket(L,{a,a,a})
  isLInfinity(L,4)
 Text
  The degree list still contains $(1,-1)$ and $(2,-3)$, and the
  ternary bracket is $6T$ in Lie degree $2$. The checker returns
  @TT "true"@ for identities through arity $4$ whose inputs have
  Lie degree $1$. It does not test identities with $T^\vee$ as an input.
  For @TT "lInfinity LM"@ the same option sets the new algebra object's
  checker bound and leaves the stored module object's bound unchanged.
SeeAlso
 GradeLimit
 (lInfinity, DGAlgebra)
 (lInfinity, LInfinityModule)
 (isLInfinity, LInfinity, ZZ)
///

doc ///
Key
 [lInfinity, OrderLimit]
Headline
 Store an algebra arity preference
Usage
 lInfinity(A,OrderLimit=>r)
Description
 Text
  This integer, at least $2$, is stored in the @TO LInfinity@ object.
  Its default is $3$. It currently neither caps the brackets available
  from the differential nor determines the arities checked by
  @TO isLInfinity@.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,OrderLimit=>2);
  L#"OrderLimit"
  a = (gens L)#0;
  bracket(L,{a,a,a})
 Text
  The first value is $2$ and the second is $6T$:
  the cubic differential still determines a nonzero $\ell_3$.
  The number of entries in the list, rather than this metadata,
  selects the operation. The same rule applies to @TT "lInfinity LM"@.
SeeAlso
 OrderLimit
 (bracket, LInfinity, List)
 (lInfinity, DGAlgebra)
///

doc ///
Key
 [lInfinityModule, GradeLimit]
Headline
 Bound the algebra input degrees in module identity tests
Usage
 lInfinityModule(A,M,GradeLimit=>n)
 lInfinityModule(F,GradeLimit=>n)
Description
 Text
  An identity for $\mu_i$ includes Lie inputs and one module input.
  This positive integer bounds the cohomological degrees of the Lie
  inputs enumerated by @TO isLInfinityModule@. It does not restrict
  module basis indices or direct evaluations of @TO mu@. The default
  is $3$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  LM = lInfinityModule(A,coker vars S,EndDegree=>1,GradeLimit=>1);
  a = gens lInfinity LM;
  e = gens LM;
  mu(LM,{a#1},e#0) == e#2
  isLInfinityModule(LM,4)
 Text
  The first @TT "true"@ verifies
  $\mu_2(T^\vee,e_0^\vee)=e_2^\vee$, even though $T^\vee$ has
  Lie degree $2$. The second @TT "true"@ checks identities through
  total arity $4$ with Lie inputs of degree $1$ and every stored module
  basis element. It makes no claim about identities with degree-$2$
  Lie inputs.
SeeAlso
 GradeLimit
 (lInfinityModule, DGAlgebra, Module)
 (isLInfinityModule, LInfinityModule, ZZ)
///

doc ///
Key
 [lInfinityModule, OrderLimit]
Headline
 Store an arity preference with higher module data
Usage
 lInfinityModule(A,M,OrderLimit=>r)
 lInfinityModule(F,OrderLimit=>r)
Description
 Text
  This integer, at least $2$, is stored in the associated algebra data;
  the default is $3$. It does not truncate the module operations.
  A list of $s$ Lie inputs passed to @TO mu@ selects $\mu_{s+1}$,
  regardless of this metadata.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  LM = lInfinityModule(A,coker vars S,EndDegree=>1,OrderLimit=>2);
  L = lieAlgebra LM;
  L#"OrderLimit"
  a = (gens L)#0;
  e = gens LM;
  entries mu(LM,{a,a},e#1)
 Text
  The stored value is $2$, but the output @TT "{0,0,2}"@ represents
  $\mu_3(x^\vee,x^\vee,e_1^\vee)=2e_2^\vee$.
  For a check through total arity $r$, call
  @TT "isLInfinityModule(LM,r)"@ explicitly.
SeeAlso
 OrderLimit
 (mu, LInfinityModule, List, Vector)
 (lInfinityModule, DGAlgebra, Module)
///

doc ///
Key
 [lInfinityModule, EndDegree]
Headline
 Set the cycle-killing depth of a semifree resolution
Usage
 lInfinityModule(A,M,EndDegree=>n)
Description
 Text
  This nonnegative integer controls the construction of the minimal
  semifree resolution $F$ of $M$ over $A$; its default is $3$.
  When the input is an existing DG module, its basis is used as supplied.
  Killing a cycle in homological degree $n$ may require a generator
  in degree $n+1$. It is therefore not a hard upper bound on the
  degrees returned by @TO (degrees, LInfinityModule)@.
 Text
  On the residue field of the cubic hypersurface, compare the first two
  depths. Which basis element becomes available for the ternary action?
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  L0 = lInfinityModule(A,M,EndDegree=>0);
  L1 = lInfinityModule(A,M,EndDegree=>1);
  {degrees L0, degrees L1}
  {entries mu(L0,{x_S,x_S},1), entries mu(L1,{x_S,x_S},1)}
 Text
  At depth zero the resolution has generators in degrees $0$ and $1$.
  At depth one it gains a third generator of bidegree $(2,-3)$ on the dual side.
  The two coordinate lists show the effect on $\mu_3(x^\vee,x^\vee,e_1^\vee)$.
  It projects to zero in the shorter computation and equals $2e_2^\vee$ in the longer one.
SeeAlso
 (lInfinityModule, DGAlgebra, Module)
 (degrees, LInfinityModule)
///

doc ///
Key
 [isLInfinity, DividedPower]
Headline
 Check algebra identities without factorial division
Usage
 isLInfinity(L,n,DividedPower=>true)
Description
 Text
  The default value @TT "false"@ tests identities using multilinear
  brackets. With @TT "true"@, the function expands $d_A^2$ and tests
  coefficients of word lengths $3$ through $n$ whose variables lie
  within the stored Lie degree bound. The graded algebra multiplication
  supplies the exterior signs and handles repeated even generators.
 Example
  S = (ZZ/3)[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity(A,GradeLimit=>2);
  a = (gens L)#0;
  bracketGamma(L,{a,a,a})
  isLInfinity(L,4,DividedPower=>true)
 Text
  The first output is $T$: the cubic coefficient survives in the
  divided-power basis. The Boolean @TT "true"@ verifies the dual
  coalgebra identities in the tested range over $\mathbb{F}_3$.
  The checker validates the stored differential, not the public bracket
  evaluation formula, and assumes that the unary operation is zero.
SeeAlso
 DividedPower
 (isLInfinity, LInfinity, ZZ)
 (bracketGamma, LInfinity, List)
///

doc ///
Key
 [isLInfinityModule, DividedPower]
Headline
 Check module identities in the dual comodule
Usage
 isLInfinityModule(LM,n,DividedPower=>true)
Description
 Text
  With @TT "true"@ this checker expands the semifree differential
  square $D_F^2$ and tests algebra word lengths $2$ through $n-1$,
  corresponding to total arities $3$ through $n$. The stored
  @TO GradeLimit@ restricts the Lie variables in the tested monomials.
  The default @TT "false"@ instead evaluates multilinear identities.
 Example
  S = (ZZ/2)[x];
  A = koszulComplexDGA ideal(x^3);
  LM = lInfinityModule(A,coker vars S,EndDegree=>1,GradeLimit=>2);
  a = (gens lInfinity LM)#0;
  e = gens LM;
  entries mu(LM,{a,a},e#1)
  entries muGamma(LM,{a,a},e#1)
  isLInfinityModule(LM,4,DividedPower=>true)
 Text
  The ordinary output is @TT "{0,0,0}"@ because $2!=0$.
  The divided-power output @TT "{0,0,1}"@ retains the coefficient
  of $x^2f_1$ in $D(f_2)$. The final @TT "true"@ checks the comodule
  identities through total arity $4$ without dividing by factorials.
  This tests the differential-encoded structure in the finite model
  and assumes that its unary module operation vanishes.
SeeAlso
 DividedPower
 (isLInfinityModule, LInfinityModule, ZZ)
 (muGamma, LInfinityModule, List, Vector)
///

doc ///
Key
 [homotopyLieAlgebra, GradeLimit]
Headline
 Form a quotient by higher Lie degrees
Usage
 homotopyLieAlgebra(A,GradeLimit=>n)
Description
 Text
  A nonnegative integer $n$ retains the generators in Lie degrees
  at most $n$ and projects the bracket to those coordinates. This
  forms a finite quotient by the higher degrees. The default
  @TT "null"@ includes every generator available in the supplied model.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^2);
  G = homotopyLieAlgebra(A,GradeLimit=>1);
  degrees G
  a = (gens G)#0;
  entries bracket(G,{a,a})
  Gfull = homotopyLieAlgebra A;
  entries bracket(Gfull,{(gens Gfull)#0,(gens Gfull)#0})
 Text
  The first object retains only $x^\vee$ in degree $1$, so its bracket
  has coordinates @TT "{0}"@. In the full supplied model the value
  is @TT "{0,2}"@, representing $2T^\vee$ in degree $2$.
  The quotient discards this component; it does not prove its absence
  from the full homotopy Lie algebra.
SeeAlso
 GradeLimit
 (homotopyLieAlgebra, DGAlgebra)
 (bracket, HomotopyLie, List)
///

doc ///
Key
 [holonomyRepresentation, GradeLimit]
Headline
 Require the action to descend to a finite Lie quotient
Usage
 holonomyRepresentation(A,M,GradeLimit=>n)
Description
 Text
  For a DG algebra or an existing @TO LInfinityModule@ input, this
  bound selects Lie degrees at most $n$. The default @TT "null"@
  retains all available Lie generators. For the action to descend,
  every omitted Lie generator must act trivially and the retained
  matrices must satisfy the graded representation identity.
 Text
  Whether the action descends can change as more of the module resolution
  is computed. Take the residue field of $\mathbb Q[x]/(x^3)$ and propose
  to keep only the degree-one Lie generator $x^\vee$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  M = coker vars S;
  H0 = holonomyRepresentation(A,M,EndDegree=>0,GradeLimit=>1);
  actionMatrix(H0,0)
  H1 = holonomyRepresentation(A,M,EndDegree=>1);
  (actionMatrix(H1,1))_0
 Text
  At depth zero, $x^\vee$ sends $e_0^\vee$ to $e_1^\vee$ and the action descends.
  At depth one, the displayed first column of the degree-two operator is $e_2^\vee$:
  the generator proposed for omission acts nontrivially. At this depth the constructor
  rejects @TT "GradeLimit=>1"@. With input $(G,M)$ it uses the degree bound stored in $G$.
SeeAlso
 GradeLimit
 (holonomyRepresentation, DGAlgebra, Module)
 (holonomyRepresentation, LInfinityModule)
 (holonomyRepresentation, HomotopyLie, Module)
///

doc ///
Key
 [holonomyRepresentation, EndDegree]
Headline
 Set the resolution depth before extracting the binary action
Usage
 holonomyRepresentation(A,M,EndDegree=>n)
Description
 Text
  For inputs $(A,M)$ or $(G,M)$, this nonnegative integer is passed
  to the semifree resolution constructor; its default is $3$.
  The resulting coordinate space contains the duals of the generators
  actually constructed. The holonomy action retains only $\mu_2$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  H = holonomyRepresentation(A,coker vars S,EndDegree=>1);
  dim H
  degrees H
  entries holonomyAction(H,0,(coordinateSpace H)_0)
 Text
  The dimension is $3$, with cohomological degrees $0,1,2$.
  The coordinate output @TT "{0,1,0}"@ represents
  $\mu_2(x^\vee,e_0^\vee)=e_1^\vee$.
  The depth parameter can introduce generators one degree higher
  than the last cycles killed. When the input is an existing
  @TO LInfinityModule@, its resolution is reused; this option does
  not extend or recompute it.
SeeAlso
 (holonomyRepresentation, DGAlgebra, Module)
 (holonomyRepresentation, HomotopyLie, Module)
 (holonomyRepresentation, LInfinityModule)
 [lInfinityModule, EndDegree]
///

doc ///
Key
 "L-infinity module on a higher matrix factorization"
Headline
 The third syzygy of a circulant module over a complete intersection
Description
  Text
   Let $k=\mathbb{Z}/101\mathbb{Z}$, $R=k[a,b,c]$, and
   $S=R/(a^4,b^4,c^4)$. Consider the $S$-module
   $N_S=\operatorname{coker}\begin{pmatrix}a&b&c\\b&c&a\end{pmatrix}$.
   Its third syzygy, $M_3=\Omega_S^3 N_S$, provides a higher matrix
   factorization example in the sense of Eisenbud and Peeva.
   We regard $M_3$ as an $R$-module by restriction of scalars along
   $R\twoheadrightarrow S$, then use the Koszul DG algebra on
   $(a^4,b^4,c^4)$ to construct its $L_\infty$-module structure.

   First construct the Koszul DG algebra and extract $M_3$ from a
   free resolution of $N_S$:
  Example
   kk = ZZ/101
   R = kk[a, b, c];
   S = R/ideal(a^4, b^4, c^4);
   A = koszulComplexDGA ideal S;
   phi = map(S, R);
   NS = coker (phi matrix{{R_0, R_1, R_2}, {R_1, R_2, R_0}});
   F = freeResolution(NS, LengthLimit => 4);
   M3 = coker F.dd_4;
   MR3 = pushForward(map(S, R), M3);
  Text
   Now construct the $L_\infty$ module and retrieve its underlying
   minimal semifree resolution:
  Example
   LM = lInfinityModule(A, MR3, EndDegree => 2, GradeLimit => 1)
   DGM = LM#"dgmodule";
  Text
   In the computed degrees, the numbers of semifree generators are
   $6,9,13,18$, respectively. These are the dimensions of
   $\operatorname{Tor}_i^S(M_3,k)$ for $0\leq i\leq 3$:
  Example
   ranks = apply(max(DGM.Degrees / first) + 1,
                 d -> #select(DGM.Degrees, e -> first e == d))
   assert(ranks == {6, 9, 13, 18})
   isMinimalSemifreeResolution DGM
  Text
   Check the module identity $L_3^M$ in both the symmetric and
   divided-power formulations. These checks agree in characteristic
   $101$ at this arity:
  Example
   isLInfinityModule(LM, 3)
   isLInfinityModule(LM, 3, DividedPower => true)
  Text
   The ternary operation $\mu_3$ is nonzero, so the $L_\infty$-module
   structure contains nontrivial higher operations. The following
   values lie in the span of the generators of $M_\infty$ in
   homological degree $3$, corresponding to indices $28$ through $45$
   in @TT "DGM.natural"@:
  Example
   T = gens A.natural
   v1 = mu(LM, {T#0, T#1}, 0);
   v2 = mu(LM, {T#0, T#2}, 2);
   coeffs = v -> select(pairs entries v, p -> p#1 != 0);
   coeffs v1
   coeffs v2
  Text
   The pairs give the basis index and its coefficient. With $e_j$ denoting
   the dual basis, the two values are $e_{43}$ and $-2e_{39}+e_{45}$.
References
 Eisenbud D., Peeva I., "Minimal Free Resolutions over Complete
 Intersections" (LNM 2152, Springer, 2016).
SeeAlso
 lInfinityModule
 isLInfinityModule
 mu
 minimalSemifreeResolution
///

doc ///
Key
 "L-infinity module on a hypersurface MF"
Headline
 A periodic resolution and its L-infinity-module structure
Description
  Text
   A matrix factorization of a polynomial $f$ is a pair of square
   matrices $(\varphi,\psi)$ satisfying
   $\varphi\psi=f\,\mathrm{id}=\psi\varphi$.
   Eisenbud's theory relates these factorizations to maximal
   Cohen--Macaulay modules over hypersurface rings. The alternating
   maps induced by $\varphi$ and $\psi$ give a periodic resolution of
   $\operatorname{coker}(\varphi)$ over the hypersurface.

   Here $f=x^4-y^2$ in $k[x,y]$, with the one-by-one factorization
   $\varphi=(x^2-y)$ and $\psi=(x^2+y)$:
  Example
   kk = ZZ/101;
   P = kk[x, y];
   U = matrix{{x^2-y}}; V = matrix{{x^2+y}};
   U*V
   Q = P/ideal(P_0^4 - P_1^2);
   A = koszulComplexDGA ideal Q;
   phi = map(Q, P);
   MQ = coker phi U;
   MR = pushForward(phi, MQ);
  Text
   The minimal semifree resolution of @TT "MR"@ over $A$ has one
   generator in each computed homological degree:
  Example
   LM = lInfinityModule(A, MR, EndDegree => 4, GradeLimit => 1)
   DGM = LM#"dgmodule";
   ranks = apply(max(DGM.Degrees / first) + 1,
                 d -> #select(DGM.Degrees, e -> first e == d))
   assert(ranks == {1, 1, 1, 1, 1, 1})
  Text
   The linear and quadratic terms of $x^2-y$ both contribute to
   the first relation. Evaluate their dual operations on $e_0$.
  Example
   {entries mu(LM,{P_1},0), entries mu(LM,{P_0,P_0},0)}
  Text
   In each case the only nonzero coordinate is in position one. Thus
   $\mu_2(y^\vee,e_0)=-e_1$ and
   $\mu_3(x^\vee,x^\vee,e_0)=2e_1$.
  Text
   Check the module identity in total arity three.
  Example
   isLInfinityModule(LM, 3)
References
 Eisenbud D., "Homological algebra on a complete intersection with an
 application to group representations" (TAMS 260, 1980);
 Knörrer H., "Cohen-Macaulay modules on hypersurface singularities I"
 (Invent. Math. 88, 1987).
SeeAlso
 lInfinityModule
 isLInfinityModule
///

doc ///
Key
 "L-infinity module on an Avramov-Buchweitz support variety"
Headline
 A cyclic module over a complete intersection of quadrics
Description
  Text
   Avramov and Buchweitz developed support varieties as homological
   invariants of modules over complete intersections. This example
   considers the complete intersection
   $Q=k[a,b,c]/(a^2,b^2,c^2)$ and its cyclic module
   $M=Q/(a+b+c)$.

   Regard $M$ as a module over $P=k[a,b,c]$ by restriction of scalars.
   The Koszul DG algebra on $(a^2,b^2,c^2)$ then yields an
   $L_\infty$-module structure on $M_\infty$:
  Example
   kk = ZZ/101;
   P = kk[a, b, c];
   Q = P/ideal(a^2, b^2, c^2);
   A = koszulComplexDGA ideal Q;
   phi = map(Q, P);
   MQ = coker matrix{{phi(P_0 + P_1 + P_2)}};
   MR = pushForward(phi, MQ);
   LM = lInfinityModule(A, MR, EndDegree => 3, GradeLimit => 1)
   DGM = LM#"dgmodule";
   ranks = apply(max(DGM.Degrees / first) + 1,
                 d -> #select(DGM.Degrees, e -> first e == d))
  Text
   The first five Betti numbers are $1,1,2,5,9$.
   In degree one there is the relation $a+b+c$ in the presentation
   of $M$. Its three coefficients are visible in the binary action.
  Example
   apply(gens P, u -> (entries mu(LM,{u},0))#1)
  Text
   The list @TT "{1,1,1}"@ means each degree-one Lie generator sends
   $e_0$ to $e_1$. Check minimality and the module identity in total
   arity three.
  Example
   isMinimalSemifreeResolution DGM
   isLInfinityModule(LM, 3)
References
 Avramov L. L., Buchweitz R.-O., "Support varieties and cohomology over
 complete intersections" (Invent. Math. 142, 2000).
SeeAlso
 lInfinityModule
 isLInfinityModule
///

doc ///
Key
 "L-infinity module over an ADE singularity"
Headline
 The residue field of an ADE hypersurface as an L-infinity module
Description
  Text
   Let $Q=k[x,y]/(x^3+y^2)$ be the $A_2$ cusp, and let
   $k=Q/(x,y)$ be its residue field. The minimal free resolution of
   $k$ over $Q$ is periodic of period two from homological degree one.
   This example constructs the associated $L_\infty$-module structure
   using the Koszul DG algebra of the defining equation:
  Example
   kk = ZZ/101;
   P = kk[x, y];
   Q = P/ideal(x^3 + y^2);
   A = koszulComplexDGA ideal Q;
   M = coker vars P;
   LM = lInfinityModule(A, M, EndDegree => 4, GradeLimit => 1)
   DGM = LM#"dgmodule";
   ranks = apply(max(DGM.Degrees / first) + 1,
                 d -> #select(DGM.Degrees, e -> first e == d))
   assert(ranks == {1, 2, 2, 2, 2, 2})
   isLInfinityModule(LM, 3)
   entries mu(LM,{P_0,P_0},2)
  Text
   In this basis the only nonzero entry is $-2$ in position four, so
   $\mu_3(x^\vee,x^\vee,e_2)=-2e_4$.
  Text
   The same construction applies to the hypersurfaces
   $k[x,y]/(x^3+xy^2)$ of type $D_4$,
   $k[x,y]/(x^4+y^3)$ of type $E_6$, and
   $k[x,y]/(x^5+y^3)$ of type $E_8$.
   In each case, the corresponding resolution has generator counts
   $1,2,2,2,2,2$ through homological degree five.
References
 Buchweitz R.-O., Greuel G.-M., Schreyer F.-O., "Cohen-Macaulay modules on
 hypersurface singularities II" (Invent. Math. 88, 1987);
 Yoshino Y., "Cohen-Macaulay Modules over Cohen-Macaulay Rings" (LMS
 Lecture Note Series 146).
SeeAlso
 lInfinityModule
 isLInfinityModule
///

doc ///
Key
 "L-infinity module over a Golod / Burch ring"
Headline
 Examples beyond complete intersections
Description
  Text
   When the defining ideal is not generated by a regular sequence,
   its Koszul DG algebra has nonzero positive-degree homology.
   An acyclic closure adjoins generators to kill this homology:
   generators $T_{2,j}$ kill homology in degree one, generators
   $T_{3,j}$ kill homology in degree two, and so on.

   First consider the Golod ring $Q=k[a,b]/(a^2,ab,b^2)$.
   The residue field has Poincaré series $P_k^Q(t)=1/(1-2t)$ and
   Betti numbers $\beta_n^Q(k)=2^n$:
  Example
   kk = ZZ/101;
   P = kk[a, b];
   I = ideal(a^2, a*b, b^2);
   Q = P/I;
   K = koszulComplexDGA I;
   A = acyclicClosure(K, EndDegree => 3);
   LM = lInfinityModule(A, coker vars P, EndDegree => 2, GradeLimit => 1)
   DGM = LM#"dgmodule";
   ranks = apply(max(DGM.Degrees / first) + 1,
                 d -> #select(DGM.Degrees, e -> first e == d))
   assert(ranks == {1, 2, 4, 8})
   isLInfinityModule(LM, 3)
  Text
   Next consider the Burch ring $Q=k[a,b]/(a^2,ab)$, which has
   a defining ideal of height one with two minimal generators. Compute
   the first four ranks of the residue-field resolution.
  Example
   P = kk[a, b];
   I = ideal(a^2, a*b);
   Q = P/I;
   K = koszulComplexDGA I;
   A = acyclicClosure(K, EndDegree => 3);
   LM = lInfinityModule(A, coker vars P, EndDegree => 2, GradeLimit => 1)
   apply({0,1,2,3}, d -> #select(degrees LM, v -> first v == d))
   isLInfinityModule(LM, 3)
  Text
   The ranks are $1,2,3,5$, the first four terms of the Fibonacci
   recurrence for this ring. The preceding square-zero example has
   ranks $1,2,4,8$ in the same degrees.
References
 Golod E. S., "On the homology of some local rings" (Dokl. Akad. Nauk
 SSSR 144, 1962);
 Burch L., "On ideals of finite homological dimension in local rings"
 (Proc. Camb. Phil. Soc. 64, 1968);
 Avramov L. L., "Infinite Free Resolutions", §5, in Six Lectures on
 Commutative Algebra (Bellaterra, 1996), Progress in Mathematics 166,
 Birkhäuser, 1998;
 Dao H., Kobayashi T., Takahashi R., "Burch ideals and Burch rings",
 Algebra & Number Theory 14 (2020), no. 8, 2121–2150.
 @HREF {"https://doi.org/10.2140/ant.2020.14.2121", "DOI"}@.
SeeAlso
 lInfinityModule
 isLInfinityModule
 acyclicClosure
///

doc ///
Key
 "Holonomy over a non-complete-intersection ring"
Headline
 Noncentral Lie generators acting on a module over a square-zero ring
Description
 Text
  Let
  $$R=\mathbb Q[x,y]/(x^2,xy,y^2),\qquad M=R/(x).$$
  The defining ideal has height $2$ and requires three generators,
  so $R$ is not a complete intersection. The module $M$ has
  $\mathbb Q$-basis $\{1,y\}$; it is not the residue field.
  This example computes its holonomy representation through
  module degree $3$, including nonzero actions by Lie generators
  of degree $3$.

  Build a model far enough to kill algebra homology through degree
  $2$, and compute the semifree module to the same cycle-killing depth.
  Present $M$ over the polynomial base ring as $S/(x,y^2)$:
 Example
  S = QQ[x,y];
  R = S/ideal(x^2,x*y,y^2);
  A = acyclicClosure(koszulComplexDGA ideal R,EndDegree=>2);
  M = coker matrix{{x_S,y_S^2}};
  LM = lInfinityModule(A,M,EndDegree=>2,GradeLimit=>3);
  H = holonomyRepresentation(LM,GradeLimit=>3);
  G = lieAlgebra H;
  b = gens G;
  degrees G
  degrees H
 Text
  The retained Lie dimensions in degrees $1,2,3$ are $2,3,2$.
  Write the basis, in the printed order, as
  $$\alpha=x^\vee,\ \beta=y^\vee;\qquad
  p=T_{1,1}^\vee,\ q=T_{1,2}^\vee,\ r=T_{1,3}^\vee;\qquad
  u=T_{2,1}^\vee,\ v=T_{2,2}^\vee.$$
  The dual module basis has dimensions $1,1,2,4$ in degrees
  $0,1,2,3$. Its internal degree is the negative of its cohomological
  degree throughout this example.

  These ranks can be compared independently with a minimal resolution
  over $R$. Since $\Omega_R^1M=(x)\cong\mathbb Q(-1)$ and the maximal
  ideal of $R$ has square zero and dimension $2$, the Betti ranks are
  $1,1,2,4,\ldots$:
 Example
  N = coker matrix{{R_0}};
  C = freeResolution(N,LengthLimit=>3);
  apply(toList(0..3),i->rank C_i)
 Text
  The output is @TT "{1,1,2,4}"@. The coordinate space used below is
  $(k\otimes_A F)^\vee$ for the computed semifree DG module $F$.
  Its ranks in degrees $0$ through $3$ agree with this independent
  $R$-resolution. The rank comparison is not a completeness
  certificate; the matrix identities below concern the computed
  model and its finite degree range.

  @SUBSECTION "Read the two degree-one action matrices"@

  Matrices act on column vectors. Column $j$ is the image of the
  $j$th dual module basis element, so multiplication $XY$ applies
  $Y$ first and then $X$:
 Example
  W = coordinateSpace H;
  e = apply(dim H,i->W_i);
  X = actionMatrix(H,0);
  Y = actionMatrix(H,1);
  X
  Y
 Text
  Here $X=\rho(\alpha)$ and $Y=\rho(\beta)$.
  Indexing the displayed basis by $e_0,\ldots,e_7$, its degree groups are
  $$\{e_0\},\qquad \{e_1\},\qquad
  \{e_2,e_3\},\qquad \{e_4,e_5,e_6,e_7\}.$$
  The complete nonzero-column descriptions of these two matrices are
  $$\begin{aligned}
  Xe_0&=e_1,&Xe_1&=e_3,&Xe_2&=e_5,&Xe_3&=e_7,\\
  Ye_1&=e_2,&Ye_2&=e_4,&Ye_3&=e_6.
  \end{aligned}$$
  All their other columns are zero in this finite coordinate space.
  In particular, $Ye_0=0$, whereas $Y$ does not vanish on the
  higher module degrees.

  The ordered basis has a useful word description. Put $z=e_0$.
  Then
  $$\begin{aligned}
  e_0&=z,& e_1&=\alpha z,&e_2&=\beta\alpha z,&e_3&=\alpha^2z,\\
  e_4&=\beta^2\alpha z,&e_5&=\alpha\beta\alpha z,
       &e_6&=\beta\alpha^2z,&e_7&=\alpha^3z,
  \end{aligned}\qquad \beta z=0.$$
  Products in this display mean successive actions, with the
  rightmost factor acting first. They are not products in $R$:
  $x^2=0$ in $R$, whereas $\alpha^2z=e_3\neq0$:
 Example
  Y*e#0 == 0
  e#2 == Y*X*e#0
  e#5 == X*Y*X*e#0
  e#6 == Y*X*X*e#0
 Text
  Each equality is @TT "true"@. This identifies actual basis vectors
  rather than just matching their degrees.

  To read the actions of $x^\vee$ and $y^\vee$ from module degree
  two to module degree three, give the source degree first.
 Example
  actionMatrix(H,b#0,2,3)
  actionMatrix(H,b#1,2,3)
 Text
  These are the maps from $\langle e_2,e_3\rangle$ to
  $\langle e_4,e_5,e_6,e_7\rangle$:
  $$X_{2\to3}=
  \begin{pmatrix}0&0\\1&0\\0&0\\0&1\end{pmatrix},
  \qquad
  Y_{2\to3}=
  \begin{pmatrix}1&0\\0&0\\0&1\\0&0\end{pmatrix}.$$
  For example, the first column of $X_{2\to3}$ says $Xe_2=e_5$.

  @SUBSECTION "Odd Lie inputs give an anticommutator"@

  The degree-one elements $\alpha,\beta$ are odd. Therefore the
  representation identity reads
  $$\rho([\alpha,\beta])=XY+YX.$$
  The quadratic differentials give
  $[\alpha,\alpha]=2p$, $[\alpha,\beta]=q$, and
  $[\beta,\beta]=2r$:
 Example
  P2 = actionMatrix(H,2);
  Q2 = actionMatrix(H,3);
  R2 = actionMatrix(H,4);
  P2 == X*X
  Q2 == X*Y+Y*X
  R2 == Y*Y
  Q2*e#1 == e#5+e#6
 Text
  All four equalities return @TT "true"@.
  The last one exhibits both terms of the anticommutator:
  $XYe_1=e_5$ and $YXe_1=e_6$.
  The degree-two actions, including every nonzero column, are
  $$\begin{aligned}
  \rho(p)e_0&=e_3,&\rho(p)e_1&=e_7,\\
  \rho(q)e_0&=e_2,&\rho(q)e_1&=e_5+e_6,\\
  \rho(r)e_1&=e_4.
  \end{aligned}$$
  Thus $\rho(r)e_0=0$ does not imply $\rho(r)=0$.

  @SUBSECTION "Degree-two Lie elements are not central"@

  The next model differentials include
  $$d(T_{2,1})=xT_{1,2}-yT_{1,1},\qquad
    d(T_{2,2})=xT_{1,3}-yT_{1,2}.$$
  They give $[\alpha,q]=u$ and $[\alpha,r]=v$.
  Since the Lie degrees are $1$ and $2$, the graded representation
  identity now uses a commutator with a minus sign:
 Example
  U3 = actionMatrix(H,5);
  V3 = actionMatrix(H,6);
  lieRepresentative(G,bracket(G,{b#0,b#3})) == b#5
  X*Q2-Q2*X == U3
  X*R2-R2*X == V3
  U3 == X*X*Y-Y*X*X
  V3 == X*Y*Y-Y*Y*X
  actionMatrix(H,5,0,3)
  actionMatrix(H,6,0,3)
 Text
  The equalities are @TT "true"@. The final two columns are
  $$\begin{pmatrix}0\\0\\-1\\0\end{pmatrix},
  \qquad
  \begin{pmatrix}-1\\0\\0\\0\end{pmatrix}.$$
  They mean $\rho(u)e_0=-e_6$ and $\rho(v)e_0=-e_4$.
  In particular $\rho(q)$ fails to commute with $X$, and their
  nonzero commutator is precisely the action of the degree-three
  Lie element $u$.

  These are still binary operations:
  $\rho(u)=\mu_2(u,-)$ and $\rho(v)=\mu_2(v,-)$.
  Lie degree $3$ specifies the degree of the first input; it does
  not select the ternary operation $\mu_3$.
  The object $H$ retains $\mu_2$ for every stored Lie generator.
  Higher-arity operations remain accessible through $LM$ and @TO mu@.
 Example
  holonomyAction(H,b#5,e#0) == -e#6
  holonomyAction(H,b#6,e#0) == -e#4
  isHolonomyRepresentation H
 Text
  These checks return @TT "true"@.
  The final check verifies the graded representation identity for
  every pair of stored Lie basis elements, not only the pairs
  displayed above.

  The matrices describe the computed quotient in module degrees
  at most $3$. An action on degree $3$ that would land in degree
  $4$ or higher is zero here because its target has been omitted.
  This finite cutoff is why $X$ and $Y$ are nilpotent as displayed
  matrices; it does not assert nilpotence of the untruncated action.
  The selected Lie quotient
  has degrees at most $3$, and omitted Lie generators act trivially
  on this computed module range.
SeeAlso
 holonomyRepresentation
 actionMatrix
 holonomyAction
 isHolonomyRepresentation
 homotopyLieAlgebra
///


doc ///
Key
 (structureConstants,LInfinity,ZZ)
Headline
 List the nonzero higher brackets of one arity
Usage
 C = structureConstants(L,r)
Description
 Text
  An entry with key @TT "{i_1,...,i_r}"@ gives $\ell_r$ on the corresponding
  elements of @TT "gens L"@, in that order. Indices start at zero. The table
  keeps one weakly increasing tuple for each nonzero basis value; graded
  skew-symmetry supplies the other orders. Its values are the same model
  representatives returned by @TO bracket@.
 Example
  S = QQ[x,y];
  A = koszulComplexDGA ideal(x^3+x*y^2);
  L = lInfinity A;
  C = structureConstants(L,3)
  (a,b,t) = toSequence gens L;
  C#{0,1,1} == bracket(L,{a,b,b})
 Text
  The two terms of $dT=x^3+xy^2$ give precisely two entries.
  The key @TT "{0,0,0}"@ gives $6T$, representing
  $\ell_3(x^\vee,x^\vee,x^\vee)=6T^\vee$.
  The key @TT "{0,1,1}"@ gives $2T$. There are two ways to pair the two
  copies of $y^\vee$ with $y^2$.
SeeAlso
 (bracket,LInfinity,List)
 (structureConstants,LInfinityModule,ZZ)
 [structureConstants,DividedPower]
///

doc ///
Key
 (structureConstants,LInfinityModule,ZZ)
Headline
 List the nonzero higher module actions of one arity
Usage
 C = structureConstants(LM,r)
Description
 Text
  The arity $r$ counts the module input. An entry with key
  @TT "({i_1,...,i_(r-1)},j)"@ gives the value of $\mu_r$ on the indicated
  Lie generators and module generator $j$. The Lie indices refer to
  @TT "gens lieAlgebra LM"@ and appear in weakly increasing order.
  The module index refers to @TT "gens LM"@. Both start at zero.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^4);
  LM = lInfinityModule(A,coker vars S,EndDegree=>1);
  C = structureConstants(LM,4)
  entries C#({0,0,0},1)
 Text
  The only entry has coordinates $\{0,0,6\}$. If $a=x^\vee$ and
  $e_0,e_1,e_2$ denote the module basis, it says
  $\mu_4(a,a,a,e_1)=6e_2$. The coefficient comes from the term
  $x^3f_1$ in $D(f_2)$. Every other basis input of arity four gives zero.
SeeAlso
 (mu,LInfinityModule,List,ZZ)
 (structureConstants,LInfinity,ZZ)
 [structureConstants,DividedPower]
///

doc ///
Key
 [structureConstants,DividedPower]
Headline
 Tabulate higher operations on a divided-power basis
Usage
 structureConstants(L,r,DividedPower=>true)
 structureConstants(LM,r,DividedPower=>true)
Description
 Text
  If an algebra input occurs $m$ times, ordinary evaluation contributes
  a factor of $m!$. The divided-power table reads the coefficient directly
  from the differential with the same suspension and ordering signs.
  Its entries agree with @TO bracketGamma@ or @TO muGamma@.
 Example
  S = (ZZ/3)[x];
  A = koszulComplexDGA ideal(x^3);
  L = lInfinity A;
  structureConstants(L,3)
  structureConstants(L,3,DividedPower=>true)
 Text
  The ordinary table is empty because $6=0$ in $\mathbb F_3$. The
  divided-power table has one entry, with key @TT "{0,0,0}"@ and
  value $T$. It records the coefficient of $x^3$ in $dT$.
SeeAlso
 (structureConstants,LInfinity,ZZ)
 (structureConstants,LInfinityModule,ZZ)
 DividedPower
///

doc ///
Key
 (lInfinityModule,DGModule)
Headline
 Read the higher module operations from a given semifree differential
Usage
 LM = lInfinityModule F
Inputs
 F:DGModule
Outputs
 LM:LInfinityModule
Description
 Text
  Let $F$ be a minimal semifree DG module over a minimal model $A$.
  The method takes the basis of $F$ in the order supplied. A term of
  algebra word length $r$ in its differential determines an operation
  $\mu_{r+1}$ on the dual basis of the fiber $k\otimes_A F$.
  @TO EndDegree@ applies only when constructing a resolution from a module.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^4);
  F = freeDGModule(A,{{0,0},{1,2}});
  setDiff(F,{0,promote(x^2,A.natural)*F.natural_0});
  LM = lInfinityModule F
  a = first gens lieAlgebra LM;
  e = gens LM;
  entries mu(LM,{a},e#0)
  entries mu(LM,{a,a},e#0)
 Text
  Here $D(f_1)=x^2 f_0$. Its linear part vanishes, so $x^\vee$
  acts trivially on $e_0$ by the binary action. The quadratic term
  gives $\mu_3(x^\vee,x^\vee,e_0)=2e_1$.
  The two coordinate lists are @TT "{0,0}"@ and @TT "{0,2}"@.
SeeAlso
 (lInfinityModule,DGAlgebra,Module)
 (lieAlgebra,LInfinityModule)
 muGamma
///

doc ///
Key
 (lieAlgebra,LInfinityModule)
Headline
 Retrieve the higher Lie algebra acting on a module
Usage
 L = lieAlgebra LM
Inputs
 LM:LInfinityModule
Outputs
 L:LInfinity
Description
 Text
  This method returns the $L_\infty$ algebra stored by the module,
  with the bounds chosen when @TO lInfinityModule@ was called.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^4);
  LM = lInfinityModule(A,coker vars S,EndDegree=>1,GradeLimit=>2,OrderLimit=>4);
  L = lieAlgebra LM
  (L#"GradeLimit",L#"OrderLimit")
  (a,t) = toSequence gens L;
  bracket(L,{a,a,a,a})
  e = gens LM;
  entries mu(LM,{a,a,a},e#1)
 Text
  The bounds are @TT "(2,4)"@. The equation $dT=x^4$ gives
  $\ell_4(a,a,a,a)=24t$ for $a=x^\vee$ and $t=T^\vee$.
  Acting on $e_1$ with three copies of $a$ gives
  $\mu_4(a,a,a,e_1)=6e_2$, printed as @TT "{0,0,6}"@.
SeeAlso
 (lInfinity,LInfinityModule)
 (lieAlgebra,HolonomyRepresentation)
 mu
///

doc ///
Key
 (actionMatrix, HolonomyRepresentation, ZZ, ZZ, ZZ)
Headline
 Read a degree block of the action of a Lie basis generator
Usage
 actionMatrix(H,i,p,q)
Description
 Text
  Let $b_i$ be generator $i$ in @TT "gens lieAlgebra H"@, counting from zero.
  The columns of this block are indexed by @TT "basis(p,H)"@ and its rows
  by @TT "basis(q,H)"@. Column $j$ records the degree-$q$ component of
  $\rho(b_i)$ applied to the $j$th basis element of degree $p$.
 Example
  S = QQ[x,y];
  A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2),EndDegree=>1);
  M = coker matrix{{x_S,y_S^2}};
  H = holonomyRepresentation(A,M,EndDegree=>1);
  actionMatrix(H,1,0,1)
  actionMatrix(H,1,1,2)
 Text
  Generator $1$ represents $y^\vee$. It kills the degree-zero generator.
  The second matrix is the column $(1,0)^{\mathsf T}$, so $y^\vee$ takes
  the degree-one generator to the first of the two degree-two generators.
SeeAlso
 actionMatrix
 (basis, ZZ, HolonomyRepresentation)
///

doc ///
Key
 (actionMatrix, HolonomyRepresentation, Vector, ZZ, ZZ)
Headline
 Read the action of a Lie coordinate vector between two module degrees
Usage
 actionMatrix(H,u,p,q)
Description
 Text
  A Lie coordinate vector may have components in several degrees.
  Specifying the source degree $p$ and the target degree $q$ selects
  the action of its component of Lie degree $q-p$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^2);
  H = holonomyRepresentation(A,coker vars S,EndDegree=>1);
  U = coordinateSpace lieAlgebra H;
  u = U_0 + 3*U_1;
  actionMatrix(H,u,0,1)
  actionMatrix(H,u,0,2)
 Text
  The two one-entry matrices are $(1)$ and $(3)$.
  The degree-one summand sends $e_0$ to $e_1$ and the degree-two
  summand sends $e_0$ to $3e_2$.
SeeAlso
 actionMatrix
 (actionMatrix, HolonomyRepresentation, Vector)
 coordinateSpace
///

doc ///
Key
 (actionMatrix, HolonomyRepresentation, RingElement, ZZ, ZZ)
Headline
 Read a holonomy degree block using a model representative
Usage
 actionMatrix(H,u,p,q)
Description
 Text
  A model generator represents its dual Lie basis element. For the
  residue field of $\mathbb Q[x]/(x^3)$, compare the actions of $x^\vee$
  and the degree-two element $t^\vee$ dual to the relation $d(t)=x^3$.
 Example
  S = QQ[x];
  A = koszulComplexDGA ideal(x^3);
  H = holonomyRepresentation(A,coker vars S,EndDegree=>1);
  t = last gens lieAlgebra H;
  actionMatrix(H,x_S,0,1)
  actionMatrix(H,x_S,1,2)
  actionMatrix(H,t,0,2)
 Text
  The entries are $1,0,1$. Thus $x^\vee$ takes the degree-zero basis
  element to degree one and kills it on the next application.
  The action of $t^\vee$ takes the degree-zero basis element directly
  to the degree-two basis element.
SeeAlso
 actionMatrix
 (actionMatrix, HolonomyRepresentation, RingElement)
///

-* Test section *-
TEST /// --graded skew symmetry:
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
lastCyclesDegree = 4
KR = koszulComplexDGA(ideal R)
A = acyclicClosure(KR, EndDegree => lastCyclesDegree)
--
assert(bracketMatrix(A,1,1) - transpose bracketMatrix(A,1,1) == 0)
assert(bracketMatrix(A,2,1) + transpose bracketMatrix(A,1,2) == 0)
assert(bracketMatrix(A,2,2) + transpose bracketMatrix(A,2,2) == 0)
assert(bracketMatrix(A,2,3) + transpose bracketMatrix(A,3,2) == 0)
assert(bracketMatrix(A,3,3) - transpose bracketMatrix(A,3,3) == 0)

///

-- isGradedSkewSymmetric / isGradedJacobi / isLieSuperalgebra:
-- exercise the Lie-superalgebra axiom checkers. Skew-symmetry is matrix-based
-- (cheap); Jacobi at (d,e,f) iterates over |Pi^d|*|Pi^e|*|Pi^f| basis triples,
-- so we keep the in-test Jacobi checks restricted to small triples and rely on
-- the hand-coded Jacobi test above for higher-grade coverage.
TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
A = acyclicClosure(koszulComplexDGA ideal R, EndDegree => 4)
-- type tests
assert instance(isGradedSkewSymmetric(A, 1, 1), Boolean)
assert instance(isGradedJacobi(A, 1, 1, 1), Boolean)
assert instance(isLieSuperalgebra(A, 1), Boolean)
-- property tests: graded skew-symmetry on every available Pi^d x Pi^e
assert isGradedSkewSymmetric(A, 1, 1)
assert isGradedSkewSymmetric(A, 2, 1)
assert isGradedSkewSymmetric(A, 1, 2)
assert isGradedSkewSymmetric(A, 2, 2)
assert isGradedSkewSymmetric(A, 2, 3)
assert isGradedSkewSymmetric(A, 3, 3)
-- property tests: graded Jacobi on the smallest meaningful triples
assert isGradedJacobi(A, 1, 1, 1)
assert isGradedJacobi(A, 1, 1, 2)
-- These mixed-degree brackets include diagonal terms with scalar multiples of a generator.
assert isGradedJacobi(A, 1, 2, 3)
assert isGradedJacobi(A, 3, 2, 1)
-- Check all basis triples with input degrees at most three.
assert isLieSuperalgebra(A, 3)
-- boundary tests: vacuous-truth cases (no generators in that grade)
assert isGradedSkewSymmetric(A, 99, 1)
assert isGradedJacobi(A, 99, 1, 1)
assert isLieSuperalgebra(A,0)
assert(#bracket(A,99,1)==0)
B=koszulComplexDGA ideal(S_0)
assert(try (isGradedSkewSymmetric(B,99,1); false) else true)
assert(try (isGradedJacobi(B,99,1,1); false) else true)
assert(try (isLieSuperalgebra(B,0); false) else true)
assert(try (bracket(B,99,1); false) else true)
///

-- Check the additional identities in characteristics two and three.
TEST ///
S2 = (ZZ/2)[x,y]
R2 = S2/ideal(x^2, y^2, x*y)
A2 = acyclicClosure(koszulComplexDGA ideal R2, EndDegree => 3)
assert(char A2.natural == 2)
assert isLieSuperalgebra(A2, 2)
S3 = (ZZ/3)[x,y]
R3 = S3/ideal(x^2, y^2, x*y)
A3 = acyclicClosure(koszulComplexDGA ideal R3, EndDegree => 3)
assert(char A3.natural == 3)
assert isLieSuperalgebra(A3, 2)
///

///
restart
needsPackage "DGAlgebras"
loadPackage "HomotopyLieAlgebra"
check HomotopyLieAlgebra
///

TEST ///
--gradedJacobi identity:
--[U,[V,W]] = [[U,V],W] + (-1)^(1+homdeg U)*(1+homdeg V))* [V,[U,W]]

kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
lastCyclesDegree = 4
KR = koszulComplexDGA(ideal R)
A = acyclicClosure(KR, EndDegree => lastCyclesDegree)

L = for d from 2 to 3 list for e from 2 to 3 list for f from 2 to 3 list(
    Pid = allgens(A,d-1);
    Pie = allgens(A,e-1);
    Pif = allgens(A,f-1);
    L = flatten flatten flatten apply(
	Pid, U -> 
	   apply(Pie,V ->
	       apply(Pif, W -> (
bracket(A, {U,bracket(A,{V,W})}) == 
bracket(A, {bracket(A, {U,V}),W}) + (-1)^(d*e) * bracket(A, {V,bracket(A,{U,W})})
    ))))
)
L1 = flatten flatten flatten L
assert (all(L1, ell ->ell));
///




-- allgens: lists the generators, partitioned by homological degree
TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
A = acyclicClosure(koszulComplexDGA ideal R, EndDegree => 4)
assert instance(allgens A, List)
assert instance(allgens(A,2), List)
-- allgens(A,d) lists exactly the homological-degree-d generators...
assert all(allgens(A,2), T -> first degree T == 2)
-- ...and is a sublist of allgens A
assert isSubset(allgens(A,2), allgens A)
-- a degree with no generators yields the empty list (boundary)
assert(allgens(A,999) === {})
///

-- bracketMatrix: a concrete value, pinning the sign logic
TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
A = acyclicClosure(koszulComplexDGA ideal R, EndDegree => 4)
b = bracketMatrix(A,1,1)
assert instance(b, Matrix)
p1 = allgens(A,1)
assert(b == matrix{{2*p1_0, p1_2}, {p1_2, 2*p1_1}})
for de in {(99,1),(1,99),(99,99)} do (
    emptyTable := bracketMatrix(A,de#0,de#1);
    assert(ring emptyTable === A.natural);
    assert(numrows emptyTable == #allgens(A,de#0-1));
    assert(numcols emptyTable == #allgens(A,de#1-1)))
///

-- bracket (list form): value, type, the zero-input boundary, and a malformed-input error
TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
A = acyclicClosure(koszulComplexDGA ideal R, EndDegree => 4)
p0 = allgens(A,0)
b = bracket(A, {p0_0, p0_1})
assert instance(b, RingElement)
-- [x,y] is the third degree-1 generator
assert(b == (allgens(A,1))_2)
-- a zero argument is handled gracefully (boundary)
assert(bracket(A, {0_(A.natural), 0_(A.natural)}) == 0)
-- a malformed (one-element) list must error
assert(try (bracket(A, {p0_0}); false) else true)
///

-- bracket: the (A,d,e) HashTable form has the right type and shape; the action form runs
TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
A = acyclicClosure(koszulComplexDGA ideal R, EndDegree => 4)
p0 = allgens(A,0)
p1 = allgens(A,1)
H = bracket(A,1,1)
assert instance(H, HashTable)
-- one entry per (u,v,T) triple over degrees d-1, e-1, d+e-1
assert(#H == (#p0) * (#p0) * (#p1))
assert(bracket(A,{p0_0,p0_1},2*p1_2+p1_0)==2)
assert(bracket(A,{p0_0,p0_1},0_S)==0)
assert(try (bracket(A,{p0_0,p0_1},1_(A.natural)); false) else true)
assert(try (bracket(A,{p0_0,p0_1},p0_0*p1_0); false) else true)
///

-- ad: the adjoint-action matrix, and the documented bracketMatrix row-sum identity
TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
A = acyclicClosure(koszulComplexDGA ideal R, EndDegree => 4)
U = sum allgens(A,0)
adU = ad(A,U,1)
assert instance(adU, Matrix)
-- concrete value
assert(adU - matrix{{2,0},{0,2},{1,1}} == 0)
-- documented identity: re-attaching the generators recovers the bracketMatrix column sums
assert(matrix{allgens(A,1)} * adU == matrix{{1,1}} * bracketMatrix(A,1,1))
for e in {0,99} do (
    emptyAction := ad(A,U,e);
    assert(ring emptyAction === A.natural);
    assert(numrows emptyAction == #allgens(A,e));
    assert(numcols emptyAction == #allgens(A,e-1)))
S=QQ[x,y,Degrees=>{1,2}]
A=koszulComplexDGA ideal(x*y,x^2,y^2)
(a,b)=toSequence allgens(A,0)
assert(entries ad(A,x_S,1)=={{0,1},{2,0},{0,0}})
assert(entries ad(A,x_S+y_S,1)=={{1,1},{2,0},{0,2}})
assert(try (ad(A,a^2,1); false) else true)
assert(try (ad(A,a+first allgens(A,1),1); false) else true)
assert(ad(A,0_S,1)==0*id_((A.natural)^2))
///

-- LInfinity smoke test: type + constructor, ell_2 sanity check, ell_3
-- vanishes on this (Koszul-like) example.
TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2, y^2, x*y)
A = acyclicClosure(koszulComplexDGA ideal R, EndDegree => 4)
L = lInfinity(A, GradeLimit => 2)
p1 = allgens(A, 0)
-- type tests
assert instance(L, LInfinity)
assert instance(bracket(L, {p1#0, p1#1}), RingElement)
assert instance(isLInfinity(L, 3), Boolean)
-- Both entry points use the same binary bracket, including mixed parity.
-- Its sign is fixed independently in the exterior-reordering test below.
-- We sample (1,1), (1,2), (2,2) to cover the diagonal, mixed-parity, and
-- both-odd cases without iterating the full degree range.
hd := U -> first degree U;
for de in {(1,1), (1,2), (2,2)} do
    for U in allgens(A, de#0 - 1) do for V in allgens(A, de#1 - 1) do
	assert(bracket(L, {U, V}) ==
	    bracket(A, {U, V}));
-- ell_3 vanishes on this Koszul-like example (no higher operations beyond ell_2)
for U in allgens(A, 0) do for V in allgens(A, 0) do for W in allgens(A, 0) do
    assert(bracket(L, {U, V, W}) == 0);
///

-- isLInfinity: the L_3 identity (= graded Jacobi for ell_2) on every basis
-- triple with Lie grades in [1, GradeLimit]. Bounded at GradeLimit=2 to keep
-- the test under the time budget; higher arities / grades work the same way
-- but are cubic in the basis size.
TEST ///
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2, y^2, x*y)
A = acyclicClosure(koszulComplexDGA ideal R, EndDegree => 4)
L = lInfinity(A, GradeLimit => 2)
assert isLInfinity(L, 3)
///

-- LInfinityModule smoke test: build the L_infty holonomy representation of
-- M = k over the complete intersection R = k[x,y]/(x^2, y^2), and check that
-- (a) the strict action mu_2 reproduces the corresponding entry of the
-- module differential, and (b) mu_3 = 0 (this is a strict module since the
-- semifree-resolution differentials are all linear-in-X over a CI).
TEST ///
kk = ZZ/101
S = kk[x, y]
R = S/ideal(x^2, y^2)
A = koszulComplexDGA ideal R
Min = coker matrix{{x_S, y_S}}
LM = lInfinityModule(A, Min, EndDegree => 3)
DGM = LM#"dgmodule"
assert instance(LM, LInfinityModule)
-- type tests for mu
assert instance(mu(LM, {x_S}, 0), Vector)
-- strict-action sanity: d(e_2) = x*e_0, so mu_2(x, e_0^vee) = e_2^vee
assert(mu(LM, {x_S}, 0) == (DGM.natural)_2)
-- and d(e_1) = y*e_0, so mu_2(y, e_0^vee) = e_1^vee
assert(mu(LM, {y_S}, 0) == (DGM.natural)_1)
-- mu_3 vanishes on this CI example (strict module structure)
algBasis := {x_S, y_S} | allgens(A, 1)
for a1 in algBasis do for a2 in algBasis do for k from 0 to min(2, rank DGM.natural - 1) do
    assert(mu(LM, {a1, a2}, k) == 0_(DGM.natural))
///

-- isLInfinityModule: the L_3^M identity holds on every basis triple for the
-- minimal semifree resolution of M = k over the complete intersection
-- R = k[x,y]/(x^2, y^2). GradeLimit=2 bounds the algebra-side basis to the
-- nonempty Pi^1 and Pi^2 pieces.
TEST ///
kk = ZZ/101
S = kk[x, y]
R = S/ideal(x^2, y^2)
A = koszulComplexDGA ideal R
Min = coker matrix{{x_S, y_S}}
LM = lInfinityModule(A, Min, EndDegree => 3, GradeLimit => 2)
assert isLInfinityModule(LM, 3)
///

-- Non-Koszul quadratic ring: nontrivial ell_3.
-- R = k[x,y,z]/(x^2, xy - z^2, yz).
-- The cubic part of the minimal differential d(T_(4,2)) = -T_(1,1)*T_(1,2)*T_(1,3) + ...
-- dualizes to nonzero ell_3 on the three Pi^2 generators. Confirms that
-- non-Koszul rings give honest L_infinity (not just strict Lie) homotopy
-- Lie algebras.
TEST ///
kk = ZZ/101
S = kk[x, y, z]
R = S/ideal(x^2, x*y - z^2, y*z)
A = acyclicClosure(koszulComplexDGA ideal R, EndDegree => 3)
L = lInfinity(A, GradeLimit => 2)
Pi2 = allgens(A, 1)
-- ell_3 on (T_(1,1), T_(1,2), T_(1,3)) is nonzero (= +- T_(4,2))
nonzero := bracket(L, {Pi2_0, Pi2_1, Pi2_2})
assert(nonzero != 0)
-- graded antisymmetry of ell_3 on this triple of even-Lie-grade args:
-- all 6 permutations should give sign(sigma) * (canonical value)
canon := bracket(L, {Pi2_0, Pi2_1, Pi2_2})
assert(bracket(L, {Pi2_0, Pi2_2, Pi2_1}) == -canon)
assert(bracket(L, {Pi2_1, Pi2_0, Pi2_2}) == -canon)
assert(bracket(L, {Pi2_1, Pi2_2, Pi2_0}) ==  canon)
assert(bracket(L, {Pi2_2, Pi2_0, Pi2_1}) ==  canon)
assert(bracket(L, {Pi2_2, Pi2_1, Pi2_0}) == -canon)
-- L_3 identity (graded Jacobi for ell_2) still holds on this ring
assert isLInfinity(L, 3)
///

-- Quartic CI: complete intersection with higher-degree relations.
-- R = k[a,b,c]/(a^4, b^4, c^4). Verifies the L_infinity module construction on
-- M = k over this CI: minimal semifree resolution, isMinimal, and the L_3^M
-- identity (strict-action Lie module compatibility).
TEST ///
kk = ZZ/101
R = kk[a, b, c]
S = R/ideal(a^4, b^4, c^4)
A = koszulComplexDGA ideal S
assert(A.ring === R)
Mk = coker matrix{{a, b, c}}
LM = lInfinityModule(A, Mk, EndDegree => 3, GradeLimit => 1)
DGM = LM#"dgmodule"
assert isMinimalSemifreeResolution(DGM)
assert isLInfinityModule(LM, 3)
///

-- Third syzygy of the module presented by the two-row circulant matrix over S.
-- The ranks of its minimal semifree resolution in degrees zero through three
-- give the corresponding dimensions of Tor over S.
TEST ///
kk = ZZ/101
R = kk[a, b, c]
S = R/ideal(a^4, b^4, c^4)
A = koszulComplexDGA ideal S
phi = map(S, R)
NS = coker (phi matrix{{R_0, R_1, R_2}, {R_1, R_2, R_0}})
F = freeResolution(NS, LengthLimit => 4)
M3 = coker F.dd_4
MR = pushForward(map(S, R), M3)
LM = lInfinityModule(A, MR, EndDegree => 2, GradeLimit => 1)
DGM = LM#"dgmodule"
assert isValidDGModule(DGM)
-- The differential entries lie in the augmentation ideal.
assert isMinimalSemifreeResolution(DGM)
-- Per-degree ranks match Tor^S(M3, k_S) = {6, 9, 13, 18}:
ranks = apply(max(DGM.Degrees / first) + 1, d -> #select(DGM.Degrees, e -> first e == d))
assert(ranks == {6, 9, 13, 18})
-- With mu_1 = 0, the arity-three identity is the Lie module identity.
assert isLInfinityModule(LM, 3)
///

-- Fifth syzygy of the same module over the quartic complete intersection.
TEST ///
kk = ZZ/101
R = kk[a, b, c]
S = R/ideal(a^4, b^4, c^4)
A = koszulComplexDGA ideal S
phi = map(S, R)
NS = coker (phi matrix{{R_0, R_1, R_2}, {R_1, R_2, R_0}})
F = freeResolution(NS, LengthLimit => 6)
M5 = coker F.dd_6
MR5 = pushForward(map(S, R), M5)
LM = lInfinityModule(A, MR5, EndDegree => 2, GradeLimit => 1)
DGM = LM#"dgmodule"
assert isMinimalSemifreeResolution(DGM)
ranks = apply(max(DGM.Degrees / first) + 1, d -> #select(DGM.Degrees, e -> first e == d))
assert(ranks == {13, 18, 24, 31})
assert isLInfinityModule(LM, 3)
///

-- Seventh syzygy of the circulant module over k[a,b,c]/(a^4,b^4,c^4).
TEST ///
kk = ZZ/101
R = kk[a, b, c]
S = R/ideal(a^4, b^4, c^4)
A = koszulComplexDGA ideal S
phi = map(S, R)
NS = coker (phi matrix{{R_0, R_1, R_2}, {R_1, R_2, R_0}})
F = freeResolution(NS, LengthLimit => 8)
M7 = coker F.dd_8
MR7 = pushForward(map(S, R), M7)
LM = lInfinityModule(A, MR7, EndDegree => 2, GradeLimit => 1)
DGM = LM#"dgmodule"
assert isMinimalSemifreeResolution(DGM)
ranks = apply(max(DGM.Degrees / first) + 1, d -> #select(DGM.Degrees, e -> first e == d))
assert(ranks == {24, 31, 39, 48})
assert isLInfinityModule(LM, 3)
///

-- L_infinity-module structure on the A_2 cusp k[x,y]/(x^3 + y^2) —
-- an ADE singularity that's a 1-dimensional reduced ring (not a CI in
-- the sense of being acyclic-closed by just koszulComplexDGA, but the
-- residue field still admits a clean L-infinity-module structure).
TEST ///
kk = ZZ/101
P = kk[x, y]
Q = P/ideal(x^3 + y^2)
A = koszulComplexDGA ideal Q
LM = lInfinityModule(A, coker vars P, EndDegree => 4, GradeLimit => 1)
DGM = LM#"dgmodule"
assert isMinimalSemifreeResolution(DGM)
ranks = apply(max(DGM.Degrees / first) + 1, d -> #select(DGM.Degrees, e -> first e == d))
assert(ranks == {1, 2, 2, 2, 2, 2})
assert isLInfinityModule(LM, 3)
///

-- Avramov-Buchweitz "line support" example: R/(a+b+c) over the
-- 3-variable quadric CI k[a,b,c]/(a^2,b^2,c^2). The support variety
-- here is a plane (= hyperplane in P^2), and the L_infinity-module
-- structure should be well-defined.
TEST ///
kk = ZZ/101
P = kk[a, b, c]
Q = P/ideal(a^2, b^2, c^2)
A = koszulComplexDGA ideal Q
phi = map(Q, P)
MQ = coker matrix{{phi(P_0 + P_1 + P_2)}}
MR = pushForward(phi, MQ)
LM = lInfinityModule(A, MR, EndDegree => 3, GradeLimit => 1)
DGM = LM#"dgmodule"
assert isMinimalSemifreeResolution(DGM)
assert isLInfinityModule(LM, 3)
///

-- Golod ring k[a,b]/m^2 — acyclic closure needed since (a^2, ab, b^2)
-- is not a regular sequence. Verifies the L_infinity-module structure
-- extends correctly via T_(2,j) generators added by the closure.
TEST ///
kk = ZZ/101
P = kk[a, b]
I = ideal(a^2, a*b, b^2)
Q = P/I
K = koszulComplexDGA I
A = acyclicClosure(K, EndDegree => 3)
LM = lInfinityModule(A, coker vars P, EndDegree => 2, GradeLimit => 1)
DGM = LM#"dgmodule"
assert isMinimalSemifreeResolution(DGM)
ranks = apply(max(DGM.Degrees / first) + 1, d -> #select(DGM.Degrees, e -> first e == d))
assert(ranks == {1, 2, 4, 8})
assert isLInfinityModule(LM, 3)
///

-- Burch ring k[a,b]/(a^2, ab) — non-CI; the L_infinity-module structure
-- on k over this ring exists via the acyclic closure.
TEST ///
kk = ZZ/101
P = kk[a, b]
I = ideal(a^2, a*b)
Q = P/I
K = koszulComplexDGA I
A = acyclicClosure(K, EndDegree => 3)
LM = lInfinityModule(A, coker vars P, EndDegree => 2, GradeLimit => 1)
DGM = LM#"dgmodule"
assert isMinimalSemifreeResolution(DGM)
assert isLInfinityModule(LM, 3)
///

-- Matrix factorization module on a hypersurface: M = coker(x^2 - y)
-- over k[x,y]/(x^4 - y^2). Eisenbud's MF theory predicts period-2
-- minimal resolution, and the L_infinity-module structure should
-- respect this periodicity.
TEST ///
kk = ZZ/101
P = kk[x, y]
Q = P/ideal(P_0^4 - P_1^2)
A = koszulComplexDGA ideal Q
phi = map(Q, P)
MQ = coker matrix{{phi(P_0^2 - P_1)}}
MR = pushForward(phi, MQ)
LM = lInfinityModule(A, MR, EndDegree => 4, GradeLimit => 1)
DGM = LM#"dgmodule"
assert isMinimalSemifreeResolution(DGM)
ranks = apply(max(DGM.Degrees / first) + 1, d -> #select(DGM.Degrees, e -> first e == d))
assert(ranks == {1, 1, 1, 1, 1, 1})  -- period-2 / Knörrer periodicity
assert isLInfinityModule(LM, 3)
///

-- Divided-power check: in char where (n-1)! is invertible, the
-- DividedPower variant should agree with the classical Sym version.
-- Verifies that the two formulations are consistent on standard examples.
TEST ///
kk = ZZ/101
R = kk[a, b, c]
S = R/ideal(a^4, b^4, c^4)
A = koszulComplexDGA ideal S

-- isLInfinity Sym vs DP agreement
L = lInfinity A
assert(isLInfinity(L, 3) == isLInfinity(L, 3, DividedPower => true))

-- bracketGamma vs bracket agreement on distinct args; scaling on repeats
T = gens A.natural
assert(bracket(L, {T#0, T#1}) == bracketGamma(L, {T#0, T#1}))
-- Repeated: bracket = multFactor * bracketGamma (= 2! · bracketGamma here)
assert(bracket(L, {T#0, T#0}) == 2 * bracketGamma(L, {T#0, T#0}))

-- isLInfinityModule Sym vs DP agreement on HMF Omega^3
phi = map(S, R)
NS = coker (phi matrix{{R_0, R_1, R_2}, {R_1, R_2, R_0}})
F = freeResolution(NS, LengthLimit => 4)
M3 = coker F.dd_4
MR = pushForward(map(S, R), M3)
LM = lInfinityModule(A, MR, EndDegree => 2, GradeLimit => 1)
assert(isLInfinityModule(LM, 3) == isLInfinityModule(LM, 3, DividedPower => true))
///

-- Graded coordinate maps must preserve exterior signs when degree sorting
-- reverses the input order of two odd generators.
TEST ///
S = QQ[z]
A = freeDGAlgebra(S,{{3,0},{1,0},{5,0}},Variable=>{u,v,w})
aa = (gens A.natural)#0
bb = (gens A.natural)#1
cc = (gens A.natural)#2
setDiff(A,{0,0,aa*bb},InitializeDegreeZeroHomology=>false,InitializeComplex=>false)
L = lInfinity A
assert(bracket(L,{aa,bb}) == -cc)
assert(bracket(A,{aa,bb}) == -cc)
assert(bracket(L,{bb,aa}) == cc)
assert(bracket(L,{aa,aa}) == 0)
assert(bracket(L,{2*aa,3*bb}) == -6*cc)
assert(bracket(A,{aa,bb},cc) == -1)
///

-- Nonzero mixed-parity Jacobi terms cancel for a valid differential.
TEST ///
S = QQ[z]
A = freeDGAlgebra(S,{{2,0},{1,0},{1,0},{4,0},{3,0},{6,0}},Variable=>{a,b,c,u,v,t})
gg = gens A.natural
aa = gg#0; bb = gg#1; cc = gg#2; uu = gg#3; vv = gg#4; tt = gg#5;
setDiff(A,{0,0,0,aa*bb,bb*cc,uu*cc-aa*vv},InitializeDegreeZeroHomology=>false,InitializeComplex=>false)
assert isWellDefined A
L = lInfinity(A,GradeLimit=>3)
assert(bracket(L,{bracket(L,{aa,bb}),cc}) == tt)
assert(bracket(L,{aa,bracket(L,{bb,cc})}) == tt)
assert isLInfinity(L,3)
///

-- A genuine ell_2/ell_3 compatibility identity, with nonzero compositions.
TEST ///
S = QQ[z]
A = freeDGAlgebra(S,{{1,0},{0,0},{0,0},{2,0},{1,0},{3,0}},Variable=>{a,b,c,u,v,t})
gg = gens A.natural
aa = gg#0; bb = gg#1; cc = gg#2; uu = gg#3; vv = gg#4; tt = gg#5;
setDiff(A,{0,0,0,aa*bb,bb*cc^2,uu*cc^2+aa*vv},InitializeDegreeZeroHomology=>false,InitializeComplex=>false)
assert isWellDefined A
L = lInfinity(A,GradeLimit=>2)
assert(bracket(L,{bracket(L,{aa,bb}),cc,cc}) == -2*tt)
assert isLInfinity(L,4)
///

-- Minimality, argument linearity, and cache refresh after changing d.
TEST ///
S = QQ[x,y]
A = freeDGAlgebra(S,{{1,2}})
tt = first gens A.natural
setDiff(A,{x*y},InitializeDegreeZeroHomology=>false,InitializeComplex=>false)
L = lInfinity A
assert(bracket(L,{x_S,y_S}) == tt)
oldMat = bracketMatrix(A,1,1)
setDiff(A,{2*x*y},InitializeDegreeZeroHomology=>false,InitializeComplex=>false)
assert(bracket(L,{x_S,y_S}) == 2*tt)
assert(bracket(A,{x_S,y_S}) == 2*tt)
assert(bracketMatrix(A,1,1) == 2*oldMat)
assert(try (bracket(L,{x_S^2,y_S});false) else true)
setDiff(A,{x_S},InitializeDegreeZeroHomology=>false,InitializeComplex=>false)
assert(try (lInfinity A;false) else true)
assert(try (bracket(L,{x_S,y_S});false) else true)
-- A square-zero differential can still have the wrong homological degree.
A = freeDGAlgebra(S,{{2,0},{1,0}});
(u,t) = toSequence gens A.natural;
setDiff(A,{0,x_S^2},InitializeDegreeZeroHomology=>false,InitializeComplex=>false);
L = lInfinity A;
setDiff(A,{0,u^2},InitializeDegreeZeroHomology=>false,InitializeComplex=>false);
assert(diff(A,diff(A,t)) == 0);
assert(try (isLInfinity(L,3,DividedPower=>true);false) else true);
///

TEST ///
-- Complete intersection: odd Lie generators have nonzero self-brackets.
S=QQ[x,y]; A=koszulComplexDGA ideal(x^2,y^2);
G=homotopyLieAlgebra A;
bb=gens G; V=G#"space";
assert(dim G==4);
assert(degrees G=={{1,-1},{1,-1},{2,-2},{2,-2}});
assert(bracket(G,{bb#0,bb#0})==2*V_2);
assert(bracket(G,{V_0,V_0})==2*V_2);
assert(bracket(G,{V_0+V_1,V_0+V_1})==2*(V_2+V_3));
assert(lieCoordinates(G,lieRepresentative(G,V_0+3*V_2))==V_0+3*V_2);
assert(numrows bracketMatrix(G,1,1)==2);
assert(numcols bracketMatrix(G,1,1)==4);
assert(numcols basis(5,G)==0);
assert(numrows bracketMatrix(G,5,1)==0);
assert(#keys structureConstants G==2);
assert isLieSuperalgebra G;
H=holonomyRepresentation(A,coker vars S,EndDegree=>2);
assert isHolonomyRepresentation H;
assert(dim H==10);
assert(degrees H==degrees holonomyRepresentation(lieAlgebra H,coker vars S,EndDegree=>2));
xx=first gens lieAlgebra H;
rho=actionMatrix(H,xx);
assert(rho*rho!=0);
assert(2*rho*rho==actionMatrix(H,bracket(lieAlgebra H,{xx,xx})));
assert(actionMatrix(H,0)==rho);
assert(holonomyAction(H,xx,(H#"space")_0)==rho*(H#"space")_0);
assert(try (holonomyRepresentation(H#"lInfinityModule",GradeLimit=>1);false) else true);
assert(try (actionMatrix(H,-1);false) else true);
assert(try (lieCoordinates(G,1_S);false) else true);
assert(try (lieCoordinates(G,bb#0^2);false) else true);
assert(try (homotopyLieAlgebra(A,GradeLimit=>-1);false) else true);
-- Cached data must not survive a changed source differential silently.
setDiff(A,{2*x_S^2,y_S^2},InitializeDegreeZeroHomology=>false,InitializeComplex=>false);
assert(try (structureConstants G;false) else true);
assert(try (actionMatrix(H,0);false) else true);
-- Characteristic two is explicit until the odd squaring operation is provided.
S2=(ZZ/2)[a];A2=koszulComplexDGA ideal(a^2);
assert(try (homotopyLieAlgebra A2;false) else true);
-- Every internal component of the grading is retained.
S3=QQ[u,v,Degrees=>{{1,0},{0,1}}];
A3=koszulComplexDGA ideal(u^2,v^2);
G3=homotopyLieAlgebra A3;
assert(degrees G3=={{1,-1,0},{1,0,-1},{2,-2,0},{2,0,-2}});
-- An omitted generator must act trivially for the representation to descend.
S4=QQ[x];A4=freeDGAlgebra(S4,{{1,0}});setDiff(A4,{0});
LM4=lInfinityModule(A4,coker vars S4,EndDegree=>2);
assert(try (holonomyRepresentation(LM4,GradeLimit=>1);false) else true);
G4=homotopyLieAlgebra A4;
setDiff(A4,{x_S4^2},InitializeDegreeZeroHomology=>false,InitializeComplex=>false);
assert(try (holonomyRepresentation(G4,coker vars S4);false) else true);
assert(try (lieRepresentative(G4,0_(G4#"space"));false) else true);
assert(try (lieCoordinates(G4,0_(G4#"space"));false) else true);
assert(try (bracketMatrix(G4,99,99);false) else true);
-- Internally inhomogeneous models expose only their valid cohomological grading.
G5=homotopyLieAlgebra A4;
assert(degrees G5=={{1},{2}});
assert(G5#"hasInternalGrading"==false);
///

-- Public higher-operation inputs and dual gradings.
TEST ///
S=QQ[x];
A=koszulComplexDGA ideal(x^3);
L=lInfinity A;
LM=lInfinityModule(A,coker vars S,EndDegree=>1,GradeLimit=>2);
b=gens L;
e=gens LM;
-- The model has only two Lie generators, even when the grade bound is large.
LMwide=lInfinityModule(A,coker vars S,EndDegree=>1,GradeLimit=>30);
assert isLInfinity(lInfinity(A,GradeLimit=>30),3);
assert isLInfinityModule(LMwide,3);
assert(degrees L == {{1,-1},{2,-3}});
assert(degrees LM == {{0,0},{1,-1},{2,-3}});
assert(bracket(L,{b#0,b#0,b#0}) == 6*b#1);
assert(mu(LM,{b#0},e#0) == e#1);
assert(mu(LM,{b#0,b#0},e#1) == 2*e#2);
assert(mu(LM,{b#0,b#0},1) == 2*e#2);
assert(bracket(lInfinity LM,{b#0,b#0,b#0}) == 6*b#1);
assert((lInfinity(LM,GradeLimit=>1))#"GradeLimit" == 1);
assert((lInfinity LM)#"GradeLimit" == 3);
assert(LM#"lInfinity"#"GradeLimit" == 2);
assert(match("2 Lie generators",toString net L));
assert(match("3 module generators",toString net LM));
print gens L;
print degrees L;
print gens LM;
print degrees LM;
S=QQ[x,y];
A=koszulComplexDGA ideal(x^3+y^2);
L=lInfinity A;
LM=lInfinityModule(A,coker vars S,EndDegree=>1);
assert all(degrees L,d -> #d == 1);
assert all(degrees LM,d -> #d == 1);
S=(ZZ/2)[x];
A=koszulComplexDGA ideal(x^3);
L=lInfinity A;
LM=lInfinityModule(A,coker vars S,EndDegree=>1);
assert(#gens L == 2);
assert(#gens LM == 3);
print "PASS operation accessors";
///

-- A shared differential monomial contributes to every target, and caches refresh.
TEST ///
S=QQ[x];
A=freeDGAlgebra(S,apply(3,i->{1,0}));
t=gens A.natural;
setDiff(A,apply(3,i->(i+1)*x_S^2+(i+2)*x_S^3),InitializeDegreeZeroHomology=>false,InitializeComplex=>false);
L=lInfinity A;
a=first allgens(A,0);
assert(bracket(L,{a,a})==sum(3,i->2*(i+1)*t#i));
assert(bracket(L,{a,a,a})==sum(3,i->6*(i+2)*t#i));
assert(bracketGamma(L,{a,a,a})==sum(3,i->(i+2)*t#i));
setDiff(A,apply(3,i->(i+2)*x_S^2),InitializeDegreeZeroHomology=>false,InitializeComplex=>false);
assert(bracket(L,{a,a})==sum(3,i->2*(i+2)*t#i));
assert(bracket(L,{a,a,a})==0);
S=QQ[x]; A=freeDGAlgebra(S,{{1,0}}); setDiff(A,{0});
a=first allgens(A,0);
F=freeDGModule(A,{{0,0}}|apply(3,i->{1,0}));
e0=F.natural_0;
setDiff(F,{0}|apply(3,i->((i+1)*a+(i+2)*a^2)*e0));
LM=lInfinityModule F;
assert(mu(LM,{a},0)==sum(3,i->(i+1)*F.natural_(i+1)));
assert(mu(LM,{a,a},0)==sum(3,i->2*(i+2)*F.natural_(i+1)));
assert(muGamma(LM,{a,a},0)==sum(3,i->(i+2)*F.natural_(i+1)));
setDiff(F,{0}|apply(3,i->(i+2)*a*e0));
assert(mu(LM,{a},0)==sum(3,i->(i+2)*F.natural_(i+1)));
assert(mu(LM,{a,a},0)==0_(F.natural));
-- A nonzero square can lie above every degree examined by a finite strand test.
A=freeDGAlgebra(S,{{2,0}}); setDiff(A,{0});
t=first gens A.natural; a=first allgens(A,0);
F=freeDGModule(A,{{0,0},{3,0},{6,0}});
(e0,e1,e2)=toSequence apply(3,i->F.natural_i);
LM=lInfinityModule F;
H=holonomyRepresentation(LM,GradeLimit=>0);
setDiff(F,{0,t*e0,t*e1});
assert(diff(F,F.diff#2)==t^2*e0);
assert(try(mu(LM,{t},0); false) else true);
assert(try(isHolonomyRepresentation H; false) else true);
assert(not isLInfinityModule(LM,3,DividedPower=>true));
assert(isLInfinityModule(LM,2,DividedPower=>true));
-- Every term in a column must have the indicated homological degree.
setDiff(F,{0,(a+t)*e0,0});
assert(try(mu(LM,{t},0); false) else true);
assert(try(isLInfinityModule(LM,3,DividedPower=>true); false) else true);
setDiff(F,{0,t*e0,0});
assert(mu(LM,{t},0)==e1);
///

-- Coordinate indices follow the public basis, including unsorted and empty models.
TEST ///
S=QQ[x]; A=freeDGAlgebra(S,{{2,0},{1,0},{3,0}}); setDiff(A,{0,0,0});
G=homotopyLieAlgebra A; V=G#"space"; b=gens G;
for i from 0 to dim G-1 do assert(lieCoordinates(G,b#i)==V_i);
v=V_0-2*V_1+3*V_2-4*V_3;
assert(lieCoordinates(G,lieRepresentative(G,v))==v);
assert(lieCoordinates(G,0_S)==0_V);
assert(lieRepresentative(G,0_V)==0_(A.natural));
assert(try(lieCoordinates(G,x_S^2);false)else true);
assert(try(lieCoordinates(G,1_S);false)else true);
G0=homotopyLieAlgebra(A,GradeLimit=>0);
assert(lieCoordinates(G0,0_S)==0_(G0#"space"));
assert(lieRepresentative(G0,0_(G0#"space"))==0_(A.natural));
assert(#structureConstants G0==0);
-- A tower k[a][b] has the same degree-one Lie basis as k[a,b].
B=QQ[symbol xa]; S=B[symbol xb,Join=>false]; A=koszulComplexDGA ideal(promote(B_0,S)*S_0);
(u,v,t)=toSequence apply({B_0,S_0,A.natural_0},g->promote(g,A.natural));
-- A computation on A should preserve symbols in another active ring.
C=QQ[symbol xa,symbol xb]; G=homotopyLieAlgebra A; W=coordinateSpace G;
assert(xa===C_0 and xb===C_1);
assert(allgens A=={u,v,t} and degrees G=={{1,-1},{1,-1},{2,-2}});
-- Both levels of the tower contribute to the absolute degree-one Lie basis.
varsOverQQ = gens(S, CoefficientRing => QQ);
B1 = basis(1,G);
assert(set apply(varsOverQQ, g -> lieCoordinates(G,g)) == set apply(numcols B1, i -> B1_i));
assert(lieCoordinates(G,B_0)==W_0 and lieCoordinates(G,S_0)==W_1);
assert(bracket(G,{B_0,S_0})==W_2);
assert(bracketMatrix(G,1,1)==matrix{{0_QQ,1,1,0}});
-- A regular ring has an abelian Lie algebra concentrated in degree one.
S=QQ[symbol z,Degrees=>{2}]; A=freeDGAlgebra(S,{}); setDiff(A,{});
G=homotopyLieAlgebra A; assert(degrees G=={{1,-2}} and bracketMatrix(G,1,1)==0);
assert(isLieSuperalgebra(G) and isLInfinity(lInfinity A,3));
///

-- The fourth bracket of the quartic hypersurface and the fourth action on its residue field.
TEST ///
S = QQ[x];
A = koszulComplexDGA ideal(x^4);
L = lInfinity(A, OrderLimit => 2);
LM = lInfinityModule(A, coker vars S, OrderLimit => 2, EndDegree => 1);
(a,t) = toSequence gens L;
e = gens LM;
assert(L#"OrderLimit" == 2 and LM#"lInfinity"#"OrderLimit" == 2);
-- Direct evaluation still sees the quartic term when the stored order is two.
assert(bracket(L,{a,a,a}) == 0);
assert(bracket(L,{a,a,a,a}) == 24*t and bracketGamma(L,{a,a,a,a}) == t);
assert(mu(LM,{a,a},e#1) == 0*e#2);
assert(mu(LM,{a,a,a},e#1) == 6*e#2 and muGamma(LM,{a,a,a},e#1) == e#2);
assert(isLInfinity(L,4) and isLInfinityModule(LM,4));
assert(try (lInfinity(A,OrderLimit=>3/2); false) else true);
assert(try (lInfinityModule(A,coker vars S,OrderLimit=>1); false) else true);
///

-- Over k[x,y]/(x^3), the exterior product and the hypersurface operator
-- span the two independent degree-two classes of the residue field.
TEST ///
k = GF 9;
S = k[x,y];
A = koszulComplexDGA ideal(x^3);
G = homotopyLieAlgebra A;
H = holonomyRepresentation(G, coker vars S, EndDegree => 1);
assert(instance(G,HomotopyLie) and instance(H,HolonomyRepresentation));
assert(ring G === k and ring H === k);
assert(numcols basis(1,G) == 2 and numcols basis(2,G) == 1);
assert(apply({0,1,2}, d -> numcols basis(d,H)) == {1,2,2});
(X,Y,T) = toSequence apply(gens G, u -> actionMatrix(H,u));
assert(X^2 == 0 and Y^2 == 0 and X*Y + Y*X == 0);
B0 = basis(0,H);
assert(image(X*Y*B0 | T*B0) == image basis(2,H));
assert(match("3 generators",toString net G));
assert(match("5 module generators, 3 Lie generators",toString net H));
///

-- In characteristic two, ordinary polarization loses the repeated inputs
-- of the quartic relation and the cubic coefficient in the module differential.
TEST ///
oneEntry = (T,key,value) -> keys T == {key} and T#key == value;
scan({QQ,ZZ/2}, k -> (
    S := k[x];
    A := koszulComplexDGA ideal(x^4);
    L := lInfinity A;
    LM := lInfinityModule(A,coker vars S,EndDegree=>1);
    (a,t) := toSequence gens L;
    e := gens LM;
    akey := {0,0,0,0};
    mkey := ({0,0,0},1);
    assert(oneEntry(structureConstants(L,4,DividedPower=>true),akey,t));
    assert(oneEntry(structureConstants(LM,4,DividedPower=>true),mkey,e#2));
    if char k == 0 then (
        G := homotopyLieAlgebra A;
        assert(try (structureConstants(G,DividedPower=>true); false) else true);
        assert(oneEntry(structureConstants(L,4),akey,24*t));
        assert(oneEntry(structureConstants(LM,4),mkey,6*e#2));
        ) else (
        assert(#structureConstants(L,4) == 0);
        assert(#structureConstants(LM,4) == 0);
        );
    assert(#structureConstants(L,1000000) == 0 and #structureConstants(LM,1000000) == 0);
    assert(try (structureConstants(L,1); false) else true);
    assert(try (structureConstants(LM,0); false) else true);
    ));
///

-- Quadratic and cubic terms of the same differential must all be reported.
TEST ///
S = QQ[x,y];
A = koszulComplexDGA ideal(x^2+x*y+y^3);
L = lInfinity A;
(a,b,t) = toSequence gens L;
C2 = structureConstants(L,2);
C3 = structureConstants(L,3);
assert(#C2 == 2 and C2#{0,0} == 2*t and C2#{0,1} == t);
assert(#C3 == 1 and C3#{1,1,1} == 6*t);
LM = lInfinityModule(A,coker vars S,EndDegree=>1);
U2 = structureConstants(LM,2);
U3 = structureConstants(LM,3);
assert(U2#({0},0) == mu(LM,{a},0) and U2#({1},0) == mu(LM,{b},0));
assert(#U3 > 0);
scan(#gens LM, j -> (
    v := mu(LM,{b,b},j);
    if v == 0 then assert(not U3#?({1,1},j)) else assert(U3#({1,1},j) == v)));
///

-- Recover operations in a supplied semifree basis, including non-acyclic modules.
TEST ///
S = QQ[x];
A = koszulComplexDGA ideal(x^4);
F = freeDGModule(A,{{0,0},{1,2},{1,2}});
u = promote(x,A.natural);
(f0,f1,f2) = toSequence apply(3,i -> F.natural_i);
setDiff(F,{0,u^2*f0,-3*u^2*f0});
LM = lInfinityModule(F,GradeLimit=>2,OrderLimit=>4);
L = lieAlgebra LM;
assert(gens LM == {f0,f1,f2});
assert(L === lieAlgebra LM and L#"GradeLimit" == 2 and L#"OrderLimit" == 4);
(a,t) = toSequence gens L;
assert(bracket(L,{a,a,a,a}) == 24*t);
assert(mu(LM,{a},f0) == 0 and mu(LM,{a,a},f0) == 2*f1-6*f2);
assert(muGamma(LM,{a,a},0) == f1-3*f2);
assert(isHolonomyRepresentation holonomyRepresentation LM);
D = lInfinityModule(A,coker vars S,EndDegree=>1,OrderLimit=>2);
assert((lieAlgebra D)#"OrderLimit" == 2);
assert((lInfinity D)#"OrderLimit" == 3);
-- Constant differential entries violate augmentation minimality.
setDiff(F,{0,f0,0});
assert(try (lInfinityModule F; false) else true);
assert(try (lieAlgebra LM; false) else true);
-- The two nonconstant entries compose to x^2 on the last generator.
E = freeDGModule(A,{{0,0},{1,1},{2,2}});
setDiff(E,{0,u*E.natural_0,u*E.natural_1});
assert(try (lInfinityModule E; false) else true);
///

-- Degree blocks use source degree first and the order of the public graded bases.
TEST ///
S = QQ[x,y];
A = acyclicClosure(koszulComplexDGA ideal(x^2,x*y,y^2),EndDegree=>2);
H = holonomyRepresentation(A,coker matrix{{x_S,y_S^2}},EndDegree=>2);
G = lieAlgebra H;
U = coordinateSpace G;
assert(actionMatrix(H,0,2,3) == matrix(QQ,{{0,0},{1,0},{0,0},{0,1}}));
assert(actionMatrix(H,y_S,2,3) == matrix(QQ,{{1,0},{0,0},{0,1},{0,0}}));
u = U_0 + 2*U_5;
scan({0,1,2,3}, p -> scan({0,1,2,3}, q -> (
    cols := positions(degrees H,d -> first d == p);
    rows := positions(degrees H,d -> first d == q);
    assert(actionMatrix(H,u,p,q) == ((actionMatrix(H,u))^rows)_cols))));
assert(actionMatrix(H,0_S,2,3) == map(QQ^4,QQ^2,0));
assert(actionMatrix(H,0,-1,3) == map(QQ^4,QQ^0,0));
assert(actionMatrix(H,0,2,9) == map(QQ^0,QQ^2,0));
assert(actionMatrix(H,0,-1,9) == map(QQ^0,QQ^0,0));
assert(try (actionMatrix(H,-1,-1,9); false) else true);
///

end--


uninstallPackage "HomotopyLieAlgebra"
restart
installPackage "HomotopyLieAlgebra"
check HomotopyLieAlgebra

viewHelp HomotopyLieAlgebra

