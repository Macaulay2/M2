newPackage(
    "LinearTruncations",
    Version => "1.0",
    Date => "May 5, 2021",
    Authors => {
	{Name => "Lauren Cranton Heller", Email => "lch@math.berkeley.edu"},
	{Name => "David Eisenbud", Email => "de@msri.org"},
	{Name => "Navid Nemati", Email => "Navid.Nemati@inria.fr"}},
    Headline => "find the multigraded truncations that give linear resolutions",
    PackageExports => {"Truncations", "TateOnProducts"},
    DebuggingMode => false
    )

export{
    "multigradedPolynomialRing",
    "irrelevantIdeal",
    "diagonalMultidegrees",
    "findMins",
    "findRegion",
    "partialRegularities",
    "isLinearComplex",
    "linearTruncations",
    "supportOfTor",
    "linearTruncationsBound",
    "regularityBound",
    "isQuasiLinear",
    "compMin",
    "compMax",
    --
    "IrrelevantIdeal",
    "Inner",
    "Outer"
    }

-------------------------
--coordinate rings for products of projective spaces

multigradedPolynomialRing = method(Options => {
	CoefficientField => ZZ/32003,
	Standard => true,
	Variables => "x"
	})
multigradedPolynomialRing List := opts -> ex -> (
    kk := opts.CoefficientField;
    len := #ex;
    xx := if opts.Standard then (
	x := opts.Variables;
	if instance(x,String) then x = getSymbol x;
	flatten apply(len, i -> apply(ex_i+1, j -> x_(i,j)))
	) else (
	vars(0..(sum ex)+#ex-1)
	);
    degs := flatten apply(len, i -> apply(ex_i+1, k -> apply(len,
		j -> if i==j then 1 else 0
		)));
    kk[xx, Degrees => degs]
    )
multigradedPolynomialRing ZZ := opts -> n -> (
    multigradedPolynomialRing(toList(n:1), opts)
    )

gradedPolynomialRing = method(Options => {
	CoefficientField => ZZ/32003,
	Variables => "y"
	})
gradedPolynomialRing List := opts -> n -> (
    kk := opts.CoefficientField;
    y := opts.Variables;
    if instance(y,String) then y = getSymbol y;
    yy := flatten apply(#n, i -> apply(n_i+1, j -> y_(i,j)));
    kk[yy]
    )

irrelevantIdeal = method()
--only works for products of projective spaces
irrelevantIdeal Ring := S -> (
    degs := unique degrees S;
    ideals := apply(degs, ell -> (
	    ideal select(gens S, s -> degree s == ell)
	    ));
    intersect ideals
    )

exponent = method()
--redundant with unexported function dimVector in VirtualResolutions
exponent Ring := S -> (
    alldegs := degrees S;
    degs := unique alldegs;
    apply(degs, ell -> -1 + #positions(alldegs, em -> em == ell))
    )

-------------------------
--functions for linearTruncations

diagonalMultidegrees = method()
--redundant with function compositions in core
diagonalMultidegrees(ZZ,ZZ) := (d,t) -> (
    --exponent list of the d-th power of the max ideal in t vars
    if d<0 then return {};
    if t==1 then {{d}} else if d==0 then {toList(t:0)} else
    flatten apply(d+1, i ->
	apply(diagonalMultidegrees(d-i, t-1), ell -> flatten prepend(i,ell))
	)
    )
diagonalMultidegrees(ZZ,List) := (d,n) -> (
    --a list of #n -tuples adding up to d, where the i-th element is <= 1+n_i
    L1 := diagonalMultidegrees(d,#n);
    select(L1, ell -> all(#n, i -> ell_i <= (1+n_i)))
    )

-*
diagonalMultidegrees' = method()
--we use this trick...based on findHashTableCorner from TateOnProducts
diagonalMultidegrees'(ZZ,ZZ) := (deg,len) -> (
    P := multigradedPolynomialRing toList (len:0);
    Q := gradedPolynomialRing toList (len:0);
    phi := map(P, Q, gens P);
    degree \ flatten entries phi basis(deg, Q)
    )
*-

findMins = method()
findMins Ideal := I -> (
    apply(flatten entries mingens I, m -> flatten exponents m)
    )
findMins List := L -> (
    if L=={} then return {};
    if any(L, ell -> not instance(ell, List)) then error "expected list of lists";
    low := compMin L;
    t := #(L_0);
    if any(L, ell -> #ell != t) then error "expected entries of list to have same length";
    x := local x;
    T := ZZ/2[x_0..x_(t-1)];
    (findMins ideal apply(L, ell -> T_(ell-low)))/(g -> g+low)
    )

-*
--using version in TateOnProducts instead
coarseMultigradedRegularity' = method()
coarseMultigradedRegularity' Module := M ->
    coarseMultigradedRegularity' res prune M
coarseMultigradedRegularity' ChainComplex := F -> (
    t := degreeLength ring F;
    if t<=2 then
    partialRegularities F
    else(
	range := toList(min F..max F-1);
	degsF := flatten(apply(range,i -> degrees (F_i)));
	apply(t, i-> max apply(degsF, ell->ell_i))
	)
    )
*-

partialRegularities = method()
partialRegularities Module := M ->
    partialRegularities res prune M
partialRegularities ChainComplex := F-> (
    t := degreeLength ring F;
    range := toList(min F..max F-1);
    apply(t, i -> max flatten apply(range, j -> (
		apply(degrees(F_j), ell -> ell_i - j)
		)))
    )

-*
--version that allows generation in multiple multidegrees
isLinearComplex = method()
isLinearComplex ChainComplex := F -> (
    if F == 0 then return true;
    t := degreeLength ring F;
    range := toList(min F..max F-1);
    dF := apply(range, i -> (degrees(F_i))/sum);
    mindF := min(dF_(min F));
    all(range, i -> max(dF_i) === mindF+i-min F)
    )
*-

isLinearComplex = method()
--requires generators in single degree
isLinearComplex ChainComplex := F -> (
    if F == 0 then return true;
    t := degreeLength ring F;
    range := toList(min F..max F-1);
    if #(unique degrees F_(range_0)) != 1 then return false;
    dF := apply(range, i -> (degrees(F_i))/sum);
    mindF := dF_(min F)_0;
    all(range, i -> max(dF_i) === mindF+i-range_0)
    )

-*
--computation moved to findRegion
linearTruncations = method(Options =>{Verbose =>false})
linearTruncations Module := o -> M -> (
    t := degreeLength M;
    F := res prune M;
    r := coarseMultigradedRegularity F;
    d := regularity F;
    L0 := diagonalMultidegrees(d,t);
    candidates := set {};
    scan(L0, ell ->candidates = candidates+set toList(ell..r));
    candidates = toList candidates;
    if o.Verbose == true then <<"candidates are"<< candidates<<endl;
    L := select(candidates, ell ->
	isLinearComplex res truncate(ell,M)
	);
    if o.Verbose == true then << "the candidates with linear truncations are" << L<<endl;
    findMins L
    )
linearTruncations(Module,List) := o -> (M, candidates) -> (
    L := select(candidates, c->(
	    isLinearComplex res prune truncate(c, M)
	    ));
    findMins L
    )
*-

findRegion = method(Options => true)
--only option symbols used by findRegion are Outer and Inner
findRegion(List,Ideal, Function) :=
findRegion(List,Module,Function) := true >> opts -> (range,M,f0) -> (
    t := degreeLength ring M;
    if #range != 2 or any(range, ell -> #ell != t)
    then error "expected range to be a list of 2 multidegrees of M";
    if not all(range_1-range_0, i -> i>=0)
    then error "expected start of range to be below end";
    (low, high) := toSequence range;
    x := local x;
    T := ZZ/2[x_0..x_(t-1)];
    f := if options f0 === null then f0 else (
	fopts := select(keys options f0, k -> opts#?k);
	fopts  = fopts / (k -> k => opts#k);
	(ell, M) -> f0(ell, M, new OptionTable from fopts)
	);
    try f(low,M) else error "expected function to take multidegree, module, and given options";
    L0 := if opts.?Inner then opts.Inner else {};
    I  := ideal({0_T} | apply(L0, ell -> T_(ell - low)));
    --assume I0 to be in I without checking
    K0 := if opts.?Outer then opts.Outer else {low};
    J := ideal apply(K0, ell -> T_(ell - low));
    if isSubset(J,I) then (
	if I==J then return (findMins I)/(ell -> ell + low) else
	error "expected Outer to contain Inner"
	);
    --don't check outside of region generated by K0
    ht := new MutableHashTable from apply(K0,
	ell -> T_(ell - low) => ell);
    --this is a breath first search borrowed from the FGLM package
    while #ht > 0 do (
	(elt, ell) := min pairs ht;
	remove(ht, elt);
	if elt % I == 0 then continue;
	if f(ell, M) then I = I + ideal elt else (
	    scan(T_*, g -> (
		    leaf := first exponents(g * elt) + low;
		    if all(high - leaf, i -> i >= 0)
		    and not ht#?(g * elt)
		    then ht#(g * elt) = leaf
		    ))
	    )
	);
    (findMins I)/(ell -> ell + low)
    )

linearTruncations = method()
linearTruncations(List,Module) := (range,M) -> (
    findRegion(range,M,isLinearTruncation)
    )
linearTruncations Module := M -> (
    t := degreeLength M;
    r := regularity M;
    range := {compMin degrees prune M, toList(t:r+1)};
    linearTruncations(range,M)
    )

-------------------------
--boolean operators for different regions

isLinearTruncation = method()
isLinearTruncation(List,Module) := (d,M) -> (
    N := truncate(d,M);
    if N==0 then return true;
    degree N_0 == d and isLinearComplex res prune N
    )

isQuasiLinear = method(Options => {IrrelevantIdeal => null})
--see Theorem 2.9 of Berkesch-Erman-Smith
isQuasiLinear(List,Module) := opts -> (d,M) -> (
    if debugLevel > 1 then print(" -- truncating at " | toString d);
    irr := if opts.IrrelevantIdeal =!= null
    then opts.IrrelevantIdeal else irrelevantIdeal ring M;
    N := truncate(d,M);
    degs := supportOfTor N;
    allowed := supportOfTor comodule irr;
    if (len := #degs) > #allowed then return false;
    all(reverse (0 .. len-1),
	i -> all(degs_i,
	    ell -> any(allowed_i,
		em -> all(em + d - ell, j -> j >= 0))))
    )
isQuasiLinear BettiTally := opts -> T -> (
    if (irr := opts.IrrelevantIdeal) === null then error "expected IrrelevantIdeal option";
    allowed := supportOfTor comodule irr;
    if (len := pdim T) > #allowed-1 then return false;
    --supportOfTor will have length 1 more than the pd of irr
    degs := keys T;
    d := if degs_0_0 == 0 then degs_0_1 else (
	degs = sort keys T;
	degs_0_1
	);
    all(reverse degs, ell -> (
	    hdeg := ell_0;
	    if hdeg < 0 then return false;
	    any(allowed_hdeg, em -> all(em + d - ell_1, j -> j >=0))
	    ))
    )
isQuasiLinear ChainComplex := opts -> F -> isQuasiLinear(betti F, opts)

-------------------------
--bounds on linear truncations region

supportOfTor = method()
supportOfTor ChainComplex := F -> (
    for i from min F to max F list (
	degs := unique degrees F_i;
	if degs == {} then continue else degs
	)
    )
supportOfTor Module := M -> supportOfTor res prune M

compMin = method()
compMin List := L -> (
    if L == {} then return {};
    n := #(L_0);
    if any(L, ell -> #ell != n) then error "expected entries to have same length";
    apply(n, i -> min apply(L, ell -> ell_i))
    )
compMin(List,List) := (L,M) -> compMin {L,M}

compMax = method()
compMax List := L -> (
    if L == {} then return {};
    n := #(L_0);
    if any(L, ell -> #ell != n) then error "expected entries to have same length";
    apply(n, i -> max apply(L, ell -> ell_i))
    )
compMax(List,List) := (L,M) -> compMax {L,M}

protect Nonlinear
MM = method(Options => {Nonlinear => false})
MM(ZZ, List, List) := opts -> (i, range, D) -> (
    n := #D;
    if opts.Nonlinear and i != 0
    then MM(i-1, range, D-toList(n:1)) else (
	low := range_0;
	high := range_1;
	L := toList(drop(low,1)..drop(high,1));
	flatten apply(L, A -> (
		dif := drop(D,1)-A;
		pos := sum select(dif, j -> j>0);
		if pos<=i then (
		    limit := D_0-i-1+pos;
		    if low_0<=limit then (
			for j from low_0 to limit list prepend(j,A)
			) else {}
		    ) else for j from low_0 to high_0 list prepend(j,A)
		))
	)
    )

bound = method(Options => {Nonlinear => false})
bound Module := opts -> M -> (
    t := degreeLength ring M;
    if M == 0 then return (t:-infinity);
    st := supportOfTor M;
    high := compMax flatten st;
    low := compMin degrees prune M;
    lower := low-toList(t:#supportOfTor M);
    st = flatten apply(#st, i -> apply(st_i, ell -> {i,ell}));
    gt := new MutableHashTable from flatten apply(st, ell -> (
	    xs := MM(ell_0,{lower,high},ell_1,Nonlinear => opts.Nonlinear);
	    apply(xs, em -> em => false)
	    ));
    L := toList(low..high);
    findMins select(L, ell -> not gt#?ell)
    )

linearTruncationsBound = method()
linearTruncationsBound Module := M -> bound M

regularityBound = method()
regularityBound Module := M -> bound(M, Nonlinear => true)

-------------------------
--functions neither used nor exported

graphTor = method()
--supportOfTor plus which maps are nonzero
graphTor Module := M -> (
    F := res prune M;
    degs := apply(1+length F, i -> degrees F_i);
    maps := apply(length F, i -> (
	    apply(entries F.dd_(i+1), ell -> (
		    apply(ell, j -> if j==0 then 0 else 1)
		    ))
	    ));
    )

testOfLinearity = method()
--whether degrees will become linear after truncating at b
--so all outputs will be "true" iff b is in linearTruncationsBound
testOfLinearity(List, Module) := (b, M) -> (
    deglists := supportOfTor M;
    absb := sum b;
    apply(#deglists, i -> apply(deglists_i, a ->
	    sum compMax{a,b} <= absb+i
	    ))
    )

nonLinearSupportOfTor = method()
nonLinearSupportOfTor(ZZ,List) := (d,L) -> (
    apply(#L, i -> select(L_i, ll -> sum ll > d+i))
    )
nonLinearSupportOfTor List := L -> (
    d := sum compMin L_0;
    nonLinearSupportOfTor(d,L)
    )
nonLinearSupportOfTor Module := M -> (
    nonLinearSupportOfTor supportOfTor M
    )

predictedNonLinearSupportOfTor = method()
--expected non-linear syzygies after truncating at L
--often but not always equal to nonLinearSupportOfTor truncate(L,M)
predictedNonLinearSupportOfTor(List, Module) := (L,M) -> (
    absL := sum L;
    is := nonLinearSupportOfTor M;
    is  = apply(is, ell -> apply(ell, L' -> compMax{L,L'}));
    nonLinearSupportOfTor(absL,is)
    )

-------------------------

TEST ///
S = multigradedPolynomialRing({1,2,3}, Variables => "y")
assert(unique degrees S == {{1,0,0},{0,1,0},{0,0,1}})
assert(#gens S == 2+3+4)
assert instance(x,Symbol)
assert instance(y,IndexedVariableTable)
///

TEST ///
S = multigradedPolynomialRing({1,2}, Standard => false)
assert(irrelevantIdeal S == ideal(a*c,a*d,a*e,b*c,b*d,b*e))
///

TEST ///
L = diagonalMultidegrees(5,3);
assert all(L, ell -> sum ell == 5)
assert all(L, ell -> #ell == 3)
assert(#L == binomial(5+3-1, 3-1))
assert(diagonalMultidegrees(-1,2) == {})
///

TEST ///
L = {{1,-5,-1},{-3,5,-3},{-3,2,-5},{-5,-2,5},{-1,-5,-2},{5,2,4},{-4,-3,2}}
L' = findMins L
assert all(L', ell -> any(L, em -> all(ell-em, i -> i >= 0)))
assert all(L, ell -> all(L', em -> ell != em) or not any(L', em -> all(ell-em, i -> i > 0)))
///

TEST ///
S = multigradedPolynomialRing {1,1,1}
--R = apply(3, i -> (baseRing S)[gens S, Degrees => apply(degrees S, ell -> ell_i)]) | {S}
M = comodule ideal(x_(0,0)*x_(0,1)*x_(1,0)*x_(2,0)^2,x_(0,1)^2*x_(1,0)^2*x_(1,1)^2*x_(2,0)^2,
    x_(0,0)^3*x_(1,0)*x_(2,1),x_(0,0)^2*x_(0,1)^2*x_(2,0)*x_(2,1),
    x_(0,0)*x_(0,1)^2*x_(1,1)^2*x_(2,0)^3,x_(0,1)^3*x_(1,0)^2*x_(1,1)*x_(2,1)^2
    )
assert(partialRegularities M == {4,3,2})
assert(regularityBound M == {{4,3,2}})
///

TEST ///
S = multigradedPolynomialRing 2
M = {
    {{{-1,0},{0,-1}},{{-1,-1}},{}},
    {{{-3,-3},{-3,-3}},{{-4,-3},{-3,-4}},{{-4,-4}},{}},
    {{{0,0}},{{0,0}},{}},
    {{{1,1}},{{0,1}},{{0,0}},{}},
    {{{0,0}},{{-1,-1}},{{-2,-1},{-1,-2}},{}},
    {{{0,0}},{{-1,0},{0,-1}},{{-2,0},{-1,-1},{0,-2}},{}},
    {{{0,0}},{{-1,0},{0,-1}},{{-2,0},{-1,-1},{0,-2}},{{0,-4}},{}}
    };
--twists appearing in chain complexes
C = apply(M, L -> chainComplex(apply(#L - 1, i -> map(S^(L_i),S^(L_(i+1)),0))));
A = {
    false,
    true,
    false,
    true,
    false,
    true,
    false
    }
--expected answers for isLinearComplex
scan(7, i -> assert(isLinearComplex C_i === A_i))
scan(7, i -> assert(set supportOfTor C_i === set(-drop(M_i/unique,-1))))
///

TEST ///
S = ZZ/101[x,y,Degrees=>{{1,0},{0,1}}]
I = ideal(x*y^2,x^3*y)
M = S^1/I
f = (d,M) -> truncate(d,M)==0
assert(set findRegion({{0,0},{4,4}},M,f) === set {{1,2},{3,1}})
///

TEST ///
L = {{0,1,1},{0,2,2},{1,2,1}}
L' = apply(3, i -> apply(L, ell -> ell_i))/max
S = multigradedPolynomialRing 3
M = S^(-L)
assert(linearTruncations M == {L'})
assert(regularityBound M == {L'})
assert(partialRegularities M == L')
///

TEST ///
S = multigradedPolynomialRing 2
M = coker map(S^{{-1,0},{0,-1},{0,-1}},S^{{-1,-1},{-1,-1}},
    {{x_(1,0),x_(1,1)},{-x_(0,0),0},{0,-x_(0,1)}})
assert(regularityBound M == {{1,1}})
assert(partialRegularities M == {1,1})
assert(set linearTruncations M === set {{0,2},{1,1}})
///

TEST ///
L = {{3,4,1},{5,8,1},{10,2,7}}
assert all(L, ell -> all(ell - compMin L, i -> i >= 0))
assert all(L, ell -> all(compMax L - ell, i -> i >= 0))
///

-------------------------

beginDocumentation()

doc ///
Key
  LinearTruncations
Headline
  truncations of a multigraded module that give linear resolutions
Description
  Text
    Let $k$ be a field, $S$ a $\ZZ^r$-graded polynomial ring over $k$, and $M$ a finitely generated, $\ZZ^r$-graded $S$-module.
    Write $M_{\geq d}$ for the truncation $\oplus_{d'\geq d} M_d'$ of $M$ at $d$ (where $d'\geq d$ if $d'_i\geq d_i$ for all $i$). 
    The main purpose of this package is to find the degrees $d\in\ZZ^r$ so that $M_{\geq d}$ has a linear resolution,
    i.e. satisfies the function @TO isLinearComplex@.  No sufficient finite search space is known, so the result may not be complete.
  Example
    S = ZZ/101[x_1..x_4,Degrees=>{{1,0},{1,0},{0,1},{0,1}}]
    I = ideal(x_1^3*x_3, x_2*x_3*x_4, x_3^4*x_4, x_4*x_2^2, x_1^3*x_2^3, x_3^3)
    M = S^1/I
    regularity M
    r = coarseMultigradedRegularity M
    L = linearTruncations({{0,0}, r}, M)
    --bounds={{5,2}}
    --linearTruncations={{2,3},{3,2}}
    apply(L, i -> isLinearComplex res truncate(i,M))
  Text
    If $M_{\geq d}$ has a linear truncation then $M_{\geq d'}$ has a linear truncation for all $d'\geq d$, so the function
    @TO linearTruncations@ gives the minimal such multidegrees in a given range, using the function @TO findRegion@.  The functions
    @TO linearTruncationsBound@ and @TO regularityBound@ estimate the linear truncation region and the multigraded regularity region
    of $M$, respectively, without calculating cohomology or truncations.
Caveat
  If the ring $S$ is standard $\ZZ$-graded then $M_{\geq d}$ has a linear resolution if and only if $d\geq\operatorname{reg} M$, 
  where $\operatorname{reg} M$ is the Castelnuovo-Mumford regularity of $M$.
SeeAlso
  Truncations
Subnodes
  multigradedPolynomialRing
  irrelevantIdeal
  diagonalMultidegrees
  findMins
  findRegion
  partialRegularities
  isLinearComplex
  linearTruncations
  supportOfTor
  linearTruncationsBound
  regularityBound
  isQuasiLinear
  compMin
///

doc ///
Key
   diagonalMultidegrees
  (diagonalMultidegrees,ZZ,ZZ)
  (diagonalMultidegrees,ZZ,List)
Headline
  t-tuples of non-negative integers with sum equal to d
Usage
  diagonalMultidegrees(d,t)
  diagonalMultidegrees(d,L)
Inputs
  d: ZZ
    the (non-negative) sum of the tuples
  t: ZZ
    the length of the tuples
  L: List
    the maximum value for each coordinate
Outputs
  : List
    containing lists of @TT "t"@ integers each
Description
  Text
    Given numbers @TT "d"@ and @TT "t"@, this function gives all lists of @TT "t"@ non-negative integers where the sum of each list is equal to @TT "d"@.
    If the second input is a list @TT "L"@, the tuples have the same length as @TT "L"@ and are constrained to have @TT "i"@-th entry at most @TT "1+L_i"@.
  Example
    t = 2
    d = 5
    diagonalMultidegrees(d,t)
    diagonalMultidegrees(d, {0,1,2})
SeeAlso
  (basis,ZZ,Module)
///

--TODO: check that modules are over polynomial rings? products of projective spaces?

doc ///
Key
   partialRegularities
  (partialRegularities,Module)
  (partialRegularities,ChainComplex)
Headline
  calculates Castelnuovo-Mumford regularity in each component of a multigrading
Usage
  partialRegularities M
Inputs
  M: Module
    a multigraded module
  F: ChainComplex
    the minimal free resolution of a module
Outputs
  : List
    with @TT "i"@-th coordinate the regularity in the @TT "i"@-th component of the grading
Description
  Text
    This function applies the definition of Castelnuovo-Mumford regularity to the complex obtained by
    resolving the module $M$ and forgetting all but the @TT "i"@-th coordinate of the twists appearing.
    Alternately, the minimal resolution of $M$ can be given directly.
  Example
    S = ZZ/101[x_0,x_1,y_0,y_1,z_0,z_1,Degrees=>{{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}}]
    I = ideal(x_0*x_1*y_0*z_0^2, x_1^2*y_0^2*y_1^2*z_0^2, x_0^3*y_0*z_1, x_0^2*x_1*y_1*z_0*z_1, x_0*x_1^2*y_1^2*z_0^3, x_1^3*y_0^2*y_1*z_1^2)
    M = S^1/I
    netList supportOfTor M
    partialRegularities M
  Text
    In the bigraded case this element will always be contained in the multigraded regularity if its total degree is at least $\operatorname{reg} M$.
Caveat
  Changing the grading of $M$ and applying the command @TO regularity@ will not yield the correct result because betti
  tables in @ITALIC "Macaulay2"@ do not (at least at the time of this writing) accommodate rings with generators of degree 0.
SeeAlso
  regularity
///

doc ///
Key
   isLinearComplex
  (isLinearComplex,ChainComplex)
Headline
  tests whether a complex of graded modules is linear
Usage
  isLinearComplex F
Inputs
  F: ChainComplex
Outputs
  : Boolean
    true if @TT "F"@ is a linear complex, false if it is not
Description
  Text
    We say that a (nonzero) $\ZZ^r$-graded complex @TT "F"@ is linear if its first nonzero module is generated in a single multidegree
    and all of its maps (including the zero maps) have degree at most 1.  For instance, if @TT "F_i"@ is zero for $i<0$ and @TT "F_0"@ is generated in degree 
    $0\in\ZZ^r$, then @TT "F_i"@ should be generated in multidegrees with sum at most $i$ for $i>0$.  If @TT "F"@ is zero then the function returns true.
  Example
    S = ZZ/101[x_1..x_4]
    I = ideal(x_1*x_2, x_1*x_3,x_1*x_4, x_2*x_3, x_3*x_4)
    M = S^1/I
    F = res M
    betti F
    isLinearComplex F
    F' = res truncate(2,M)
    betti F'
    isLinearComplex F'
Caveat
  It is possible for a complex to have differential matrices containing only linear entries yet be nonlinear by the definition above.
SeeAlso
  isQuasiLinear
///

doc ///
Key
   linearTruncations
  (linearTruncations,List,Module)
  (linearTruncations,Module)
Headline
  finds minimal multidegree(s) in a given range where the resolution of a truncated module is linear
Usage
  linearTruncations(L,M)
  linearTruncations M
Inputs
  L: List
    containing 2 lists: minimum and maximum multidegrees defining range
  M: Module
    the module to be truncated
Outputs
  : List
    containing multidegrees with linear truncations
Description
  Text
    Given a list @TT "{L1,L2}"@ and a module $M$, this function truncates $M$ at each multidegree between @TT "L1"@ and @TT "L2"@ and tests whether the minimal resolution
    of the resulting module is linear.  The set of multidegrees producing linear resolutions is invariant under positive translation, and the
    function returns the minimal multidegrees in this set.  If a list @TT "L"@ is not provided it will search between the componentwise minimum of the
    degrees of the generators of $M$ and the degree with all coordinates equal to $r+1$, where $r$ is the @TO regularity@ of $M$.
  CannedExample
    i1 : S = multigradedPolynomialRing({2,3},Standard=>false)
    
    o1 = S
    
    o1 : PolynomialRing
    
    i2 : M = coker map(S^{{0,-3},{0,-3},{-3,0},{-3,0},{-3,0},{-3,0}},S^{{-3,-3},{-3,-3},{-3,-3},{-3,-3},{-3,-3},{-3,-3}},
	 {{a^3,b^3,c^3, 0,0,0},{0,0,0,b^3,a^3,c^3},{-d^3,0,0,-d^3,0,0},{0,-e^3,0,0,-e^3,0},{0,0,-f^3,0,0,-f^3},{0,0,0,-g^3,0,0}})
    
    o2 = cokernel {0, 3} | a3  b3  c3  0   0   0   |
                  {0, 3} | 0   0   0   b3  a3  c3  |
                  {3, 0} | -d3 0   0   -d3 0   0   |
                  {3, 0} | 0   -e3 0   0   -e3 0   |
                  {3, 0} | 0   0   -f3 0   0   -f3 |
                  {3, 0} | 0   0   0   -g3 0   0   |
    
                                 6
    o2 : S-module, quotient of S
    
    i3 : linearTruncations M
    
    o3 = {{3, 3}}
    
    o3 : List
    
    i4 : linearTruncations({{0,0},{9,9}}, M)
    
    o4 = {{3, 3}, {8, 2}}
    
    o4 : List
Caveat
  With the default options there may be generators of the linear truncation region not contained
  in the search region and thus not returned.
SeeAlso
  coarseMultigradedRegularity
  isLinearComplex
///

doc ///
Key
   multigradedPolynomialRing
  (multigradedPolynomialRing,List)
  (multigradedPolynomialRing,ZZ)
  [multigradedPolynomialRing,CoefficientField]
  [multigradedPolynomialRing,Variables]
  [multigradedPolynomialRing,Standard]
Headline
  produces polynomial rings with standard multigradings
Usage
  multigradedPolynomialRing L
  multigradedPolynomialRing n
Inputs
  L: List
    entries 1 less than the desired number of variables for each multidegree
  n: ZZ
    the rank of the grading, in the case with 2 variables for each multidegree
  CoefficientField=>Ring
    the coefficient field
  Variables=>String
    name to use for indexed variables
  Standard=>Boolean
Outputs
  : PolynomialRing
    a multigraded polynomial ring
Description
  Text
    Given a list @TT "L"@, this function gives a $\ZZ^r$-graded polynomial ring (where $r$ is the length of @TT "L"@) containing @TT"L_i+1"@ variables
    of multidegree equal to the @TT "i"@-th basis vector of $\ZZ^r$, i.e. the coordinate ring of the product of projective spaces with
    dimensions the entries of @TT "L"@.  Given an integer @TT "n"@ it returns the coordinate ring of a product of @TT "n"@ copies of $\PP^1$.
  Example
    S = multigradedPolynomialRing({1,3,4})
    gens S
    degrees S
    gens multigradedPolynomialRing 4
  Text
    By default the output will be a ring over @TT "ZZ/32003"@ in variables of the form @TT "x_(i,j)"@.  The coefficients can be changed using the option 
    @TT "CoefficientField"@ and the variable name with @TT "Variables"@ (which takes a string).  Setting the option @TT "Standard"@ to false will produce
    variables with no indices, starting at @TT "a"@.
  Example
    multigradedPolynomialRing({1,2},CoefficientField => ZZ/5,Variables=>"y")
    multigradedPolynomialRing(3,Standard=>false)
Caveat
  The output of @TT "multigradedPolynomialRing"@ is not compatible with some functions from the package @TO "TateOnProducts"@, such as @TO "TateOnProducts :: cohomologyHashTable"@.
  Use @TO "TateOnProducts :: productOfProjectiveSpaces"@ instead.
SeeAlso
  productOfProjectiveSpaces
  irrelevantIdeal
///

doc ///
Key
   irrelevantIdeal
  (irrelevantIdeal,Ring)
Headline
  gives the irrelevant ideal of the coordinate ring of a product of projective spaces
Usage
  irrelevantIdeal R
Inputs
  R: Ring
    coordinate ring of a product of projective spaces
Outputs
  : Ideal
    irrelevant ideal in @TT "R"@
Description
  Text
    Given the coordinate ring of a product of projective spaces, this function produces the irrelevant ideal (in the sense of toric geometry)
    by listing the unique degrees of generators of @TT "R"@, creating an ideal from the generators of each degree, and intersecting them.
  Example
    R = multigradedPolynomialRing {1,2}
    irrelevantIdeal R
    R = multigradedPolynomialRing 3
    irrelevantIdeal R
Caveat
  This function will not give the correct irrelevant ideal for the Cox ring of a toric variety that is not a product of projective spaces.
  Use the package @TO "NormalToricVarieties"@ instead.
SeeAlso
  multigradedPolynomialRing
  --"NormalToricVarieties :: ideal(NormalToricVariety)"
--TODO: uncomment these when documentation error messages fixed
///

doc ///
Key
   findMins
  (findMins,List)
  (findMins,Ideal)
Headline
  calculates the minimal elements of a subset of ZZ^r
Usage
  findMins L
Inputs
  L: List
    subset of $\ZZ^r$
Outputs
  : List
    minimal elements
Description
  Text
    Given a list @TT "L"@ of elements in $\ZZ^r$, this function will return the minimal elements of @TT "L"@ under the partial order where
    $c\leq d$ if and only if $c_i\leq d_i$ for all $1\leq i\leq r$. 
  Example
    L = {{1,2},{3,1},{3,2},{1,4}}
    findMins L
  Text
    The algorithm constructs a monomial ideal by using the elements of the list as exponents.  Such an ideal can also be given directly.
SeeAlso
  findRegion
///

doc ///
Key
   findRegion
   Inner
   Outer
  (findRegion,List,Ideal, Function)
  (findRegion,List,Module,Function)
Headline
  finds minimal multidegree(s) in a given region where an ideal or module satisfies a Boolean function
Usage
  findRegion({L1,L2},M,f)
Inputs
  L1: List
    the minimum multidegree to check
  L2: List
    the maximum multidegree to check
  M:{Module,Ideal}
  f: Function
    a function that takes a List and an Ideal or Module and returns a Boolean value
Outputs
  : List
    minimal multidegrees
Description
  Text
    $M$ should be an ideal or a module over a $\ZZ^r$-graded ring and
    @TT "f"@ a function so that @TT "f(d,M)"@ is a Boolean value for @TT "d"@ in $\ZZ^r$
    and @TT "f(d,M)"@ implies @TT "f(d+e,M)"@ for @TT "e"@ in $\NN^r$.
    Given a list @TT "{L1, L2}"@, this function will return the minimal multidegrees @TT "d"@
    between @TT "L1"@ and @TT "L2"@ satisfying @TT "f(d,M)"@.
  Example
    S = ZZ/101[x,y,Degrees=>{{1,0},{0,1}}]
    I = ideal(x*y^2,x^3*y)
    M = S^1/I
    f = (d,M) -> truncate(d,M)==0
    findRegion({{0,0},{4,4}},M,f)
  Text
    If some degrees @TT "d"@ are known to satisfy @TT "f(d,M)"@, then they can be specified
    using the option @TT "Inner"@ in order to expedite the computation.  Similarly, degrees
    not above those given in @TT "Outer"@ will be assumed not to satisfy @TT "f(d,M)"@.
    If @TT "f"@ takes options these can also be given to @TT "findRegion"@.
  Example
    elapsedTime findRegion({{0,0},{4,4}},M,f)
    elapsedTime findRegion({{0,0},{4,4}},M,f,Inner=>{{1,2},{3,1}},Outer=>{{1,1}})
Caveat
  Use the option @TT "Outer"@ with care if @TT "f"@ is not invariant under positive translation.
Contributors
  Mahrud Sayrafi contributed to the code for this function.
SeeAlso
  findMins
  "VirtualResolutions :: multigradedRegularity"
///

doc ///
Key
   linearTruncationsBound
  (linearTruncationsBound,Module)
Headline
  bounds the region where truncations of a module have linear resolutions
Usage
  linearTruncationsBound M
Inputs
  M: Module
    a multigraded module
Outputs
  : List
    containing multidegrees where the truncation of M will have a linear resolution
Description
  Text
    Using only the multigraded betti numbers of a $\ZZ^r$-graded module $M$,
    this function identifies multidegrees at which the truncation of $M$ will have
    a linear minimal resolution (i.e. where the resolution will satisfy 
    @TT "isLinearComplex"@).
  Example
    S = ZZ/101[x_0,x_1,y_0,y_1,z_0,z_1,Degrees=>{{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}}]
    I = ideal(x_0*x_1*y_0*z_0^2, x_1^2*y_0^2*y_1^2*z_0^2, x_0^3*y_0*z_1, x_0^2*x_1*y_1*z_0*z_1, x_0*x_1^2*y_1^2*z_0^3, x_1^3*y_0^2*y_1*z_1^2)
    M = S^1/I
    L = linearTruncationsBound M
    apply(L, d -> isLinearComplex res prune truncate(d,M))
  Text
    The output is a list of the minimal multidegrees $d$ such that the sum of the positive
    coordinates of $b-d$ is at most $i$ for all degrees $b$ appearing in the @TT "i"@-th step of
    the resolution of $M$.
  Example
    elapsedTime linearTruncations({{2,2,2},{4,4,4}}, M)
    elapsedTime linearTruncationsBound M
Caveat
  In general @TT "linearTruncationsBound"@ will not find the minimal degrees where $M$
  has a linear resolution but will be faster than repeatedly truncating $M$.
SeeAlso
  isLinearComplex
  linearTruncations
///

doc ///
Key
   regularityBound
  (regularityBound,Module)
Headline
  bounds the multigraded regularity of a module
Usage
  regularityBound M
Inputs
  M: Module
    a multigraded module with no $H^0_B$ or $H^1_B$
Outputs
  : List
    containing multidegrees in the regularity of @TT "M"@
Description
  Text
    Using only the multigraded betti numbers of a $\ZZ^r$-graded module $M$, this function identifies a subset of the
    multigraded regularity of a module $M$ over the coordinate ring $S$ of a product of projective spaces, in the sense of Maclagan
    and Smith.  It assumes that the local cohomology groups $H^0_B(M)$ and $H^1_B(M)$ vanish, where $B$ is the irrelevant ideal of $S$.
  Example
    (S,E) = productOfProjectiveSpaces {1,2}
    I = ideal(x_(0,0)*x_(1,0),x_(1,1)^3)
    M = S^1/I
    regularityBound M
    needsPackage "VirtualResolutions"
    multigradedRegularity(S,M)
  Text
    The output is often but not always @TT "{partialRegularities M}"@.
Caveat
  In general @TT "regularityBound"@ will not give the minimal elements of $\operatorname{reg} M$ but will be faster than computing cohomology.
SeeAlso
  partialRegularities
  isQuasiLinear
  "VirtualResolutions :: multigradedRegularity"
///

doc ///
Key
   isQuasiLinear
   IrrelevantIdeal
  (isQuasiLinear,List,Module)
  (isQuasiLinear,BettiTally)
  (isQuasiLinear,ChainComplex)
  [isQuasiLinear,IrrelevantIdeal]
Headline
  checks whether degrees in the resolution of a truncation are at most those of the irrelevant ideal
Usage
  isQuasiLinear(d,M)
Inputs
  d: List
    a multidegree at which to truncate the module
  M: Module
    a module over a multigraded ring $S$
  IrrelevantIdeal=>Ideal
    the irrelevant ideal of $S$
Outputs
  : Boolean
Description
  Text
    This function truncates the module $M$ at degree $d$ and compares the twists appearing in its resolution with those appearing in the 
    resolution of $S/B$, where $S$ is the ring of $M$ and $B$ the irrelevant ideal.  If for some $i$ the @TT "i"@-th step of the resolution contains a summand
    $S(-c-d)$ such that no summand of the @TT "i"@-th step of the resolution of $S/B$ has generator of degree greater than $c$ then the output will be
    false.

    If the option @TT "IrrelevantIdeal"@ is not specified it will be calculated assuming that $S$ is the coordinate ring of a product of
    projective spaces.
  Example
    S = ZZ/101[x_0,x_1,y_0,y_1,z_0,z_1,Degrees=>{{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}}]
    I = ideal(x_0*x_1*y_0*z_0^2, x_1^2*y_0^2*y_1^2*z_0^2, x_0^3*y_0*z_1, x_0^2*x_1*y_1*z_0*z_1, x_0*x_1^2*y_1^2*z_0^3, x_1^3*y_0^2*y_1*z_1^2)
    M = S^1/I
    d = {4,3,2}
    isLinearComplex res prune truncate({4,3,2},M)
    isQuasiLinear(d,M)
  Text
    The condition comes from Theorem 2.9 in Berkesch, Erman, and Smith's paper "Virtual Resolutions for a Product of Projective Spaces."
    The @TT "ChainComplex"@ and @TT "BettiTally"@ usages take the resolution of the truncation (or some other virtual resolution) directly.
Caveat
  If the resolution of the truncation is longer than the resolution of $S/B$ then @TT "isQuasiLinear"@ will return false.
SeeAlso
  irrelevantIdeal
  truncate
///

doc ///
Key
   supportOfTor
  (supportOfTor,Module)
  (supportOfTor,ChainComplex)
Headline
  computes multidegrees in the support of Tor_i(M,k), where k is the residue field
Usage
  supportOfTor M
Inputs
  M: Module
    a multigraded module
  F: ChainComplex
    the minimal resolution of a module
Outputs
  L: List
    where @TT "L_i"@ contains multidegrees for $\operatorname{Tor}_i(M,k)$
Description
  Text
    This function computes a minimal free resolution of the (pruned) module $M$, reduces it by the maximal ideal, and returns a list of the
    unique degrees that occur at each step.
  Example
    S = multigradedPolynomialRing {1,2}
    B = irrelevantIdeal S
    M = S^1/B
    F = res prune M
    multigraded betti F
    supportOfTor M
    netList supportOfTor M
  Text
    Alternately, the minimal free resolution can be given directly.
  Example
    netList supportOfTor F
SeeAlso
  (multigraded,BettiTally)
///

doc ///
Key
   compMin
  (compMin,List)
  (compMin,List,List)
   compMax
  (compMax,List)
  (compMax,List,List)
Headline
  takes componentwise minimum or maximum of a list of lists
Usage
  compMin L
  compMin(L1,L2)
  compMax L
  compMax(L1,L2)
Inputs
  L: List
    containing lists of the same length
Outputs
  : List
    with the same length as those in @TT "L"@
Description
  Text
    The functions @TT "compMin"@ and @TT "compMax"@ take the minimum and maximum, respectively,
    of a list of lists of integers (or elements of another partially ordered set) 
    by considering each component separately.
  Example
    L = {{3,4,1},{5,8,1},{10,2,7}}
    compMin L
    compMax L
  Text
    The usage @TT "compMin(L1,L2)"@ gives the same output as @TT "compMin {L1,L2}"@.
Caveat
  If L is empty it will be returned unchanged.
///

end--
