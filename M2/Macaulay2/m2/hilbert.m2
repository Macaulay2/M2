--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

needs "max.m2" -- infinity
needs "modules2.m2"

protect symbol Order

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

recipN := (n, wts, f) -> (
     -- n is a positive integer
     -- wts is a weight vector
     -- f is a polynomial of the form 1 plus terms of positive weight, which we verify
     -- we compute the terms of the expansion of 1/f of weight less than n
     if n <= 0 then error "expected a positive integer";
     if part(, 0, wts, f) != 1 then error "expected a polynomial of the form 1 plus terms of positive weight";
     g := 1_(ring f); -- g always has the form 1 plus terms weight 1,2,...,m-1
     m := 1;          -- 1-f*g always has terms of wt m and higher
     tr := h -> part(, m-1, wts, h);
     while m < n do (
	  m = 2*m;
	  g = g + tr(g * (1 - tr(g * tr f)));
	  );
     if m === n then g else part(, n-1, wts, g))

-- truncate f * g^e
truncatePower := (f, g, e, tr) -> fold(0..e-1, f, (i, r) -> tr(r * g))

-- truncate a power series element f given as an expression of type
-- Divide{RingElement, Product{Power{1-T_i, ZZ}, ...}}.
truncateSeries = (n, wts, f) -> (
    if n === infinity then return f;
    tr := h -> part(, n-1, wts, h);
    if not instance(f, Divide) then return tr f;
    num := tr numerator f;
    if num == 0 then return 0_(ring num);
    den := denominator f; -- a Product of Power expressions
    (lo, hi) := weightRange(wts, num);
    fold(toList den, num,
	(pow, r) -> truncatePower(r, recipN(n-lo, wts, pow#0), pow#1, tr)))

-----------------------------------------------------------------------------
-- helpers for hilbert methods
-----------------------------------------------------------------------------

-- also used in betti.m2
hilbertFunctionRing = memoize(() -> QQ(monoid [getSymbol "i"]))
hilbertFunctionQ = method()
hilbertFunctionQ ZZ := n -> (
    if n === 0 then 1_(hilbertFunctionRing())
    else (
	i := (hilbertFunctionRing())_0;
	(1/n) * (n+i) * hilbertFunctionQ(n-1)))
hilbertFunctionQ(ZZ, ZZ) := memoize(
    (n, d) -> (
	if d === 0 then hilbertFunctionQ(n)
	else (
	    i := (hilbertFunctionRing())_0;
	    substitute(hilbertFunctionQ(n), {i => i+d}))))

-----------------------------------------------------------------------------
-- heft
-----------------------------------------------------------------------------

-- TODO: where should this go?
heft = method()
heft Ring           :=
heft Monoid         := R -> if (o := options R) =!= null and o.?Heft then o.Heft
heft PolynomialRing := R -> heft R.FlatMonoid
heft QuotientRing   := R -> heft ambient R
-- TODO: deprecate this in favor of just "heft ring M"
heft Module         := M -> heft ring M

-----------------------------------------------------------------------------
-- poincare
-----------------------------------------------------------------------------

-- see the comment in the documentation for (degree, Ideal) about what this means when M is not homogeneous
poincare = method(TypicalValue => RingElement)
poincare Ring   := R -> poincare module R
poincare Ideal  := I -> poincare comodule I
poincare Module := M -> (
    computation := (cacheValue symbol poincare) (M -> runHooks((poincare, Module), M));
    if (P := computation M) =!= null then return P;
    error("no applicable strategy for computing poincare over ", toString ring M))

addHook((poincare, Module), Strategy => Default, M -> (
	new degreesRing M from rawHilbert raw leadTerm gb -* presentation cokernel ?? *- presentation M))

-- manually installs the numerator of the reduced Hilbert series for the module
storefuns#poincare = method()
storefuns#poincare(Ideal,  RingElement) := (I, hf) -> storefuns#poincare(comodule I, hf)
storefuns#poincare(Matrix, RingElement) := (m, hf) -> storefuns#poincare(cokernel m, hf)
storefuns#poincare(Module, RingElement) := (M, hf) -> M.cache.poincare = substitute(hf, degreesRing M)

-- TODO: deprecate this
installHilbertFunction = storefuns#poincare

-----------------------------------------------------------------------------
-- pdim, dim, degree, multidegree, length
-----------------------------------------------------------------------------

pdim Module := M -> length resolution minimalPresentation M

dim Ideal  := I -> dim comodule I
dim Module := M -> if (c := codim M) === infinity then -1 else dim ring M - c

degree Ring   := R -> degree module R
degree Ideal  := I -> degree comodule I
degree Module := M -> (
    computation := (cacheValue symbol degree) (M -> runHooks((degree, Module), M));
    if (d := computation M) =!= null then return d;
    error("no applicable strategy for computing degree of modules over ", toString ring M))

addHook((degree, Module), Strategy => Default, M -> (
	R := ring M;
	if (hft := heft R) === null then error "degree: no heft vector defined";
	T := degreesRing 1;
	A := degreesRing R;
	n := degreeLength R;
	hn := poincare M;
	if n === 0 then return lift(hn, ZZ);
	-- this assigns a privileged role to the heft vector, which we need to investigate
	to1 := map(T, A, apply(hft, i -> T_{i}));
	hn = to1 hn;
	if hn == 0 then return 0;
	h := 1 - T_0;
	while hn % h == 0 do hn = hn // h;
	ev := map(ZZ, T, {1});
	ev hn))

multidegree Ring   := R -> multidegree module R
multidegree Ideal  := I -> multidegree comodule I
multidegree Module := M -> (
    computation := (cacheValue symbol multidegree) (M -> runHooks((multidegree, Module), M));
    if (d := computation M) =!= null then return d;
    error("no applicable strategy for computing multidegree of modules over ", toString ring M))

addHook((multidegree, Module), Strategy => Default, M -> (
    A := degreesRing M;
    if (c := codim M) === infinity then return 0_A;
    onem := map(A, A, apply(generators A, t -> 1 - t));
    part(c, numgens A:1, onem numerator poincare M))
    )

length Module := ZZ => M -> (
    computation := (cacheValue symbol length) (M -> runHooks((length, Module), M));
    if (n := computation M) =!= null then return n;
    error("no applicable strategy for computing length of modules over ", toString ring M))

addHook((length, Module), Strategy => Default, M -> (
    if not isHomogeneous M then notImplemented();
    if dim M > 0 then infinity else degree M))

-----------------------------------------------------------------------------
-- ProjectiveHilbertPolynomial type declaration
-----------------------------------------------------------------------------

ProjectiveHilbertPolynomial = new Type of HashTable
ProjectiveHilbertPolynomial.synonym = "projective Hilbert polynomial"

-- printing
expression ProjectiveHilbertPolynomial := h -> sum(sort pairs h, (n, c) -> c * new Subscript from {"P", n})
net     ProjectiveHilbertPolynomial :=     net @@ expression
texMath ProjectiveHilbertPolynomial := texMath @@ expression

-- basic constructor
projectiveHilbertPolynomial = method(TypicalValue => ProjectiveHilbertPolynomial)
projectiveHilbertPolynomial ZZ      := n -> new ProjectiveHilbertPolynomial from { n => 1 }
projectiveHilbertPolynomial(ZZ, ZZ) := memoize(
    (n, d) -> new ProjectiveHilbertPolynomial from (
	if d <= 0
	then apply(min(-d+1, n+1), j -> n-j => (-1)^j * binomial(-d, j))
	else apply(n+1, j -> n-j => binomial(d-1+j, j))))

-- arithmetic ops
-- TODO: how can we abstract away this section?
P0 := projectiveHilbertPolynomial 0
ProjectiveHilbertPolynomial == ProjectiveHilbertPolynomial := Boolean => (h, k) -> h === k
ProjectiveHilbertPolynomial + ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (h, k) -> merge(h, k, continueIfZero @@ plus)
ProjectiveHilbertPolynomial - ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (h, k) -> h + -k
   - ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => h -> applyValues(h, minus)
ZZ * ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (b, h) -> (
    if b === 0 then new ProjectiveHilbertPolynomial from {} else if b === 1 then h else applyValues(h, c -> b * c))
ProjectiveHilbertPolynomial * ZZ := ProjectiveHilbertPolynomial => (h, b) -> b * h
ProjectiveHilbertPolynomial + ZZ := ProjectiveHilbertPolynomial => (h, n) -> h + n * P0
ZZ + ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (n, h) -> h + n * P0
ProjectiveHilbertPolynomial - ZZ := ProjectiveHilbertPolynomial => (h, n) -> h - n * P0
ZZ - ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (n, h) -> -h + n * P0
ProjectiveHilbertPolynomial == ZZ := Boolean => (h, n) -> h === n * P0
ZZ == ProjectiveHilbertPolynomial := Boolean => (n, h) -> h === n * P0

-- evaluation
ProjectiveHilbertPolynomial ZZ := (P, i) -> sum(pairs P, (n, c) -> c * binomial(n + i, n))

-- other methods
euler  ProjectiveHilbertPolynomial := P -> P(0)
dim    ProjectiveHilbertPolynomial := P -> if #P === 0 then -1 else max keys P
degree ProjectiveHilbertPolynomial := P -> if #P === 0 then 0 else P#(dim P)

-- differentiation
diff(ProjectiveHilbertPolynomial, ZZ) := ProjectiveHilbertPolynomial => (P,i) -> (
    new ProjectiveHilbertPolynomial from select(apply(pairs P, (n, c) -> (n - i, c)), (n, c) -> n >= 0))
diff ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => P -> diff(P, 1)

-----------------------------------------------------------------------------
-- hilbertPolynomial
-----------------------------------------------------------------------------

hilbertPolynomial = method(TypicalValue => ProjectiveHilbertPolynomial, Options => { Projective => true })
hilbertPolynomial Ring   := opts -> R -> hilbertPolynomial(module R, opts)
hilbertPolynomial Ideal  := opts -> I -> hilbertPolynomial(comodule I, opts)
hilbertPolynomial Module := opts -> M -> (
    HP := runHooks((hilbertPolynomial, Module), (opts, M));
    if HP =!= null then return HP;
    error("no applicable strategy for computing Hilbert polynomial over ", toString ring M))

addHook((hilbertPolynomial, Module), Strategy => Default, (opts, M) -> (
    R := ring M;
    if not isHomogeneous M then error "hilbertPolynomial: expected a homogeneous module";
    if degreeLength R != 1 then error "hilbertPolynomial: expected a singly graded ring";
    if not all(degrees R, d -> d === {1}) then error "hilbertPolynomial: expected a ring whose variables all have degree 1";
    --
    n := numgens R - 1;
    p := pairs standardForm poincare M;
    if opts.Projective then (
	if #p === 0 then new ProjectiveHilbertPolynomial from {}
	else sum(p, (d, c) -> (
		if #d === 0 then d = 0 else d = d#0;
		c * projectiveHilbertPolynomial(n, -d))))
    else (
	if #p === 0 then 0_(hilbertFunctionRing())
	else sum(p, (d, c) -> (
		if #d === 0 then d = 0 else d = d#0;
		c * hilbertFunctionQ(n, -d)))))
    )

-----------------------------------------------------------------------------
-- euler, eulers, genus, genera
-----------------------------------------------------------------------------

euler Ring   := R -> euler module R
euler Module := M -> euler hilbertPolynomial M

eulers Ring   := R -> eulers module R
eulers Module := M -> (
    h := hilbertPolynomial M;
    for i in 0 .. dim h list euler diff(h, i))

genus Ring   := R -> genus module R
genus Module := M -> (
    e := euler M;
    d := dim M - 1;
    (-1)^d * (e - 1))

genera Ring   := R -> genera module R
genera Module := M -> (
    e := eulers M;
    d := dim M - 1;
    apply(#e, i -> (-1)^(i+d) * (e#i - 1)))

-----------------------------------------------------------------------------
-- reduceHilbert
-----------------------------------------------------------------------------

reduceHilbert = method()
reduceHilbert Divide := ser -> (
    num := numerator ser;   -- an element of the degrees ring
    if num == 0 then return Divide {num, 1_(ring num)};
    den := denominator ser; -- a Product of Powers
    newden := Product nonnull apply(toList den, pwr -> (
	    fac := pwr#0;   -- 1-T_i
	    ex  := pwr#1;   -- exponent
	    while ex > 0
	    and num % fac == 0 -- this works because of Mike's magic in the engine
	    do (
		num = num // fac;
		ex = ex - 1;
		);
	    if ex > 0 then Power {fac, ex}));
    Divide {num, newden})

-----------------------------------------------------------------------------
-- hilbertSeries
-----------------------------------------------------------------------------

exactKey := "exact hilbertSeries"
reducedKey := "reduced exact hilbertSeries"
approxKey := "approximate hilbertSeries"

hilbertSeries = method(
    Options => {
	Order    => infinity,
	Reduce   => false,
	}
    )

hilbertSeries QuotientRing   :=
hilbertSeries PolynomialRing := opts -> R -> hilbertSeries(module R, opts)

hilbertSeries Ideal  := opts -> I -> hilbertSeries(comodule I, opts)
hilbertSeries Module := opts -> M -> (
    R := ring M;
    hft := heft R;
    if hft === null then error "hilbertSeries: ring has no heft vector";
    ord := opts.Order;
    -- using cached result
    if ord === infinity then (
	if opts.Reduce then (
	    if M.cache#?reducedKey then return M.cache#reducedKey;
	    if M.cache#?exactKey   then return(M.cache#reducedKey = reduceHilbert M.cache#exactKey))
	else if M.cache#?exactKey  then return M.cache#exactKey)
    else if instance(ord, ZZ) then (
	if M.cache#?approxKey then (
	    (ord2, ser) := M.cache#approxKey;
	    if ord == ord2 then return ser else
	    if ord  < ord2 then return part(, ord-1, hft, ser));
	if M.cache#?exactKey or M.cache#?reducedKey then (
	    if not M.cache#?reducedKey then M.cache#reducedKey = reduceHilbert M.cache#exactKey;
	    return last(M.cache#approxKey = (ord, truncateSeries(ord, hft, M.cache#reducedKey))))
	    )
    else error "hilbertSeries: option Order expected infinity or an integer";
    -- computing the hilbert series
    ser = runHooks((hilbertSeries, Module), (opts, M));
    if ser === null   then error("no applicable strategy for computing Hilbert series over ", toString R);
    -- returning the appropriate format
    if ord < infinity then last M.cache#approxKey else
    if opts.Reduce    then M.cache#reducedKey else M.cache#exactKey)

addHook((hilbertSeries, Module), Strategy => Default, (opts, M) -> (
    -- some examples compute degrees of inhomogeneous modules,
    -- so we can't refuse to compute when the module is not homogeneous.
    -- is it guaranteed to work in some sense?
    -- if not isHomogeneous M then error "expected a homogeneous module";
	R := ring M;
	T := degreesRing R;
	hft := heft R;
	ord := opts.Order;
	num := poincare M; -- 'poincare' treats monomial ideals correctly (as the corresponding quotient module)
	deg := tally degrees R.FlatMonoid;
	den := Product apply(sort apply(pairs deg, (i, e) -> {1 - T_i, e}), t -> Power t);
	M.cache#exactKey = ser := Divide {num, den};
	if ord < infinity or opts.Reduce then
	M.cache#reducedKey = reduceHilbert ser;
	if ord < infinity then
	M.cache#approxKey = (ord, truncateSeries(ord, hft, ser)) else ser))

hilbertSeries ProjectiveHilbertPolynomial := opts -> P -> (
    d := max keys P;
    t := (degreesRing 1)_0;
    new Divide from {
	sum apply(pairs P, (n, a) -> a * (1-t)^(d-n)),
	new Power from {1-t, d+1}
	})

-----------------------------------------------------------------------------
-- hilbertFunction
-----------------------------------------------------------------------------

hilbertFunction = method()
hilbertFunction(ZZ, Ring)   :=
hilbertFunction(ZZ, Ideal)  :=
hilbertFunction(ZZ, Module) := (d, M) -> hilbertFunction({d}, M)

hilbertFunction(List, Ring)   := (L, R) -> hilbertFunction(L, module R)
hilbertFunction(List, Ideal)  :=
hilbertFunction(List, Module) := (L, M) -> (
    -- computes the Hilbert series to a sufficiently high order and
    -- returns the desired coefficient, thus it is cached by hilbertSeries
    R := ring M;
    if not all(L, i -> instance(i, ZZ)) then error "hilbertFunction: expected degree to be an integer or list of integers";
    if #L =!= degreeLength R            then error "hilbertFunction: degree length mismatch";
    if heft R === null                  then error "hilbertFunction: ring has no heft vector";
    --
    HF := runHooks((hilbertFunction, List, Module), (L, M));
    if HF =!= null then return HF;
    error("no applicable strategy for computing Hilbert function over ", toString R))

addHook((hilbertFunction, List, Module), Strategy => Default, (L, M) -> (
    h := heft ring M;
    f := hilbertSeries(M, Order => 1 + sum(h, L, times));
    U := monoid ring f;
    coefficient(U_L, f)))
