-- moved from chaincomplexes.m2

-* TODO
- https://github.com/Macaulay2/M2/issues/647
- https://github.com/Macaulay2/M2/issues/2159
*-

needs "gb.m2" -- for GroebnerBasis
needs "hilbert.m2"
needs "modules2.m2"

-----------------------------------------------------------------------------
-- unexported helper functions used in several packages
-----------------------------------------------------------------------------

unpackEngineBetti = w -> (
    -- w is the result of e.g. rawGBBetti.
    -- this is an array of ints, of the form:
    -- [lodegree, hidegree, len, b(lodegree,0), b(lodegree,1), ..., b(lodegree,len), ... b(hidegree,len)]
    (lo, hi, len) := (w#0, w#1, w#2);
    w = pack(len+1, drop(w, 3));
    w = flatten table(toList(lo .. hi), toList(0 .. len),
	(i,j) -> ((j, {i+j}, i+j), w#(i-lo)#j)); -- no weight option used here
    new BettiTally from select(w, (k,v) -> v != 0))

-- used in EngineTests
rawBetti = (computation, type) -> unpackEngineBetti rawGBBetti(computation, type)

-----------------------------------------------------------------------------
-- BettiTally type declarations and basic constructors
-----------------------------------------------------------------------------

BettiTally = new Type of VirtualTally
BettiTally.synonym = "Betti tally"

BettiTally == BettiTally := Boolean => (C, D) -> C === D
BettiTally ++ BettiTally := BettiTally => (C, D) -> merge(C, D, plus)
BettiTally ** BettiTally := BettiTally => (C, D) -> combine(C, D, (j, k) -> apply(j, k, plus), times, plus)

dual BettiTally := BettiTally => {} >> o -> C -> applyKeys(C, j -> apply(j, minus))

-- homologically shift the tally
BettiTally Array := BettiTally => (B, A) -> (
    n := if #A == 1 then A#0 else error "expected array of length 1";
    applyKeys(B, (i,d,h) -> (i-n,d,h)))

-- shift the tally by total weight
BettiTally ZZ := BettiTally => (B, n) -> applyKeys(B, (i,d,h) -> (i,d,h-n))

-- some extra routines by David Eisenbud and Mike:
lift(BettiTally, ZZ) := BettiTally => opts -> (B, ZZ) -> applyValues(B, v -> lift(v, ZZ))
QQ * BettiTally :=
ZZ * BettiTally := BettiTally => (d, B) -> applyValues(B, v -> d*v)

-----------------------------------------------------------------------------
-- MultigradedBettiTally type declarations and basic constructors
-----------------------------------------------------------------------------

MultigradedBettiTally = new Type of BettiTally
MultigradedBettiTally.synonym = "multigraded Betti tally"

-- Note: set compactMatrixForm to false for more compact output
multigraded = method()
multigraded BettiTally := MultigradedBettiTally => bt -> new MultigradedBettiTally from bt

-- shift the multigraded tally by multidegrees
-- note: this is subtly different from BettiTally ZZ
-- see https://github.com/Macaulay2/M2/issues/2303
MultigradedBettiTally List := (B, l) -> applyKeys(B, (i,d,h) -> (i,d-l,h))
MultigradedBettiTally Sequence := (B, a) -> B toList a

-----------------------------------------------------------------------------
-- functions for pretty-printing the internal representation of Betti tables
-----------------------------------------------------------------------------

rawBettiTally = v -> (
    v' := new MutableHashTable;
    scan(pairs v, (key,n) -> (
	    (i,d,h) := key;
	    if h === null then h = d#0; -- the raw version may produce no heft here
	    h = h-i;				       -- skew in the usual way
	    key = (h,i);
	    if v'#?key then v'#key = v'#key + n else v'#key = n;
	    ));
    v = v';
    k := keys v;
    fi := first \ k;
    la := (s -> s#1) \ k;
    mincol := min la;
    maxcol := max la;
    minrow := min fi;
    maxrow := max fi;
    v = table(toList (minrow .. maxrow), toList (mincol .. maxcol), (i,j) -> if v#?(i,j) then v#(i,j) else 0);
    leftside := splice {, "total:", apply(minrow .. maxrow, i -> RowExpression{ i, symbol :} )};
    totals := apply(transpose v, sum);
    v = prepend(totals,v);
    v = applyTable(v, bt -> if bt === 0 then symbol . else bt);
    v = prepend(toList (mincol .. maxcol), v);
    v = apply(leftside,v,prepend);
    v)

rawMultigradedBettiTally = B -> (
    if keys B == {} then return 0;
    N := max apply(pairs B, (key, n) -> ((i,d,h) := key; length d));
    R := ZZ(monoid[vars(0..N-1), MonomialOrder => Lex, Inverses => true]);
    H := new MutableHashTable;
    (rows, cols) := ({}, {});
    scan(pairs B,
	(key, n) -> (
	    (i,d,h) := key;
	    key = (h, i);
	    (rows, cols) = (append(rows, h), append(cols, i));
	    if compactMatrixForm then (
		m := n * R_d;
		if H#?key then H#key = H#key + m else H#key = m;
		) else (
		s := hold n : d;
		if H#?i then H#i = H#i | {s} else H#i = {s};
		);
	    ));
    (rows, cols) = (sort unique rows, sort unique cols);
    if compactMatrixForm then (
	T := table(toList (0 .. length rows - 1), toList (0 .. length cols - 1),
	    (i,j) -> if H#?(rows#i,cols#j) then H#(rows#i,cols#j) else 0);
	-- Making the table
	xAxis := cols;
	yAxis := (i -> RowExpression{ i, symbol :}) \ rows;
--	T = applyTable(T, n -> if n === 0 then symbol . else raw n);
	T = applyTable(T, n -> if n === 0 then symbol . else n);
	T = prepend(xAxis, T);
	T = apply(prepend(, yAxis), T, prepend);
	) else (
	T = table(max((keys H)/(j -> #H#j)), sort keys H,
	    (i,k) -> if i < #H#k then H#k#i);
	T = prepend(sort keys H, T);
	);
    T)

toStringn := x -> if x===null then "" else toString x
net            BettiTally := B -> netList(rawBettiTally B,            Alignment => Right, HorizontalSpace => 1, BaseRow => 1, Boxes => false)
net MultigradedBettiTally := B -> netList(applyTable(rawMultigradedBettiTally B,toStringn), Alignment => Right, HorizontalSpace => 1, BaseRow => 1, Boxes => false)

texMathn := method()
texMathn Nothing := x -> ""
texMathn String := s -> "\\text{"|s|"}" -- minor variation, no tt
texMathn Thing := texMath

texMath BettiTally := v -> (
    v = rawBettiTally v;
--    v = join({prepend("  ", drop(v#0, 1)), prepend("\\text{total:}\n  ", drop(v#1, 1))}, drop(v, 2));
    v = between("\\\\\n", apply(v, row -> concatenate between(" & ", apply(row,texMathn))));
    concatenate("\\begin{matrix}", newline, v, newline, "\\end{matrix}"))
texMath MultigradedBettiTally := v -> (
    v = rawMultigradedBettiTally v;
    v = if compactMatrixForm then prepend(prepend(, drop(v#0, 1)), drop(v, 1)) else v;
    v = between("\\\\\n", apply(v, row -> concatenate between(" & ", apply(row,texMathn))));
    concatenate("\\begin{matrix}", newline, v, newline, "\\end{matrix}"))

-----------------------------------------------------------------------------
-- betti
-----------------------------------------------------------------------------

-- local function for selecting and computing the appropriate heft
heftfun := wt -> d -> sum( min(#wt, #d), i -> wt#i * d#i )
--heftvec := (wt1, wt2) -> if wt1 =!= null then wt1 else if wt2 =!= null then wt2 else {}

-- betti(Matrix) is defined in OldChainComplexes now
betti = method(TypicalValue => BettiTally, Options => { Weights => null, Minimize => false })
betti GroebnerBasis := opts -> G -> betti(generators G, opts)
betti Ideal         := opts -> I -> betti(generators I, opts)
betti Module        := opts -> M -> betti(presentation M, opts)
betti Matrix        := opts -> f -> missingPackage "either Complexes or OldChainComplexes"
betti BettiTally    := opts -> B -> if opts.Weights === null then B else (
    heftfn := heftfun opts.Weights;
    applyKeys(B, (i,d,h) -> (i,d,heftfn d)))


minimalBetti = method(
    TypicalValue => BettiTally,
    Options => {
	DegreeLimit         => null,
	LengthLimit         => infinity,
	ParallelizeByDegree => false, -- currently: only used over primes fields of positive characteristic
	Weights             => null,
    })

minimalBetti Ideal  :=
minimalBetti Module := o -> M -> missingPackage "either Complexes or OldChainComplexes"

-----------------------------------------------------------------------------

-- TODO: implement the following for MultigradedBettiTally
poincare BettiTally := B -> (
    if #B === 0 then return 0;				    -- yes, it's not in a degree ring, but that should be okay
    K := keys B;
    R := degreesRing (#K#0#1);				    -- it doesn't matter which key we inspect
    sum apply(K, k -> (-1)^(k#0) * B#k * R_(k#1)))
hilbertPolynomial(ZZ, BettiTally) := o -> (nvars, B) -> (
    f := poincare B;
    if f == 0 then return (
	if o.Projective
	then new ProjectiveHilbertPolynomial from {}
	else 0_(hilbertFunctionRing())
	);
    T := (ring f)_0;
    p := pairs standardForm f;
    n := nvars - 1;
    if o.Projective
    then sum(p, (d,c) -> (
	    if #d === 0 then d = 0 else d = d#0;
	    c * projectiveHilbertPolynomial(n,-d)))
    else sum(p, (d,c) -> (
	    if #d === 0 then d = 0 else d = d#0;
	    c * hilbertFunctionQ(n,-d))))
degree BettiTally := B -> (
    f := poincare B;
    if f === 0 then return 0;
    T := (ring f)_0;
    while f % (1-T) == 0 do f = f//(1-T);
    substitute(f, T=>1))
codim BettiTally := {} >> opts -> B -> (
    f := poincare B;
    if f === 0 then return infinity;
    T := (ring f)_0;
    c := 0;
    while f % (1-T) == 0 do (c = c+1; f = f//(1-T));
    c)
hilbertSeries(ZZ, BettiTally) := o -> (n,B) -> (
    num := poincare B;
    if num === 0 then return 0;
    T := (ring num)_0;
    denom := Product{Power{(1-T),n}};
    Divide{num, denom})

-----------------------------------------------------------------------------
-- pdim and regularity
-----------------------------------------------------------------------------

nonzeroKeys = x -> select(keys x, k -> x#k != 0)

pdim Module     := M -> missingPackage "either Complexes or OldChainComplexes"
pdim BettiTally := B -> if #(s := first \ nonzeroKeys B) > 0 then max s - min s else 0

regularity = method(TypicalValue => ZZ, Options => { Weights => null })
regularity Ideal      :=
regularity Module     := o -> M -> missingPackage "either Complexes or OldChainComplexes"
regularity BettiTally := o -> B -> max apply(nonzeroKeys betti(B, o), (i, d, h) -> h-i)
