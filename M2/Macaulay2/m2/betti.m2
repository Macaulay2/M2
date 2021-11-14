-- moved from chaincomplexes.m2

-* TODO
- https://github.com/Macaulay2/M2/issues/1976
- https://github.com/Macaulay2/M2/issues/647
- https://github.com/Macaulay2/M2/issues/2159
*-

needs "gb.m2" -- for GroebnerBasis
needs "chaincomplexes.m2"
needs "gradedmodules.m2"
needs "modules2.m2"

regularity = method( TypicalValue => ZZ, Options => { Weights => null } )
regularity ChainComplex := opts -> C -> regularity betti(C,opts)
regularity Module := opts -> (M) -> (
    if not isHomogeneous M then error "regularity: expected homogeneous module";
    regularity betti(resolution minimalPresentation M,opts))
regularity Ideal := opts -> (I) -> (
    if I == 0 then -infinity
    else if I == 1 then 0
    else 1 + regularity betti(resolution cokernel generators I,opts))

BettiTally = new Type of VirtualTally
BettiTally.synonym = "Betti tally"
BettiTally == BettiTally := (C,D) -> C === D
BettiTally ++ BettiTally := (C,D) -> merge(C,D,plus)
BettiTally ** BettiTally := (C,D) -> combine(C,D,(j,k)->apply(j,k,plus),times,plus)
BettiTally ZZ := (C,n) -> applyKeys(C, (i,d,h) -> (i,d,h-n))
dual BettiTally := {} >> o -> (C) -> applyKeys(C,j -> apply(j,minus))
regularity BettiTally := opts -> (C) -> (
    if opts.Weights =!= null then C = betti(C,opts);
    max apply(keys C, (i,d,h) -> h-i))
BettiTally Array := (C,A) -> (
    if # A =!= 1 then error "expected array of length 1";
    n := A#0;
    applyKeys(C,(i,d,h) -> (i-n,d,h)))

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
    leftside := splice {"", "total:", apply(minrow .. maxrow, i -> toString i | ":")};
    totals := apply(transpose v, sum);
    v = prepend(totals,v);
    v = applyTable(v, bt -> if bt === 0 then "." else toString bt);
    v = prepend(toString \ toList (mincol .. maxcol), v);
    v = apply(leftside,v,prepend);
    v)

net BettiTally := v -> netList(rawBettiTally v, Alignment => Right, HorizontalSpace => 1, BaseRow => 1, Boxes => false)
texMath BettiTally := v -> (
    v = rawBettiTally v;
    concatenate(
	"\\begin{matrix}\n",
	apply(v, row -> (between("&", apply(row,x->if not match("^[0-9]*$",x) then ("\\text{",x,"}") else x)), "\\\\")),
	"\\end{matrix}\n",
	))

-- local function for selecting and computing the appropriate heft
heftfun0 := wt -> d -> sum( min(#wt, #d), i -> wt#i * d#i )
heftfun := (wt1,wt2) -> (
    if wt1 =!= null then heftfun0 wt1
    else if wt2 =!= null then heftfun0 wt2
    else d -> 0
    )

betti = method(TypicalValue => BettiTally, Options => { Weights => null, Minimize => false })
betti BettiTally := opts -> t -> if opts.Weights === null then t else (
    heftfn := heftfun0 opts.Weights;
    applyKeys(t, (i,d,h) -> (i,d,heftfn d)))
betti Matrix := opts -> f -> betti(chainComplex f, opts)
betti GroebnerBasis := opts -> G -> betti(generators G, opts)
betti Ideal := opts -> I -> betti(generators I, opts)
betti Module := opts -> M -> betti(presentation M, opts)

unpackEngineBetti = (w) -> (
    -- w is the result of e.g. rawGBBetti.
    -- this is an array of ints, of the form:
    -- [lodegree, hidegree, len, b(lodegree,0), b(lodegree,1), ..., b(lodegree,len), ... b(hidegree,len)]
    lo := w#0;
    hi := w#1;
    len := w#2;
    w = drop(w,3);
    w = pack(len+1,w);
    w = table(lo .. hi, 0 .. len, (i,j) -> (j,{i+j},i+j) => w#(i-lo)#j); -- no weight option used here
    w = toList splice w;
    w = select(w, option -> option#1 != 0);
    new BettiTally from w)

rawBetti = (computation, type) -> (
    w := rawGBBetti(computation, type);
    lo := w#0;
    hi := w#1;
    len := w#2;
    w = drop(w,3);
    w = pack(len+1,w);
    w = table(lo .. hi, 0 .. len, (i,j) -> (j,{i+j},i+j) => w#(i-lo)#j); -- no weight option used here
    w = toList splice w;
    w = select(w, option -> option#1 != 0);
    new BettiTally from w)

undocumented' (betti,Resolution)
betti Resolution := opts -> X -> (
    -- this version works only for rings of degree length 1
    -- currently if opts.Minimize is true, then an error is given
    -- unless the FastNonminimal=>true option was given for the free resolution.
    b := rawBetti(X.RawComputation, if opts.Minimize then 4 else 0); -- the raw version takes no weight option
    heftfn := heftfun(opts.Weights,heft ring X);
    b = applyKeys(b, (i,d,h) -> (i,d,heftfn d));
    b)

minimalBetti = method(Options => true)
minimalBetti Ideal :=
minimalBetti Module := {
    DegreeLimit => null,
    LengthLimit => null,
    Weights => null
    } >> opts -> (I) -> (
    C := if opts.LengthLimit === null then
    resolution(I, StopBeforeComputation=>true, FastNonminimal=>true)
    else
    resolution(I, StopBeforeComputation=>true, FastNonminimal=>true, LengthLimit=>opts.LengthLimit+1);
    if not C.?Resolution or not C.Resolution.?RawComputation then
    error "cannot use 'minimalBetti' with this input.
    Input must be an ideal or module in a polynomial
    ring or skew commutative polynomial ring over
    a finite field, which is singly graded.
    These restrictions might be removed in the future.";
    rawC := C.Resolution.RawComputation;
    w := rawMinimalBetti(rawC,
	if opts.DegreeLimit =!= null then {opts.DegreeLimit} else {},
	if opts.LengthLimit =!= null then {opts.LengthLimit} else {}
	);
    b := unpackEngineBetti w;
    -- The following code is lifted directly from 'betti Resolution'
    heftfn := heftfun(opts.Weights,heft ring C.Resolution);
    b = applyKeys(b, (i,d,h) -> (i,d,heftfn d));
    b
    )

betti GradedModule := opts -> C -> (
    if C.?Resolution and degreeLength ring C === 1 and heft ring C === {1} then betti(C.Resolution,opts)
    else (
	if opts.Minimize then error "Minimize=>true is currently only supported for res(...,FastNonminimal=>true)";
	complete C;
	heftfn := heftfun(opts.Weights, heft ring C);
	new BettiTally from flatten apply(
	    select(pairs C, (i,F) -> class i === ZZ),
	    (i,F) -> (
		if not isFreeModule F then error("betti: expected module at spot ", toString i, " in chain complex to be free");
		apply(pairs tally degrees F, (d,n) -> (i,d,heftfn d) => n)))))

-----------------------------------------------------------------------------
MultigradedBettiTally = new Type of BettiTally
MultigradedBettiTally.synonym = "multigraded Betti tally"
MultigradedBettiTally List := (B,l) -> applyKeys(B, (i,d,h) -> (i,d-l,h))

-- Helper function for pretty-printing the hash table
rawMultigradedBettiTally = B -> (
    if keys B == {} then return 0;
    N := max apply(pairs B, (key, n) -> ((i,d,h) := key; length d));
    R := ZZ[vars(0..N-1)];
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
		s := toString n | ":" | toString d;
		if H#?i then H#i = H#i | {s} else H#i = {s};
		);
	    ));
    (rows, cols) = (sort unique rows, sort unique cols);
    if compactMatrixForm then (
	T := table(toList (0 .. length rows - 1), toList (0 .. length cols - 1),
	    (i,j) -> if H#?(rows#i,cols#j) then H#(rows#i,cols#j) else 0);
	-- Making the table
	xAxis := toString \ cols;
	yAxis := (i -> toString i | ":") \ rows;
	T = applyTable(T, n -> if n === 0 then "." else toString raw n);
	T = prepend(xAxis, T);
	T = apply(prepend("", yAxis), T, prepend);
	) else (
	T = table(max((keys H)/(j -> #H#j)), sort keys H,
	    (i,k) -> if i < #H#k then H#k#i else null);
	T = prepend(sort keys H,T);
	);
    T
    )

net MultigradedBettiTally := B -> netList(rawMultigradedBettiTally B, Alignment => Right, HorizontalSpace => 1, BaseRow => 1, Boxes => false)

-- Converts a BettiTally into a MultigradedBettiTally, which supports better pretty-printing
-- Note: to compactify the pretty-printed output, set compactMatrixForm to false.
multigraded = method(TypicalValue => MultigradedBettiTally)
multigraded BettiTally := bt -> new MultigradedBettiTally from bt

-----------------------------------------------------------------------------
-- some extra betti tally routines by David Eisenbud and Mike :
lift(BettiTally, ZZ) := opts -> (B,R) -> applyValues(B, v -> lift(v,ZZ))
QQ * BettiTally := (d,B) -> applyValues(B, v -> d*v)
ZZ * BettiTally := (d,B) -> applyValues(B, v -> d*v)
pdim BettiTally := (B) -> max apply ((keys B), i->i_0)
poincare BettiTally := (B) -> (
    if #B === 0 then return 0;				    -- yes, it's not in a degree ring, but that should be okay
    K := keys B;
    R := degreesRing (#K#0#1);				    -- it doesn't matter which key we inspect
    sum apply(K, k -> (-1)^(k#0) * B#k * R_(k#1)))
hilbertPolynomial(ZZ,BettiTally) := o -> (nvars,B) -> (
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
degree BettiTally := (B) -> (
    f := poincare B;
    if f === 0 then return 0;
    T := (ring f)_0;
    while f % (1-T) == 0 do f = f//(1-T);
    substitute(f, T=>1))
codim BettiTally := {} >> opts -> (B) -> (
    f := poincare B;
    if f === 0 then return infinity;
    T := (ring f)_0;
    c := 0;
    while f % (1-T) == 0 do (c = c+1; f = f//(1-T));
    c)
hilbertSeries(ZZ,BettiTally) := o -> (n,B) -> (
    num := poincare B;
    if num === 0 then return 0;
    T := (ring num)_0;
    denom := Product{Power{(1-T),n}};
    Divide{num, denom})
-----------------------------------------------------------------------------
Ring ^ BettiTally := (R,b) -> (
    -- donated by Hans-Christian von Bothmer
    -- given a betti Table b and a Ring R make a chainComplex -- with zero maps over R  that has betti diagram b. --
    -- negative entries are ignored
    -- rational entries produce an error
    -- multigraded R's work only if the betti Tally contains degrees of the correct degree length
    F := new ChainComplex;
    F.ring = R;
    scan(sort pairs b, (k,n) -> (
	    (i,deg,wt) := k;   -- the keys of a betti table have the form (homological degree, multidegree, weight)
	    -- use F_i since it gives 0 if F#i is not there:
	    F#i = F_i ++ R^{n:-deg};		    -- this could be a bit slow
	    ));
    F)
