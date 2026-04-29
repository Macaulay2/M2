witt = method()
unWitt = method()
wittLength = method()
explicit = method()
wittIdeal = method(Dispatch => Thing)
verschiebung = method()
wittFrobenius = method()
makeCoefficientFieldPrime = method()
charPCheck = method()
baseRing' = method()
isFinitePrimeField' = method()


---
--- new unexported methods
---
baseRing'(Ring) := R -> (
    p := char R;
    if R === ZZ/p then return R;
    if class R === GaloisField then return R;
    if class R === PolynomialRing then baseRing R else baseRing'(ambient R)
)

isFinitePrimeField'(Ring) := R -> if isFinitePrimeField R and class R =!= GaloisField then true else false


---
--- WittRingElement
---

WittRingElement = new Type of MutableHashTable;

charPCheck(Ring) :=  R -> (
    if char R == 0 then error "expected a ring of positive characteristic";
    if not isPrime char R then error "expected a ring of prime characteristic";
)

witt(List) := L0 -> (
    ww := new WittRingElement from MutableHashTable;
    --check all elements of the list lie in ZZ or same ring
    L := apply(L0, i -> ring i);
    BaseRing := unique select(L, i-> i =!= ZZ);
    if length (BaseRing) == 0 then error "must specify ring; e.g., use sub";
    if  length (BaseRing) > 1 then error "expected elements from the same ring";
    charPCheck(first BaseRing);
    ww.tuple = apply(L0, i -> sub(i, first BaseRing ));
    for nn from 0 to length L0 - 1 do ww#nn = L0#nn;
    ww
)

net(WittRingElement) := w -> net(w.tuple)

toList(WittRingElement) := w -> w.tuple



ring(WittRingElement) := W -> (
    R := ring(W.tuple#0); -- note that witt subs every entry into R so this is good enough
    n := length W;
    witt(n, R)
)

length WittRingElement := ww -> length ww.tuple

WittRingElement + WittRingElement := (w1, w2) -> (
    if length w1 != length w2 then error "expected vectors of the same length";
    if (ring w1) =!= (ring w2) then error "expected vectors over the same ring";
    w1over := wittTupleToOverring(w1);
    w2over := wittTupleToOverring(w2);
    wittOverringToTuple(w1over + w2over)
)

- WittRingElement := ww -> wittOverringToTuple(-wittTupleToOverring (ww))

WittRingElement - WittRingElement := (w1, w2) -> w1 + (-w2)

ZZ * WittRingElement := (nn, ww) -> wittOverringToTuple(nn*wittTupleToOverring(ww))

WittRingElement * ZZ := (ww, nn) -> nn * ww

WittRingElement * WittRingElement := (w1, w2) -> (
    if length w1 != length w2 then error "expected vectors of the same length";
    if ring w1 =!= ring w2 then error "expected elements of the same ring";
    w1over := wittTupleToOverring(w1);
    w2over := wittTupleToOverring(w2);
    wittOverringToTuple(w1over * w2over)
)

WittRingElement ^ ZZ := (ww, nn) -> (
    wwover := wittTupleToOverring(ww);
    wittOverringToTuple wwover^nn
)

WittRingElement | WittRingElement := (w1, w2) -> witt(w1.tuple | w2.tuple)

WittRingElement == WittRingElement := (w1, w2) -> w1.tuple == w2.tuple

WittRingElement == ZZ := (w1, n) -> (
    w1 == n*witt({1_(unWitt ring w1)} | for i from 0 to wittLength ring w1 - 2 list 0)
)

verschiebung(WittRingElement) := ww -> (
    R := (ring ww).unWitt;
    witt({0_R}|ww.tuple)
)

truncate(ZZ, WittRingElement) :=  {} >> opts -> (n, w) -> (
    if length w<n then error "Can't truncate to something longer";
    witt drop(w.tuple, n-length w)
)



-------------------------------
-------------WittPolynomialRing
-------------------------------

WittPolynomialRing = new Type of MutableHashTable;

protect overring
protect coeffFieldPrime
protect coeffFieldMap


makeCoefficientFieldPrime(GaloisField) := makeCoefficientFieldPrime(PolynomialRing) := R -> R.cache#(coeffFieldPrime) ??= (
    F := baseRing' R;
    if not isField F then error "expected a field as coefficient ring";
    if isFinitePrimeField' F then R else(
	FAmb := newRing(ambient(F), DegreeRank => 0);
        S' := if class R === GaloisField then FAmb else FAmb(monoid R);
        FS := flattenRing S';
        R.cache#coeffFieldMap = map(first FS, R);
        first FS)
)

makeCoefficientFieldPrime(QuotientRing) := R -> R.cache#(coeffFieldPrime) ??= (
    F := baseRing' R;
    if F =!= null and not isField F then error "expected a field as coefficient ring";
    if isFinitePrimeField'(F) then return R;
    S := ambient(R);
    if class(S) =!= PolynomialRing then error "makeCoefficientFieldPrime is only implemented for quotients of polynomial rings. Consider flattening first";
    S' := makeCoefficientFieldPrime(S);
    FR := flattenRing quotient sub(ideal R, S');
    R.cache#coeffFieldMap = map(first FR, R);
    first FR
)

witt(ZZ, PolynomialRing) := (n, R)->(
    charPCheck R;
    F := baseRing' R;
    if n <= 0 then error "witt vectors must have positive length";
    if not R.?cache then R.cache = new CacheTable;
    if not R.cache.?wittRings then R.cache.wittRings = new CacheTable;
    if not R.cache.wittRings#?n then(
	W := new WittPolynomialRing from MutableHashTable;
	W.wittLength = n;
	W.unWitt = R;
        W.overring = wittOverring(n, R);
	R.cache.wittRings#n = W;
    );
    R.cache.wittRings#n
)


unWitt(WittPolynomialRing) := WPR -> WPR.unWitt

net(WittPolynomialRing) := WPR -> horizontalJoin("Witt", (net(WPR.wittLength))^-1, "(", net WPR.unWitt, ")")

explicit(WittPolynomialRing) := (cacheValue (symbol explicit)) (WPR->wittVectors(WPR.wittLength, WPR.unWitt))



-------------------------------
------------- WittQuotientRing
-------------------------------

WittQuotientRing = new Type of MutableHashTable;

witt(ZZ, GaloisField) :=
witt(ZZ, QuotientRing) := (n, R)->(
    F := baseRing' R;
    if n <= 0 then error "witt vectors must have positive length";

    if not R.?cache then R.cache = new CacheTable;
    if not R.cache.?wittRings then R.cache.wittRings = new CacheTable;
    if not R.cache.wittRings#?n then(
	W := if isFinitePrimeField' R or class R === GaloisField then new WittPolynomialRing from MutableHashTable else new WittQuotientRing from MutableHashTable;
	W.wittLength = n;
	W.unWitt = R;
	W.overring = wittOverring(n, R);
	R.cache.wittRings#n = W;
    );
    R.cache.wittRings#n
)

unWitt(WittQuotientRing) := WQR -> WQR.unWitt

wittLength(WittQuotientRing) := W -> W.wittLength
wittLength(WittPolynomialRing) := W -> W.wittLength

net(WittQuotientRing) := WQR -> horizontalJoin("Witt", (net(WQR.wittLength))^-1, "(", net WQR.unWitt, ")")


explicit(WittQuotientRing) := (cacheValue (symbol explicit)) (WPR->wittVectors(WPR.wittLength, WPR.unWitt))


------
------
------

random(ZZ, WittPolynomialRing) :=
random(ZZ, WittQuotientRing) := opts -> (nn, WPR) -> (
    R := WPR.unWitt;
    ll := WPR.wittLength;
    witt apply(toList(1..ll), xx -> random(nn, R))
)

sub(ZZ, WittPolynomialRing) :=
sub(ZZ, WittQuotientRing) := (nn, WR) -> (
    if nn === 1 then(
	ll := wittLength(WR);
	R := unWitt(WR);
	zeroes := toList apply( 1..(ll-1), xx -> 0_R );
	return witt( {1_R} | zeroes)
    ) else(
	return nn*sub(1, WR)
	);
)

ZZ_WittPolynomialRing := ZZ_WittQuotientRing := (nn, WR) -> sub(nn, WR)

-------------
------------- WittIdeal
-------------

WittIdeal = new Type of MutableHashTable;

protect wittGenerators

wittIdeal(WittRingElement) := ww -> new WittIdeal from {wittGenerators => toSequence{ww}}

wittIdeal List := wittIdeal Sequence := LL -> (
    if not all(LL, ll -> class(ll) === WittRingElement) then error "the suggested generators are not WittRingElement";
    if not length unique apply(LL, ll -> length(ll)) == 1 then error "the generators do not have the same length";
    new WittIdeal from {wittGenerators => toSequence(LL)}
)


explicit(WittIdeal) := I -> (
    if not I.?explicit then(
	Igens := I.wittGenerators;
	Igensover := apply(Igens, wittTupleToOverring);
	I.explicit = ideal(Igensover);
    );
    I.explicit
)

trim (WittIdeal) := opts -> I -> (
    Iexp := trim explicit(I);
    ggs := apply( flatten entries gens Iexp, wittOverringToTuple);
    wittIdeal ggs
)

generators (WittIdeal) := opts -> I -> toList I.wittGenerators


WittIdeal == WittIdeal := (I, J) -> explicit(I) == explicit(J)

--- addition and multiplication

WittIdeal + WittIdeal := (I, J) -> wittIdeal(I.wittGenerators | J.wittGenerators)

WittIdeal * WittIdeal := (I, J) -> wittIdeal apply( toList(I.wittGenerators)**toList(J.wittGenerators), (x,y) -> x*y)

WittIdeal ^ ZZ := (I, nn) -> if nn == 1 then I else trim( I*I^(nn-1))

--WittIdeal display

addCommas := LL -> (
    nn := #LL;
    out := ();
    for jj in 0..(2*nn-2) do(
	if jj % 2 == 0 then(
	    out = append(out, LL#(jj//2))
	    ) else (
	    out = append(out, ", ");
	    );
	);
    out
)

net WittIdeal := WI -> (
    wgs := WI.wittGenerators;
    if #wgs == 1 then( horizontalJoin("ideal ", net (wgs#0))) else (
	wgsnet := apply( wgs, net );
	wgsnet = addCommas(wgsnet);
	horizontalJoin("ideal (", wgsnet, ")"  )
        )
)


----------------------------------
---------------- WittRingMap
---------------------------------


WittRingMap = new Type of MutableHashTable;

net(WittRingMap) := Wf-> horizontalJoin("WittRingMap ", net(Wf.target), " <-- ", net(Wf.source))

witt(ZZ, ZZ , RingMap) := WittRingMap => (mm, nn, ff) -> (
    if mm > nn then error "wittLength of target is bigger than wittLength of source";
    if mm <= 0 or nn<=0 then error "witt vectors must have positive length";
    R := source ff;
    S := target ff;
    WR := witt(nn, R);
    WS := witt(mm, S);
    Wf := new WittRingMap from MutableHashTable;
    Wf.source = WR;
    Wf.target = WS;
    Wf.baseMap = ff;
    Wf
)

witt(ZZ, RingMap) := WittRingMap => (n, f) -> witt(n, n, f)

WittRingMap * WittRingMap :=  WittRingMap => (gg, ff) -> (
    if source gg =!= target ff then error "the WittRingMaps given are not composable";
    witt( target(gg).wittLength, source(ff).wittLength, ff.baseMap*gg.baseMap)
)

explicit(WittRingMap) := Wf -> (
    Wse := explicit source Wf;
    Wte := explicit target Wf;
    l := wittLength target Wf;
    WteTuples := for i in gens Wse list witt apply( (wittRingToTuple i).tuple, j->(baseMap Wf)(j));
    mapList := for i in WteTuples list wittTupleToRing truncate(l, i);
    map(Wte, Wse, mapList)
)

WittRingMap ^ ZZ := (Wf, mm) -> (
    if source Wf =!= target Wf then error "the WittRingMaps are not composable";
    f := baseMap(Wf);
    ll := wittLength source Wf;
    fm := f^mm;
    witt(ll, fm)
)

target(WittRingMap) := W -> W.target
source(WittRingMap) := W -> W.source

baseMap = method()
baseMap(WittRingMap) := W -> W.baseMap

WittRingMap WittRingElement := WittRingElement => (Wf, w) -> (
    if ring w =!= source(Wf) then(
	error "the input is not an element of the source";
    );
    f := baseMap(Wf);
    outputLength := wittLength(target(Wf));
    wList := toList(w);
    outputList := toList apply(0..outputLength-1, xx -> f( w#xx ) );
    witt(outputList)
)

---

wittFrobenius(WittQuotientRing) := WittRingMap => WPR -> (
    R := WPR.unWitt;
    nn := wittLength(WPR);
    pp := char (R);
    Rvars := gens R;
    Rvarsp := apply(Rvars, xx -> xx^pp);
    frob := map(R, R, Rvarsp);
    witt(nn, frob)
)


wittFrobenius(WittPolynomialRing) := WittRingMap => WPR -> (
    R := WPR.unWitt;
    nn := wittLength(WPR);
    pp := char (R);
    Rvars := gens R;
    Rvarsp := apply(Rvars, xx -> xx^pp);
    frob := map(R, R, Rvarsp);
    witt(nn, frob)
)

wittFrobenius(ZZ, Ring) := WittRingMap => (n, R) -> wittFrobenius(witt(n, R))

wittFrobenius(WittRingElement) := WittRingElement => ww -> (
    wF := wittFrobenius(ring(ww));
    wF(ww)
)

truncate(ZZ, WittPolynomialRing) := {} >> opts -> (n, W) -> (
    if n > wittLength W then error "can't truncate to something longer";
    witt(n, wittLength W, map(unWitt W, unWitt W))
)

truncate(ZZ, WittQuotientRing) := {} >> opts -> (n, W) ->  (
    if n > wittLength W then error "can't truncate to something longer";
    witt(n, wittLength W, map(unWitt W, unWitt W))
)
