wittOverring = method()
wittTupleToOverring = method()
wittVectors = method()
wittTupleToRing = method()
wittRingToTuple = method()
wittOverringToTuple = method()
wittRingIdeal = method()
wittOverringIdeal = method()

export{"WittOverring"}
protect symbol overringRep
-----------------------------
-----------------------------
-----------------------------

-- NB: The wittOverring of a quotient ring is (a copy of) the wittOverring of the ambient ring

wittOverring(ZZ, Ring) := (n, R) -> R.cache#(symbol WittOverring, n) ??= (
    if class R === GaloisField then(
        S := ambient ambient R;
        OS := wittOverring(n, S);
	OSvars := flatten entries vars OS;
	WittSub := map(OS, makeCoefficientFieldPrime R, OSvars); -- WARNING: not a real map!
	OS.cache.wittSub = WittSub;
	OS.cache.unWitt = R;
        R.cache#(symbol WittOverring, n) = OS;
	return(OS)
	);
    if class R =!= PolynomialRing and not isField(R) then(
        S = ambient R;
        if class S =!= PolynomialRing then(
	    error "wittVectors currently only implemented for quotients of polynomial rings; consider flattening before applying witt"
	    );
	OS = wittOverring(n, newRing S);
	WittSub = map(OS, makeCoefficientFieldPrime R, OS.cache.wittSub); -- WARNING: not a real map!
	OS.cache.wittSub = WittSub;
	OS.cache.unWitt = R;
        R.cache#(symbol WittOverring, n) = OS;
	return(OS)
	);
    if class R === PolynomialRing or isField(R) then(
        R' := makeCoefficientFieldPrime R;
        phi := if R' === R then id_R else R.cache.coeffFieldMap;
        Rvars := flatten entries vars R';
        p := char R;
        d := length Rvars;
        -- we create the WittOverring; called so because the n-th Witt ring of R
        -- will be a subring of this WittOverring.
        T := symbol T;
	OR := ZZ[T_1 .. T_d] / p^n;
	OR.cache = new CacheTable;
	ORvars := flatten entries vars OR;
	WittSub = map(OR, R', ORvars); -- WARNING: this is not a "real" map!
	OR.cache.wittSub = WittSub;
	OR.cache.unWitt = R;
        R.cache#(symbol WittOverring, n) = OR;
	return(OR)
	);
)

wittOverring(WittPolynomialRing) := WR -> (
    WR.overring
)

wittOverring(WittQuotientRing) := WS -> (
    WS.overring
)

-----------------------------
-----------------------------
-----------------------------

wittTupleToOverring(WittRingElement) := (cacheValue (symbol overringRep)) (w -> (
    W := ring w;
    R := W.unWitt;
    n := W.wittLength;
    p := char R;
    OR := W.overring;
    WittSub := OR.cache.wittSub;
    R' := makeCoefficientFieldPrime R;
    phi := if R' === R then id_R else R.cache.coeffFieldMap;
    WittLL := apply(toList(w), ff -> WittSub(phi(ff)));
    sum toList apply(0..(n-1), j -> p^j*(WittLL#j)^(p^(n-1-j)) )
))

-----------------------------
-----------------------------
-----------------------------

wittVectors(ZZ, Ring):=(n, R)->(
    --check if R is a quotient ring that is not a field or has GaloisField base ring
    if (class R === QuotientRing and not isField R) or class baseRing' R === GaloisField then(
        I := ideal makeCoefficientFieldPrime R;
        WS := wittVectors(n, ring I);
        Phi0 := WS.cache.overringMap;
        iso := map(wittOverring(n, R), target Phi0, gens wittOverring(n, R));
        WR := first flattenRing quotient wittRingIdeal(n, I);
        Phi := map(target iso, WR, iso*Phi0);
        WR.cache.overringMap = Phi;
        WR.cache.unWitt = R;
        return WR
    );
    --
    p := char R;
    d := numgens R; -- number of variables
    baseVariables := apply(for i from 0 to d-1 list insert(i, 1, toList(d-1:0)), j->{0}|{j});
    -- cubes is the list of indices;
    -- T_{n,{a_1..a_d}} corresponds to p^n * x_1^{a_1/p^n}..x_d^{a_n/p^n}
    cubesExps := if d > 0 then flatten for i from 1 to n-1 list apply(flatten \ entries \ latticePoints hypercube(d, 0, p^(i) - 1), j->{i}|{j}) else {};
    cubes := try baseVariables | sort select(cubesExps, i-> last i != toList(d:0));
    if class cubes === Nothing then cubes = baseVariables;
    T := symbol T;
    A := ZZ[for i in cubes list T_i]/p^n;
    t := symbol t;
    --t_i is x_i^(1/p^n)
    wittR := witt(n, R);
    B := wittR.overring;
    L := for i in cubes list p^(first i) * (
        product for j from 0 to d - 1 list B_j^((last i)_j * p^(n - first i - 1)) 
    );
    aA := ambient A;
    iA := ideal A;
    K := kernelZZ map(B, A, L);
    aK := sub(K, aA);
    WR  = quotient (iA + aK);
    Phi = map(B, WR, L);
    --cache overringMap and unWitt
    WR.cache.overringMap = Phi;
    WR.cache.unWitt = R;
    WR
)

-----------------------------
-----------------------------
-----------------------------

wittVectors(ZZ, RingMap) := (n, f)->(
    (WT, WS) := (wittVectors(n, target f), wittVectors(n, source f));
    L := for i in gens WS list wittTupleToRing witt(f \ (wittRingToTuple(i)));
    map(WT, WS, L)
)

-----------------------------
-----------------------------
-----------------------------

wittTupleToRing(WittRingElement):= w-> (
    W := ring w;
    n := W.wittLength;
    R := W.unWitt;
    p := char R;
    WR := explicit W;
    Phi := WR.cache.overringMap;
    if (not isField R and class R === QuotientRing) or class baseRing' R === GaloisField then(
        IOR := wittOverringIdeal(n, ideal makeCoefficientFieldPrime R);
        subIOR := (map(target Phi, ring IOR, gens target Phi))(IOR);
        piI := map(quotient subIOR, target Phi);
        Phi = piI * Phi;
    );
    G := wittTupleToOverring(w);
    if G == 0 then return 0_WR;
    Gcons := sum select(terms G, i-> degree i == {0});
    --the following is an attempt to find the unique preimage of G in the Witt ring
    G = G - Gcons;
    Grem := if G == 0 then 0 else (
	    (B, pi) := flattenRing quotient ideal G;
            subMap := map (source pi, target Phi, gens source pi);
            L := subMap \ Phi \ gens source Phi;
            Phi' := map(source pi, source Phi, L);
	    preimages := (kernelZZ(pi*Phi'))_*;
	    multiplied := unique flatten for i in preimages list for j from 0 to p^n - 1 list {j*i, j*Phi i};
	    Preim := first \ select(multiplied, i->(last i) == G);
            if length Preim == 1 then first Preim else error "no preimage found"
	    );
    sub(Gcons, source Phi) + Grem
)


-----------------------------
-----------------------------
-----------------------------

wittRingToTuple(RingElement) := (F)->(
    WR := ring F;
    Phi := WR.cache.overringMap;
    wittOverringToTuple(Phi(F))
)

-----------------------------
-----------------------------
-----------------------------

takeRoot := (f, n) -> (
    --- in a ring of char p , takes the (1/p^n) root of a polynomial f
    R0 := ring f;
    if isFinitePrimeField'( baseRing' ring f) then( R := ring f; phi := id_R)
        else( R = makeCoefficientFieldPrime ring f; phi = R0.cache#coeffFieldMap);
    p := char R;
    d := numgens R;
    S := if class R === QuotientRing then ambient R else R;
    yy := symbol yy;
    SY := (SY0 := S[yy_0 .. yy_(d-1)]) / sub(ideal( for i from 0 to d-1 list yy_i^(p^n) - S_i ), SY0);
    Ssub := map( SY, S, toList( yy_0..yy_(d-1)) );
    RY := quotient Ssub(ideal R);
    Rsub := (last flattenRing(RY))*map(RY, R, Ssub);
    root := sub(Rsub(phi(f)), R0);
    if root^(p^n) == f then root else error "no root found"
);



wittOverringToTuple(RingElement) := F -> (
    OR := ring F;
    (p, n) := toSequence (factor(char OR))#0;
    R := OR.cache.unWitt;
    k := baseRing' R;

    if class k === GaloisField then(
        R' := makeCoefficientFieldPrime R;
        OR' := wittOverring(n, R');
        inc := map(OR', OR, gens OR');
        return witt apply( (wittOverringToTuple(inc(F))).tuple, i->sub(i, R))
    );

    if class R === QuotientRing and not isFinitePrimeField R then(
        S := ambient R;
        ORs := wittOverring(n, S);
        inc = map(ORs, OR, gens ORs);
        return witt apply( (wittOverringToTuple(inc(F))).tuple, i->sub(i, R))
    );

    unWittSub := map(R, OR, vars R);
    wittSub := OR.cache.wittSub;

    if n == 1 then( return witt{sub(unWittSub(F), R)});
    WR1 := witt(n-1, R);
    OR1 := WR1.overring;
    wittReduce := map( OR1, OR, vars OR1);

    F0 := F % ideal(sub(p, OR));
    f0 := sub(takeRoot( unWittSub(F0), n-1 ), R);

    nextF := wittReduce( ( F - (wittSub (f0))^(p^(n-1)) ) // p);
    witt{f0} | wittOverringToTuple( nextF )
)



-----------------------------
-----------------------------
-----------------------------

wittRingToTuple(Ideal) := I -> (
    Igens := flatten entries gens I;
    wittgens := apply(Igens, wittRingToTuple);
    wittIdeal(wittgens)
)


-----------------------------
-----------------------------
-----------------------------

wittOverringIdeal(ZZ, Ideal):=(n, I)->(
    R := ring I;
    d := dim R;
    if class R =!= PolynomialRing and R =!= ZZ then error "expected an ideal in a polynomial ring";
    ideal flatten for k from 0 to n -1 list (
	for j in I_* list wittTupleToOverring witt( toList(k:0_R) | {j} | toList(n-k-1:0_R) )
    )
)

-----------------------------
-----------------------------
-----------------------------

wittRingIdeal(ZZ, Ideal):=(n, I)->(
    J := wittOverringIdeal(n, I);
    WittR := witt(n, ring I);
    WR := explicit WittR;
    Phi := WR.cache.overringMap;
    OR := target Phi;
    B := quotient J;
    kernelZZ( (flattenRing(B))_1 * map(B, ring J) * Phi)
)

