
Delta1 = method()
Delta1(RingElement) := a-> (
    last (wittOverringToTuple( wittTupleToOverring(witt{a, 0}) - sum apply(terms a, i -> wittTupleToOverring(witt{i, 0})))).tuple
)



u = method()
u(ZZ, RingElement) := (e, f) -> (
    S := ring f;
    if class S =!= PolynomialRing then error "needed an element of a polynomial ring";
    n := numgens S;
    p := char S;
    traceTerms := select(terms f, i-> ((flatten exponents i) % p^e) == toList(n:p^e-1));
    sum apply(traceTerms, i-> (last coefficients(i)  ) * S_(flatten exponents(i) // p^e))
)


--We arbitrarily chose to stop the computations at a height higher than 102 because it gets quite slow. The code does work for higher quasi F Splitting height so a user can change this if they wishes.

fSplittingHeight = method(Options=>{MaxHeight => 102})
fSplittingHeight(Ideal) := ZZ => opts-> I0-> (
    S := ring I0;
    if class S =!= PolynomialRing then error "expected ambient ring to be a polynomial ring";
    if codim(I0) < numgens I0 then error "expected ideal to be a complete intersection";
    p := char S;
    if p == 0 then error "expected a ring of characteristic p";
    if (coefficientRing S)#order != p then error "expected a field with p elements";
    A := dim S-1;
    ff := product I0_*;
    Frob := map(S, S, matrix{apply(gens S, u -> u^p)});
    (FS, GS, transformS) := pushFwd(Frob);

    M := ideal (gens S);
    MP := frobenius(1, M);
    v := product(A+1, i -> (S_i)^(p-1));
    u0 := map(S^1, FS, transpose(transformS(v)));

    I := ideal ff^(p-1);
    if not isSubset(I, MP) then return 1;
    if isSubset(ideal ff^(p-2), MP) then return infinity;
    if isSubset( (ideal(ff^(p-2)) + frobenius(1, I0)) * ff^(p*(p-2)) * Delta1(ff), frobenius(2, M)) then return infinity;

    del := Delta1(ff^(p-1));
    K := pushmultiple(del, GS, transformS);
    II := I;

    for i from 2 to opts.MaxHeight do(
        print( "trying i = "|i);
        FI := image(pushideal(I, GS, transformS));
        u := map(S^1, ambient FI, u0);
        FS := source u;
        J := intersect(FI, kernel(u));
        JJ := inducedMap(FS, J);
        KK := image(u*K*JJ);
        II := ideal(mingens KK) + ideal(ff^(p-1));
        if not isSubset(II, MP) then break return i;
        if I == II then break return infinity;
        I = II;
    );
)

pushmultiple = (r, GS, transformS) -> (
    A := dim ring r - 1;
    p := char ring r;
    FS := source GS;
    S := target GS;
    C := transformS( r*GS_(0,(0)) );
    for j from 2 to p^(A+1) do (
        D := transformS( r*GS_(0,(j-1)) );
        E := C|D;
        C = E;
    );
    map(FS, S^(p^(A+1)), C)
)

pushideal = (I, GS, transformS) -> matrix{ for i in I_* list pushmultiple(i, GS, transformS)}


