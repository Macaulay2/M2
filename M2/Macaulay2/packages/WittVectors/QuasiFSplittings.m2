
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



table2 = method()
table2(ZZ):= n-> (
x := symbol x;
y := symbol y;
z := symbol z;
w := symbol w;
S := (ZZ/3)[x, y, z, w];
I := new MutableHashTable from {};
I#1 = x^4 + y^4 + z^4 + 2*w^4 + x^2* y*w + y*z^2*w;
I#2 = x^4 + 2*y^4 + 2*z^4 + 2*w^4 + x*y*z^2;
I#3 = x^4 + y^4 + z^4 + w^4 + x^2*z^2 + x*y*z^2 + z^3*w;
I#4 = x^4 + y^4 + z^4 + w^4 + x^2*z^2 + x*y*z^2;
I#5 = x^4 + y^4 + z^4 + w^4 + x^3*z + z^3*w + y*z^2*w + y*z*w^2;
I#6 = x^4 + y^4 + z^4 + w^4 + x^2*z^2 + x^2*y*z;
I#7 = x^4 + y^4 + z^4 + w^4 + x*y^2*z + x*z^2*w + y*z*w^2 + y^2*z*w;
I#8 = x^4 + x^2*y*z + x^2*y*w + 2*x^2*z^2 + x*y*w^2 + 2*y^4 + y^3*w + z^4 + w^4;
I#9 = x^4 + y^4 + z^4 + w^4 + x*y^3 + y^3*w + z^2*w^2 + 2*x*y*z^2 + y*z*w^2;
I#10 = x^4 + 2*x^2*y*z + x^2*y*w + x*y^2*w + y^4 + y^3*w + y^2*z^2 +2*y^2*z*w + y^2*w^2 + y*z^3 + y*z^2*w + y*z*w^2 + z^4 + z*w^3;
I#11 = x^4 + y^4 + z^4 + w^4;
if n>10 then return I#11 else return I#n
)

