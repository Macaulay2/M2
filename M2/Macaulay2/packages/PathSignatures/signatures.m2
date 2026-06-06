---------------------------------------------------------------------------------
--METHODS FOR COMPUTING SIGNATURES
---------------------------------------------------------------------------------
sig = method()

sig (Path, List) := (X,w) -> (
    nop := X.numberOfPieces;
    h := length(w);
    if(w == {}) then return 1;
    if(nop == 0) then return 0;
    if(nop == 1) then (
        return(polySigGen(X.pieces#0,w,X.bR))
    );
    sum(h+1, i -> (
        sig(X_(0..nop-2), w_{0..i-1})*sig(X_(nop-1),w_{i..h-1}))
    )
)

sig(Path,NCRingElement) := (X,f) -> (
    linExt(w->sig(X,w),f)
)

--Compute the signature in level h of a path X, 
--the output is in the ring R
sig(Path,ZZ,NCRing) := (X, h, R) -> (
    nop := X.numberOfPieces;
    d := X.dimension;
    if(h == 0) then return 1_R;
    if(nop == 0) then return 0;
    if(nop == 1) then (
        ws := toList(apply((h:1)..(h:d), toList));
        return(sum(ws,w-> polySigGen(X.pieces#0,w,X.bR)*(new Array from w)_R));
    );
    sum(h+1, i -> (
        sig(X_(0..nop-2), i, R)*sig(X_(nop-1),h-i, R))
    )
)

--The k-th level signatures of the path, 
--automatically created the output word algebra
sig(Path,ZZ) := (X,h) ->
(
    R := wordAlgebra(X.dimension, CoefficientRing => X.bR);
    sig(X,h,R)
)

--Hard coded canonical monomial path tensor 
--The path is in dimension the number of generators of r
--computes the k-th level signature 
CAxisTensor = method();
CAxisTensor(ZZ, NCPolynomialRing) := NCRingElement => (k,r) -> (
    sum(apply((entries basis(k,r))#0, i-> CAxisComponent(i) * i))
)


-- Hard coded canonical axis path tensor simple components as in 
-- Example 2.1 of "varieties of signature tensors" 
-- C. Amendola et al, 2018
--Inputs: 
--  w, a word in a NCpolynomial ring 

CAxisComponent = method();
CAxisComponent (NCRingElement) := QQ => w -> (
    L := ncMonToList (w);
    if(L!=sort(L)) then return 0;
    distinctPermutations := (#L)!/(product( apply(values tally L, i-> i !)));
    distinctPermutations/((#L))!
);



--Hard coded canonical axis path tensor 
--The path is in dimension the number of generators of r
--computes the k-th level signature
CMonTensor = method();
CMonTensor(ZZ, NCPolynomialRing) := NCRingElement => (k,r) -> (
    sum(apply((entries basis(k,r))#0, i-> CMonComponent(i) * i))
)

-- Hard coded canonical moment path tensor simple components as in 
-- Example 2.3 of "varieties of signature tensors" 
-- C. Amendola et al, 2018
--Inputs: 
--  w, a word in a NCpolynomial ring

CMonComponent= method();
CMonComponent (NCRingElement) := QQ => w -> (
    L := ncMonToList (w);
    product(L_{1..length(L)-1})/product(accumulate(plus,L))
);

----------------------------------------------------------------
--AUXILIARY METHODS (for polynomial integration)
----------------------------------------------------------------

--polyIntegral computes integrals of polynomials with respect to one variable
--f is the integrand
--xn is variable wrt which to integrate
polyIntegral = method()
polyIntegral (RingElement, RingElement) := RingElement => (f, xn) ->(
    R := ring f;
    indexn := index xn;
    termsf := terms f;
    sum(termsf, i->(1_R/(((((exponents(i))#0)#(indexn)+1))) * i * xn))
);


-- polySigGen computes the signature of a polynomial path for words
-- l is the list of components of the polynomial path, each represented by a list
-- Here, a polynomial is represented by its list form, see M2 documentation for listForm
-- w is a list representing a word as in linsig
-- br is the base ring of the coefficients

polySigGen = method()

polySigGen (List, List, Ring) := RingElement => (l,w, baseR) ->(
    if(w == {}) then return 1;

    k:= length w;
    x := getSymbol("x" | toString(random(1000)));
    R := baseR monoid([x_1..x_k]);
    S := baseR monoid([ value("s" | toString(random(1000)))]);
    X := apply(l, i-> sum(0..length(i)-1, j -> ((i#j)#1)_S * (S_0)^((i#j)#0#0)));

    resd := product for i from 1 to k list (
        comp := X#(w#(i-1)-1);
        if(comp == 0) then 0_R else sub(sub(diff(S_0,comp),S), {S_0 => R_(i-1)})
        );
    
    for i from 1 to k-1 do (
        indefinite := sub(polyIntegral(resd, R_(i-1)),R);
        eval0 := substitute(indefinite, {R_(i-1) => 0_QQ});
        eval1:= substitute (indefinite, {R_(i-1) => R_(i)}); --(if i<n then t_{i+1} else 1_R)
        resd = eval1-eval0;
        );
    resd = sub(polyIntegral(resd, R_(k-1)),R);
    --use(baseR);
    resd = substitute(resd, {R_(k-1) => 1_baseR}) - substitute(resd, {R_(k-1) =>0_baseR});
    
    if class resd === baseR then resd else leadCoefficient resd
);







