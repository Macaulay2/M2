----------------------------------------------------
-- some matrix differentiating tools
----------------------------------------------------

diffWeyl=method();
diffWeyl(RingElement,RingElement) :=(P,f)->(
    DR := ring P;
    R := ring f;
    createDpairs DR;
    partials := ideal( DR.dpairVars_1);
    sub((P*sub(f,DR)) % partials,R)
);

-- quotient differentiating formula
diffRationalWeyl=method();
diffRationalWeyl(RingElement,RingElement,RingElement) :=(P,f,g)->(
    (h1,h2) :=(g*diffWeyl(P,f)-f*diffWeyl(P,g),g^2);
    R := ring f;
    sub(h1, R)/sub(h2,R)
);

-- Convert entries of connection matrices to fractions
convertEntry = method();
convertEntry(RingElement) := (h)->(
    if coefficientRing(ring h) === QQ then subRing := ring h
        else subRing = coefficientRing ring h;
    frach := sub(h, subRing);
    (numerator frach, denominator frach)
);

--Differentiate matrix
diffMatrixWeyl = method();
diffMatrixWeyl(RingElement, Matrix) :=(P, M)->(
    lrows := entries M;
    matrix apply(lrows, a-> apply(a, b-> diffRationalWeyl(toSequence({P}| toList convertEntry(b)))))
);

----------------------------------------------------
--gaugeTransform: implements gauge transform formula
----------------------------------------------------

gaugeTransform = method();
-- gives the gauge transform of a system of connection matrices w.r.t. a given change of basis matrix.
gaugeTransform(Matrix, List, PolynomialRing) := (M, A, D)->(
    M = sub(M, ring A#0);
    invM := inverse M;
    n := dim D//2;
    for i from 1 to n list(
        dxi := D_(n+i-1);
        diffMatrixWeyl(dxi, M)*invM + M*(A#(i-1))*invM
    )
);
-- Infering the Weyl algebra to perform the gauge transform in:
gaugeTransform(Matrix, List) := (M,A)->(
    D := inferWeylAlgebra(ring A#0);
    gaugeTransform(M,A,D)
)

----------------------------------------------------
--epsilonFactorized: checks if the system is in epsilon-factorized form
----------------------------------------------------

isEpsilonFactorized = method();
-- checks factorization for a whole system of connection matrices
isEpsilonFactorized(List,RingElement) := (P,e) -> (
    lst := new MutableList;
    for p in P do (
        tmp  := lstOfDegrees(p,e);
        lst#(#lst) = tmp;
        if #tmp == 0 then return false;
    );
    lstdeg := flatten toList lst;
    all(lstdeg, x -> x == first lstdeg)
)

--checks factorization for single connection matrix
isEpsilonFactorized(Matrix,RingElement) := (M,e) -> (
    lstdeg := lstOfDegrees(M,e);
    if #lstdeg == 0 then return false;
    all(lstdeg, x -> x == first lstdeg)
)


-- extract list of degrees with vars set to 0
lstOfDegrees = (M,e) -> (
    -- Read off the "exponents" of e, to single it out from the generators
    d := (if (class ring e === FractionField) then (exponents numerator e) else (assert(class ring e === PolynomialRing); exponents e))#0;

    -- Create new ring, where only e has degree 1, rest 0.
    R := QQ[gens ring e, Degrees => d];
    F := frac(R);
    Mlst := select(flatten entries M, x -> x != 0);
    if #Mlst == 0 then return {{0}};
    lst := new MutableList;
    for m in Mlst do (
        if isHomogeneous(numerator sub(m,F)) == true and isHomogeneous(denominator sub(m,F)) == true then (
            lst#(#lst) = degree (numerator sub(m,F)) - degree(denominator sub(m,F));
        )
        else return {};
    );
    toList lst
)