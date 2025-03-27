----------------------------------------------------
--gaugeMatrix computes change of basis matrix
----------------------------------------------------
gaugeMatrix = method();
-- I D-ideal, new standard monomials
gaugeMatrix(Ideal, List) := (I, newStdMon) -> (

  if not isWeylAlgebra (ring I) then error "expected left ideal in a Weyl algebra";

  if not isWeylAlgebra (ring first newStdMon) then "expected elements of list to be in Weyl algebra";

  if not same apply(newStdMon, ring) then error "expected elements of the list to be in the same Weyl algebra";

  D := ring I;
  F := baseFractionField D;

  G := flatten entries gens gb I;
  stdMon := standardMonomials(I);
  -- obtain weight ordering from D
  w := (((options(D)).MonomialOrder)#1)#1;
  gaugeMat := mutableMatrix map(F^(length newStdMon), F^(length stdMon),0);
  -- rows are indexed by new std monomials
  for rowIndex from 0 to length(newStdMon)-1 do
  (
    -- compute normalForm of new std monomials wrt. gb of I
    reducedWRTG := normalForm(newStdMon#rowIndex,G);
    coeffsReduced := coefficients reducedWRTG;
    for j from 0 to length(flatten entries coeffsReduced_0)-1 do
    --runs through (d-)monomial support of the reduced element
    (
      -- sorts matrix according to the orderd std monomial basis
      monomialToFind := sub((flatten entries coeffsReduced_0)#j,D);
      colIndex := position(stdMon, i->sub(i,D) == monomialToFind);
      gaugeMat_(rowIndex,colIndex) = sub((flatten entries coeffsReduced_1)#j, F);
    );
  );
  matrix gaugeMat
);

-- G a generating set for a D-ideal, new standard monomials
gaugeMatrix(List,List) := (G, newStdMon) -> (
  D := ring G#0;
  gaugeMatrix(ideal(G),newStdMon)
)

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
    if not same apply(A, ring) or frac(ring M) =!= ring first A then error "expected matrices over the same ring";
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
    if not same apply(A, ring) or frac(ring M) =!= ring first A then error "expected matrices over the same ring";
    D := inferWeylAlgebra(ring A#0);
    gaugeTransform(M,A,D)
)

-----------------------------------------------------------------------
--epsilonFactorized: checks if the system is in epsilon-factorized form
-----------------------------------------------------------------------

isEpsilonFactorized = method();
-- checks factorization for a whole system of connection matrices
isEpsilonFactorized(List,RingElement) := (P,e) -> (
    e = e_(ring first P);
    if not same apply(P, ring) then error "expected matrices over the same ring";
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
    e = e_(ring M);
    lstdeg := lstOfDegrees(M,e);
    if #lstdeg == 0 then return false;
    all(lstdeg, x -> x == first lstdeg)
)


-- extract list of degrees with vars set to 0
lstOfDegrees = (M,e) -> (
    R := ring M;
    -- Read off the "exponents" of e in R, to single it out from the generators
    d := if instance(R, FractionField)  then first exponents numerator e_R
    else if instance(R, PolynomialRing) then first exponents e_R
    else error "expected matrices over a fraction field or rational Weyl algebra";

    -- Create new ring, where only e has degree 1, rest 0.
    F := frac newRing(baseRing baseFractionField R, Degrees => d);
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
