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
-- Add check that P is degree 1 in derivation
diffRationalWeyl(RingElement,RingElement,RingElement) :=(P,f,g)->(
    (h1,h2) :=(g*diffWeyl(P,f)-f*diffWeyl(P,g),g^2);
    R := ring f;
    sub(h1, R)/sub(h2,R)
);

-- Convert entries of Pfaffian to fractions
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
--gaugeTransform implements gauge transform formula
----------------------------------------------------

gaugeTransform = method();
-- gives the gauge transform of a pfaffians system for a given change of basis matrix
gaugeTransform(Matrix, List, PolynomialRing) := (G, C, W)->(
    G = sub(G, ring C#0);
    invG := inverse G;
    n := dim W//2;
    for i from 1 to n list(
        dxi := W_(n+i-1);
        diffMatrixWeyl(dxi, G)*invG + G*(C#(i-1))*invG
    )
);

----------------------------------------------------
--epsilonFactorized checks if the system is in epsilon-factorized form
----------------------------------------------------

isEpsilonFactorized = method();
-- checks factorization for a whole Pfaffian system
isEpsilonFactorized(List,RingElement) := (P,e) -> (
    EF := true;
    for p in P do (
        if p % e == 0 then continue
        else EF = false
    );
    EF
)

-- checks factorization for a single Pfaffian
isEpsilonFactorized(List,RingElement) := (P,e) -> (
    EF := true;
    for p in P do (
        if p % e == 0 then continue
        else EF = false
    );
    EF
)

end--
restart
-- debug needsPackage "Dmodules"
-- debug needsPackage "WeylAlgebras"
-- needs "changeofbasis.m2"


-- W = makeWA(QQ[x,y])
-- P = dx
-- f = x^2
-- g = 3*y^5
-- diffratW(P,f,g)

-- needs "pfaffians.m2"
-- W = QQ[x,y,dx,dy, WeylAlgebra =>{x=>dx,y=>dy}]
-- P=x*dx^2-y*dy^2+2*dx-2*dy
-- Q=x*dx+y*dy+1
-- I=ideal(P,Q)
-- C = pfaffians I
-- G = matrix{{x,0},{0,y}}
-- gaugeTransform(G,C,D)
