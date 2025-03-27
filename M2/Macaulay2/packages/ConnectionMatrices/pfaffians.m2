------------------------------------------------------------
--connectionMatrices: computes system of connection matrices
------------------------------------------------------------

-- to access private methods from Core
importFrom_Core { "concatCols" }

connectionMatrices = method()

-- gives connection matrices for a D-ideal
-- c.f. [Theorem 1.4.22, SST]
connectionMatrices(Ideal) := List => (I) -> (
    D := ring I;
    createDpairs D;
    w := (((options(D)).MonomialOrder)#1)#1;
    -- this acts as the associated graded ring of rational Weyl algebra
    R := rationalWeylAlgebra(D);
    -- gb with respect to an elimination order
    G := gens gb I;
    if debugLevel > 0 then printerr("Grobner basis: ", net G);
    r := holonomicRank(w, M := comodule I);
    if r === infinity then error "system is not finite dimensional";
    B := sub(M.cache#"basis", R);
    if debugLevel > 0 then printerr("Standard monomials: ", net B);
    A := apply(D.dpairVars#1,
	dt -> transpose concatCols apply(flatten entries B,
	    s -> last coefficients(
		-- computes (dt * s) % G
		normalForm(dt_R * s, first entries G), Monomials => B)));
    apply((A/entries)/matrix, p ->sub(p,fractionField D))
)

-- gives the system of connection matrices with respect to a new basis B
connectionMatrices(List,Ideal) := (B,I)->(
    W := ring I;
    G := gaugeMatrix(I,B);
    invG := inverse G;
    n := dim W//2;
    C := connectionMatrices I;
    for i from 1 to n list(
        dxi := W_(n+i-1);
        diffMatrixWeyl(dxi, G)*invG + G*(C#(i-1))*invG
    )
)

----------------------------------------------------
--connectionMatrix: computes connection matrix of I
----------------------------------------------------
connectionMatrix = method()

-- D-ideal as an input
connectionMatrix(Ideal) := List => (I) -> (
    P := connectionMatrices(I);
    R := rationalWeylAlgebra(ring I);
    var := gens R;
    net(sum((for i from 0 to length(var)-1 list var_i*P_i )))
)


-- allows for a system of connection matrices as input
connectionMatrix(List) := List => (P) -> (
    R := rationalWeylAlgebra(makeWA(coefficientRing(ring P_0)[gens ring P_0]));
    var := gens R;
    net(sum((for i from 0 to length(var)-1 list var_i*sub(P_i,R))))
)

----------------------------------------------------------------
--standardMonomials: computes std monomials wrt. to weight order
----------------------------------------------------------------
standardMonomials = method()

-- D-ideal as an input
standardMonomials(Ideal) := (I) -> (
    D := ring I;
    if not isWeylAlgebra D then error "expected left ideal in a Weyl algebra";
    w := (((options(D)).MonomialOrder)#1)#1;
    M := comodule I;
    r := holonomicRank(w, M);
    B := sub(M.cache#"basis", D);
    return flatten entries B;
);

-- GB of a D-ideal as an input
standardMonomials(List) := (G) -> (
    D := ring (G#0);
    standardMonomials(ideal G)
)
