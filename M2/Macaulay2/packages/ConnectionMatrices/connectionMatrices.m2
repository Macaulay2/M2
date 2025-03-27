------------------------------------------------------------
--connectionMatrices: computes system of connection matrices
------------------------------------------------------------

-- to access private methods from Core
importFrom_Core { "concatCols" }

connectionMatrices = method()

-- gives connection matrices for a D-ideal
-- c.f. [Theorem 1.4.22, SST]
connectionMatrices Ideal := List => I -> I.cache.connectionMatrices ??= (

    -- Todo: Assert that I is an ideal in the Weyl algebra.

    D := ring I;
    createDpairs D;
    -- TODO: this might not always be correct,
    -- why was the method that accepted w removed?
    w := (((options(D)).MonomialOrder)#1)#1;
    -- this acts as the associated graded ring of rational Weyl algebra
    R := rationalWeylAlgebra D;
    F := baseFractionField D;
    f := map(F, R);
    -- gb with respect to an elimination order
    G := gens gb I;
    if debugLevel > 0 then printerr("Grobner basis: ", net G);
    r := holonomicRank(w, M := comodule I);
    if r === infinity then error "system is not finite dimensional";
    B := sub(M.cache#"basis", R);
    if debugLevel > 0 then printerr("Standard monomials: ", net B);
    apply(D.dpairVars#1,
	dt -> transpose concatCols apply(flatten entries B,
	    s -> f last coefficients(
		-- computes (dt * s) % G
		normalForm(dt_R * s, first entries G), Monomials => B)))
)

-- gives the system of connection matrices with respect to a new basis B
connectionMatrices(List,Ideal) := (B,I)->(
    g := gaugeMatrix(I,B);
    A := connectionMatrices I;
    gaugeTransform(g, A)
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
    -- TODO: does this not suffice?
    --R := ring A#0;
    --D := R#"OriginalWeylAlgebra"
    K := coefficientRing(ring P_0);
    D := makeWA(K(monoid[gens ring P_0]));
    R := rationalWeylAlgebra D;
    f := map(R, ring P_0);
    var := gens R;
    net sum(numgens R, i -> var_i * f(P_i))
)

----------------------------------------------------------------
--standardMonomials: computes std monomials wrt. to weight order
----------------------------------------------------------------
standardMonomials = method()

-- D-ideal as an input
standardMonomials Ideal := I -> (
    D := ring I;
    if not isWeylAlgebra D then error "expected left ideal in a Weyl algebra";
    w := (((options(D)).MonomialOrder)#1)#1;
    M := comodule I;
    r := holonomicRank(w, M);
    B := sub(M.cache#"basis", D);
    return flatten entries B;
)

-- TODO: does it matter that G is a GB?
-- GB of a D-ideal as an input
standardMonomials List := G -> standardMonomials ideal G
