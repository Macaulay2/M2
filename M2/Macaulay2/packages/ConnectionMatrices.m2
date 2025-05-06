newPackage(
    "ConnectionMatrices",
    Version => "1.0",
    Date => "March 2025",
    Authors => {
	{ Name => "Paul Goerlach",           Email => "paul.goerlach@ovgu.de",              HomePage => "" },
	{ Name => "Joris Koefler",           Email => "joris.koefler@mis.mpg.de",           HomePage => "" },
	{ Name => "Mahrud Sayrafi",          Email => "mahrud@fields.utoronto.ca",          HomePage => "https://math.umn.edu/~mahrud/" },
	{ Name => "Anna-Laura Sattelberger", Email => "anna-laura.sattelberger@mis.mpg.de", HomePage => "" },
	{ Name => "Hendrik Schroeder",       Email => "h.schroeder@tu-berlin.de",           HomePage => "" },
	{ Name => "Nicolas Weiss",           Email => "nicolas.weiss@mis.mpg.de",           HomePage => "" },
	{ Name => "Francesca Zaffalon",      Email => "francesca.zaffalon@mis.mpg.de",      HomePage => "" }
    },
    Headline => "connection matrices and integrable systems from D-ideals",
    Keywords => { "D-modules" },
    PackageExports => { "Dmodules" },
    AuxiliaryFiles => true,
    DebuggingMode => false,
)

export {
    -- see ConnectionMatrices/reduce.m2
    "normalForm",
    "baseFractionField",
    -- see ConnectionMatrices/connectionMatrices.m2
    "standardMonomials",
    "connectionMatrices",
    "connectionMatrix",
    -- see ConnectionMatrices/integrabilityCheck.m2
    "isIntegrable",
    -- see ConnectionMatrices/gaugeMatrix.m2
    "gaugeMatrix",
    -- see ConnectionMatrices/gaugeTransform.m2
    "gaugeTransform",
    "isEpsilonFactorized",
}

-- to access private methods from Dmodules
--debug Dmodules

--------------------------------------------------------------------
-- contains changes to Dmodules method holonomicRank
--------------------------------------------------------------------

load "./ConnectionMatrices/holonomic.m2"

--------------------------------------------------------------------
-- Normal forms over the rational Weyl algebra
--------------------------------------------------------------------

load "./ConnectionMatrices/normalForm.m2"

-------------------------------------------------------------
-- connectionMatrices: computes system of connection matrices
-------------------------------------------------------------

-- borrowed from Varieties: twists don't make sense on connection matrices, so we remove them
dehomogenizeMatrix = f -> (R := ring f; map(R^(numRows f), R^(numColumns f), f))

-- to access private methods from Core
importFrom_Core { "concatCols" }

connectionMatrices = method()

-- gives connection matrices for a D-ideal
-- c.f. [Theorem 1.4.22, SST]
connectionMatrices Ideal := List => I -> I.cache.connectionMatrices ??= (
    D := ring I;
    if not isWeylAlgebra D then error "expected left ideal in a Weyl algebra";
    -- TODO: Change holonomic Rank, so it only takes ideal and infers the order from it.
    w := (((options(D)).MonomialOrder)#1)#1;
    -- this acts as the associated graded ring of rational Weyl algebra
    R := rationalWeylAlgebra D;
    F := baseFractionField D;
    RtoF := map(F, R);
    -- gb with respect to an elimination order
    G := gens gb I;
    if debugLevel > 0 then printerr("Grobner basis: ", net G);
    r := holonomicRank(w, M := comodule I);
    if r === infinity then error "system is not finite dimensional";
    B := sub(M.cache#"basis", R);
    if debugLevel > 0 then printerr("Standard monomials: ", net B);
    apply(generators R, -- loops over the differentials
	dt -> dehomogenizeMatrix transpose concatCols apply(flatten entries B,
	    s -> RtoF last coefficients(
		-- computes (dt * s) % G
		normalForm(dt * s, first entries G), Monomials => B)))
)

-- gives the system of connection matrices with respect to a new basis B
connectionMatrices(Ideal, List) := (I, B)->(
    g := gaugeMatrix(I,B);
    A := connectionMatrices I;
    gaugeTransform(g, A)
)

----------------------------------------------------
-- connectionMatrix: computes connection matrix of I
----------------------------------------------------

connectionMatrix = method()

-- D-ideal as an input
connectionMatrix Ideal := Net => I -> (
    P := connectionMatrices I;
    R := rationalWeylAlgebra ring I;
    net sum(P, gens R, (Px, dx) -> Px * dx)
)

-- a system of connection matrices as input
connectionMatrix List := Net => P -> (
    F := ring P#0;
    D := inferWeylAlgebra F;
    R := rationalWeylAlgebra D;
    -- Check all are over the same ring.
    if not same apply(P, ring) then error "expected matrices over the same ring";
    -- Check that length of list == #differential variables
    if not numgens R == #P     then error "expected the same number of matrices as there are differential variables";
    f := map(R, F);
    net sum(P, gens R, (Px, dx) -> f(Px) * dx)
)

-----------------------------------------------------------------
-- standardMonomials: computes std monomials wrt. to weight order
-----------------------------------------------------------------

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

-- List of generators of ideal as input
standardMonomials List := G -> standardMonomials ideal G

--------------------------------------------------------------------
-- change of basis
--------------------------------------------------------------------

load "./ConnectionMatrices/gaugeTransform.m2"

--------------------------------------------------------------------
-- Tools to check the integrability of a system of connection matrices
--------------------------------------------------------------------

load "./ConnectionMatrices/integrabilityCheck.m2"

--------------------------------------------------------------------
-- Tests section
--------------------------------------------------------------------

load "./ConnectionMatrices/tests.m2"

--------------------------------------------------------------------
-- Documentation section
--------------------------------------------------------------------

beginDocumentation()
load "./ConnectionMatrices/docs.m2"
load "./ConnectionMatrices/examples.m2"

end--

--------------------------------------------------------------------
-- Development section
--------------------------------------------------------------------

restart
debug needsPackage "ConnectionMatrices"
check "ConnectionMatrices"

uninstallPackage "ConnectionMatrices"
restart
installPackage "ConnectionMatrices"
viewHelp "ConnectionMatrices"
