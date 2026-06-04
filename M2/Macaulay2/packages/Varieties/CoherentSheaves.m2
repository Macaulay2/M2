-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- given a list {a,a,b,b,b,c,...} returns a list {{2,a}, {3,b}, {1,c}, ...}
runLengthEncoding := x -> if #x === 0 then x else (
    p := join({0}, select(1 .. #x - 1, i -> x#i =!= x#(i-1)), {#x});
    apply(#p-1, i -> (p#(i+1)-p#i, x#(p#i))))

-----------------------------------------------------------------------------
-- SheafOfRings and CoherentSheaf type declarations and basic constructors
-----------------------------------------------------------------------------

-- TODO: is this a good idea for fixing type errors?
SheafOfRings = new Type of Ring
SheafOfRings.synonym = "sheaf of rings"

CoherentSheaf = new Type of HashTable
CoherentSheaf.synonym = "coherent sheaf"
CoherentSheaf.GlobalAssignHook = globalAssignFunction
CoherentSheaf.GlobalReleaseHook = globalReleaseFunction

-- see Varieties/SheafMaps.m2
CoherentSheaf#id = F -> map(F, F, id_(module F))
CoherentSheaf#0  = X -> (sheaf X)^0

-- constructors
sheaf = method()
sheaf Ring := Ring^~ := SheafOfRings =>     R  -> sheaf(variety R, R)
sheaf Variety        := SheafOfRings =>  X     -> sheaf(X, ring X)

sheaf(Variety, Ring) := SheafOfRings => (X, R) -> (
    if ring X =!= R then error "sheaf: expected ring of the variety";
    X.sheaf ??= (
	O := new SheafOfRings from {
	    symbol variety => X,
	    symbol ring    => R,
	    symbol cache   => new CacheTable
	    };
	-- TODO: need to either automate this, or add it in NormalToricVarieties, etc.
	promote(Number,   O) := promote(RingElement, O) := RingElement => (x, O) -> promote(x, ring variety O);
	promote(SheafMap, O) := promote(SheafMap,    R) := SheafMap    => baseChange;
	-- We cache this so that constructing O^1 at different times will yield the _same_ CoherentSheaf,
	-- which itself may have cached information over time.
	O)
    )

-- TODO: should the module of a sheaf be fixed, or should it be allowed to change?
-- TODO: https://github.com/Macaulay2/M2/issues/1358
sheaf Module := Module^~ := CoherentSheaf =>     M  -> sheaf(variety ring M, M)
sheaf(Variety, Module)   := CoherentSheaf => (X, M) -> M.cache#(sheaf, X) ??= (
    if ring M =!= ring X then error "sheaf: expected module and variety to have the same ring";
    if instance(X, ProjectiveVariety) and not isHomogeneous M then error "sheaf: expected a homogeneous module";
    new CoherentSheaf from {
	symbol variety => X,
	symbol module  => M,
	symbol cache   => new CacheTable
	}
    )

-- TODO: consider adding IdealSheaf or SheafOfIdeals type
sheaf Ideal := Ideal^~ := CoherentSheaf =>     I  -> sheaf(variety ring I, module I)
sheaf(Variety, Ideal)  := CoherentSheaf => (X, I) -> sheaf(X,              module I)

OO = new ScriptedFunctor from {
    subscript => X -> applyMethod((symbol _,     OO, class X), (OO, X)),
    argument  => X -> applyMethod((symbol SPACE, OO, class X), (OO, X)),
    }
OO.texMath = ///{\mathcal O}///
installMethod(symbol_, OO, Variety, SheafOfRings => (OO, X) -> sheaf(X, ring X))

isWellDefined SheafOfRings := O -> O.variety.ring === O.ring and isWellDefined O.variety
isCommutative SheafOfRings := O -> isCommutative O.ring

isWellDefined CoherentSheaf := F -> (
    M := module F;
    X := variety F;
    true -- TODO: isWellDefined M
    and isWellDefined X
    -- data type checks
    and assert'(set keys F === set { symbol variety, symbol module, symbol cache },
	"the hash table does not have the expected keys")
    and assert'(
	instance(F.variety, Variety) and
	instance(F.module, Module)   and
	instance(F.cache, CacheTable),
	"the hash table does not have the expected values")
    -- mathematical checks
    and assert'(ring M === ring X,
	"underlying module and variety do not have the same ring")
    and assert'(not isProjective X or isHomogeneous M,
	"underlying module of coherent sheaf on a projective variety should be homogeneous")
    )

-- basic methods
variety SheafOfRings  :=
variety CoherentSheaf := F -> F.variety

ring SheafOfRings  :=
ring CoherentSheaf := SheafOfRings => F -> sheaf variety F

-- This is the module associated to the fixed CoherentSheaf, O^1.
module SheafOfRings  := Module => O -> module O.ring
module CoherentSheaf := Module => F -> F.module

codim   CoherentSheaf := options(codim, Module) >> o -> F -> (
    -- If the support of F in its variety is empty,
    -- we say that the codimension of F is infinity.
    if dim F <= -1 then infinity else codim(F.module, o))
rank    CoherentSheaf := F -> rank    F.module
numgens CoherentSheaf := F -> numgens F.module
betti   CoherentSheaf := o -> F -> betti(F.module, o)

super   CoherentSheaf := CoherentSheaf => F -> sheaf(F.variety, super   F.module)
ambient CoherentSheaf := CoherentSheaf => F -> sheaf(F.variety, ambient F.module)
cover   CoherentSheaf := CoherentSheaf => F -> sheaf(F.variety, cover   F.module)

-- TODO: do all of the following need to be hookified? Perhaps prefixing
-- the variety to the key, like 'dim(X, F)', would be better?

dim SheafOfRings  := O -> dim O^1
dim CoherentSheaf := F -> (
    if not isProjective variety F then return dim module F;
    -- Note that twisting does not change the dimension of F
    if F.cache.?BaseTwist then F = F.cache.BaseTwist#0;
    M := currentModuleBaseRing F;
    -- For a singly graded algebra R, the dimension of F = M^~ on Proj(R) is the
    -- dimension of M as an R-module minus 1, except that we always return at least -1.
    -- For an m-graded ring R, this definition views Proj(R)
    -- as the quotient of an (unspecified) open subset of Spec(R) by a generically stable action
    -- of a torus of dimension m. That is not the standard definition of Proj; but it may
    -- be useful if developed further. For now, many commands for projective varieties
    -- are restricted to the singly graded case.
    max(-1, dim M - degreeLength ring M))

degree SheafOfRings  := O -> degree O^1
degree CoherentSheaf := F -> (
    -- The degree of a coherent sheaf on a closed subspace of a weighted projective space.
    -- Beware that the degree of a coherent sheaf is not directly related
    -- to the notion of the degree of a line bundle on a curve.
    -- Indeed, every line bundle L on a curve X in projective space has degree
    -- _as a coherent sheaf_ equal to the degree of X. See degreeOnCurve.
    -- Note that twisting does not change the degree of F.
    if F.cache.?BaseTwist then F = F.cache.BaseTwist#0;
    M := currentModuleBaseRing F;
    R := assertWeightedZZGraded ring M;
    -- The degree is an integer if all weights are equal to 1, but otherwise a rational number.
    -- For example, on X = PP^n(a_0,...,a_n), the structure sheaf OO_X has degree 1/(a_0...a_n).
    -- This is compatible with the function hilbertPolynomial F.
    if isStandardGraded R then degree module F
    else degree M / product flatten degrees R)

-- this function is not an invariant of sheaves,
-- but is very convenient for sums of line bundles, for instance
degrees CoherentSheaf := F -> degrees module F

degreeOnCurve = method()
degreeOnCurve SheafOfRings  := O -> degreeOnCurve O^1
degreeOnCurve CoherentSheaf := F -> (
    -- The degree of a vector bundle F (for example, a line bundle) on a projective curve X.
    -- Beware that this is not directly related to the degree of F as a coherent sheaf on projective space,
    -- which would just be the rank of F times the degree of X.
    --
    -- This function does not check that F is a vector bundle of constant rank;
    -- if not, the answer may be meaningless. It does check that the variety
    -- on which F is defined has dimension 1. The function works correctly for a vector bundle
    -- on a curve X (viewed as a stack) in a weighted projective space,
    -- in which case the degree of F is a rational number rather than an integer.
    -- (It is the intersection number  integral_X c_1(F).)
    X := variety F;
    if dim X != 1 then error "expected a vector bundle on a curve";
    hilbF := hilbertPolynomial(F,      Projective => false);
    hilbO := hilbertPolynomial(OO_X^1, Projective => false);
    i := (ring hilbO)_0;
    -- this is the rank of F on a curve
    rkF := coefficient(i, hilbF) / coefficient(i, hilbO);
    deg := coefficient(i^0, hilbF) - rkF * coefficient(i^0, hilbO);
    -- We return an integer if X lives in projective space,
    -- and a rational number for X in a more general weighted projective space.
    if isStandardGraded ring X then lift(deg, ZZ) else deg)

genus   CoherentSheaf := F -> (
    if isStandardGraded ring module F
    then genus module F
    else (-1)^(dim F) * (euler F - 1))
genera  CoherentSheaf := F -> genera  module F
-- TODO: this is incorrect in higher Picard rank
pdim    CoherentSheaf := F -> tryHooks((pdim,  CoherentSheaf), F, pdim  @@ module)

-- twist and powers
-- TODO: sheaf should dehomogenize modules on Affine varieties
-- These work correctly even for multigraded rings, e.g. F(1) or F(1,2,3)
SheafOfRings(ZZ)  := SheafOfRings  Sequence := CoherentSheaf => (O, a) -> O^1(a)
CoherentSheaf(ZZ) := CoherentSheaf Sequence := CoherentSheaf => (F, a) -> (
    X := variety F;
    deg := splice {a};
    -- If a coherent sheaf is defined as a twist, say G = F(a), we cache the original sheaf.
    -- Any later calculations made about G will be cached as information about F.
    if F.cache.?BaseTwist then (F, deg) = (
	F.cache.BaseTwist#0,        -- the original sheaf
	F.cache.BaseTwist#1 + deg); -- the combined twist
    if deg === degree 1_(ring X) then return F; -- F(a)(-a) = F.
    G := F ** OO_X^{deg};
    G.cache.BaseTwist = (F, deg);
    G)
-- TODO: should modules also cache their base twist?
Module(ZZ) := Module Sequence := Module => (M, a) -> M ** (ring M)^{splice{a}}
Matrix(ZZ) := Matrix Sequence := Matrix => (f, a) -> f ** (ring f)^{splice{a}}
Ring(ZZ)   := Ring   Sequence := Module => (R, a) -> (R^1) ** R^{splice{a}}

CoherentSheaf ^ ZZ := CoherentSheaf ^ List := CoherentSheaf => (F, n) -> sheaf(F.variety, F.module^n)
SheafOfRings  ^ ZZ := SheafOfRings  ^ List := CoherentSheaf => (O, n) -> (
    -- this distinction is made because module(Ring) is cached, but Ring^ZZ currently is not
    if n === 1 then sheaf(O.variety, module O.ring) else sheaf(O.variety, O.ring^n))

dual CoherentSheaf := CoherentSheaf => options(dual, Module) >> o -> F -> sheaf(F.variety, dual(F.module, o))

-- There are several equivalent conditions for equality:
-- 1. Saturation of the underlying modules is the same (i.e. Gamma_* F == Gamma_* G)
-- 2. Truncation of the underlying modules is the same
-- Here we use the first, but start with comparing Hilbert polynomials, which may be faster,
-- TODO: benchmark different strategies
CoherentSheaf == CoherentSheaf := Boolean => (F, G) -> F.variety === G.variety and (
    hilbertPolynomial F === hilbertPolynomial G and module prune F == module prune G)
-- FIXME: dim module F <= 0 breaks for toric varieties
CoherentSheaf == ZZ            := Boolean => (F, z) -> (
    if z == 0 then dim module F <= 0 else error "attempted to compare sheaf to nonzero integer")
CoherentSheaf == Module        := Boolean => (F, M) -> F == sheaf M
Module        == CoherentSheaf := Boolean => (M, F) -> sheaf M == F
ZZ            == CoherentSheaf := Boolean => (z, F) -> F == z
-- isIsomorphic is defined in SheafMaps.m2 because we return the isomorphism as well

-- use for sorting a list
CoherentSheaf ? CoherentSheaf := lookup(symbol ?, Module, Module)

-- arithmetic ops
CoherentSheaf.directSum = args -> (
    assertSameVariety args;
    F := sheaf(variety args#0, directSum apply(args, module));
    F.cache.components = toList args;
    F)
CoherentSheaf ++ CoherentSheaf := CoherentSheaf => (F, G) -> CoherentSheaf.directSum(F, G)
CoherentSheaf  / CoherentSheaf := CoherentSheaf => (F, G) -> sheaf(F.variety, F.module  / G.module)
CoherentSheaf  / Ideal         := CoherentSheaf => (F, I) -> sheaf(F.variety, F.module  / I)
Ideal * CoherentSheaf          := CoherentSheaf => (I, F) -> sheaf(F.variety, I * F.module)
directSum CoherentSheaf        := CoherentSheaf =>  F     -> CoherentSheaf.directSum(1 : F)

components CoherentSheaf := List => F -> F.cache.components ??= apply(components module F, N -> sheaf(F.variety, N))
component(CoherentSheaf, Thing) := (F, k) -> (
    if not F.cache.?indexComponents then error "expected Sheaf to be a direct sum with indexed components";
    if not F.cache.indexComponents#?k then error("expected "|toString k|" to be the index of a component");
    (components F)#(F.cache.indexComponents#k))

-- tensor
tensor(CoherentSheaf, CoherentSheaf) := CoherentSheaf => {} >> opts -> (F, G) -> (
    sheaf(F.variety, tensor(F.module, G.module, opts)))
CoherentSheaf ** CoherentSheaf := CoherentSheaf => (F, G) -> sheaf(F.variety, F.module ** G.module)
CoherentSheaf^** ZZ            := CoherentSheaf => (F, n) -> sheaf(F.variety, F.module ^** n)

-- base change, extension of scalars, etc.
baseChange = method()
CoherentSheaf ** Ring         := baseChange(CoherentSheaf, Ring)         := CoherentSheaf => (F, R) -> baseChange(F, sheaf R)
CoherentSheaf ** SheafOfRings := baseChange(CoherentSheaf, SheafOfRings) := CoherentSheaf => (F, O) -> if O === ring F then F else tensor(F, O^1)
SheafOfRings ** CoherentSheaf := Ring ** CoherentSheaf := CoherentSheaf => (R, F) -> baseChange(F, R)
-- TODO: add ** RingMap

-- multilinear ops
-- TODO: document
determinant        CoherentSheaf  := CoherentSheaf => o ->     F  -> exteriorPower(rank F, F, o)
exteriorPower (ZZ, CoherentSheaf) := CoherentSheaf => o -> (i, F) -> sheaf(F.variety,  exteriorPower(i, F.module, o))
symmetricPower(ZZ, CoherentSheaf) := CoherentSheaf =>      (i, F) -> sheaf(F.variety, symmetricPower(i, F.module))

support     CoherentSheaf := Ideal =>      F -> annihilator module F
annihilator CoherentSheaf := Ideal => o -> F -> annihilator(module F, o)

-- printing
expression SheafOfRings := O -> Subscript { OO, expression O.variety }
net        SheafOfRings :=      net @@ expression
texMath    SheafOfRings :=  texMath @@ expression
toString   SheafOfRings := toString @@ expression

describe   CoherentSheaf := F -> Describe (Subscript { expression sheaf, expression F.variety }) (expression F.module)
expression CoherentSheaf := F -> (
    (X, M) := (variety F, module F);
    if M.?relations or M.?generators or numgens M === 0 then return SheafExpression expression M;
    -- in the affine case, we ignore the grading of M, if any
    if not isProjective X then return new Superscript from {expression OO_X, expression numgens M};
    --
    degs := runLengthEncoding(- degrees M); -- a list of O_X^r(d) for each summand
    sums := apply(degs, (r, d) -> (
	    s := new Superscript from {expression OO_X, expression r};
	    -- TODO: get rid of the extra space in OO_X^1 (1,2) when #d > 1
	    if all(d, zero) then s else new Adjacent from {
		s, (if #d == 1 then new Parenthesize from d else expression toSequence d)}));
    fold((a, b) -> a++b, sums))
net      CoherentSheaf :=      net @@ expression
texMath  CoherentSheaf :=  texMath @@ expression
toString CoherentSheaf := toString @@ expression

-- used to be in m2/mathml.m2
mathML SheafOfRings :=
mathML CoherentSheaf := lookup(mathML, Thing)

CoherentSheaf#AfterPrint = F -> ("coherent sheaf on ", variety F,
    if isFreeModule(M := module F)    then (", free of rank ",   rank F)    else
    if M.?generators and M.?relations then (", subquotient of ", ambient F) else
    if M.?generators                  then (", subsheaf of ",    ambient F) else
    if M.?relations                   then (", quotient of ",    ambient F)
    )

-- used to be in m2/expressions.m2
SheafExpression = new WrapperType of Expression;
toString'(Function, SheafExpression) := (fmt,x) -> toString'(fmt,new FunctionApplication from { sheaf, x#0 })
net SheafExpression := x -> net x#0
texMath SheafExpression := x -> texMath x#0
expressionValue SheafExpression := x -> sheaf expressionValue x#0

-----------------------------------------------------------------------------
-- Euler Characteristic
-----------------------------------------------------------------------------

euler SheafOfRings  := ZZ => O -> euler O^1
euler CoherentSheaf := ZZ => F -> tryHooks((euler, CoherentSheaf), F,
    F -> if isWeightedZZGraded(R := ring variety F) then (
	-- Compute the Euler characteristic of a coherent sheaf
	-- on a closed subspace of a weighted projective space.
	--
	-- The distinction between a WPS as a stack X and its associated
	-- coarse moduli space e: X -> V, does not matter for this purpose.
	-- Indeed, for a coherent sheaf F on the stack X, we have
	-- H^i(X, F) = H^i(V, e_*(F)) for every i.
	--
	-- If F = F0(twist), we compute chi(X, F0(twist)) and use F0 to cache
	(F0, twist) := if F.cache.?BaseTwist then F.cache.BaseTwist else (F, {0});
	-- TODO: is poincare polynomial the same before the pushforward?
	-- if so we just need M = module F0, certainly not saturation!
	M := currentModuleBaseRing F0; -- pushforward the module and kill m-torsion
	n := numgens R; -- dim X = n-1
	w := sum degrees R; -- n+sigma in Symoonds's notation
	hf := hilbertFunction R;
	eulerCharOfTwistingSheaf := c -> (
	    -- Compute the Euler characteristic of O(c) on a weighted projective space X,
	    -- for an integer c. Here O(c) can be viewed as a line bundle on the stack X.
	    -- This agrees with the Euler characteristic of the direct image sheaf O(c)
	    -- on the coarse moduli space of X, X -> Y. But be aware that the sheaves O(c)
	    -- on Y behave better when X = P^(n-1)(a_0,...,a_(n-1)) is well-formed, meaning
	    -- that gcd(a_0,...,a_j omitted,...,a_(n-1)) = 1 for each j. Namely, in that case,
	    -- O(c+d) = (O(c) ** O(d))^** is the reflexive tensor product of O(c) and O(d) on Y.
	    if  c <= -w then hilbertFunction(-c-w) * (-1)^(n-1) else
	    if {0} <= c then hilbertFunction(c)                 else 0);
	--
	-- We compute the Euler characteristic using the Betti numbers of a free resolution of M,
	-- which are captured in the terms of the poincare polynomial (i.e. numerator of its Hilbert series)
	sum(listForm poincare M, (e, c) -> c * eulerCharOfTwistingSheaf(twist - e))
	)
    )

-- if the ring is standard graded, this is much faster
addHook((euler, CoherentSheaf), Strategy => "StandardGraded",
    F -> if isStandardGraded ring variety F then euler module F)

-- the algorithm above also works in the non-standard graded case,
-- so we take advantage of it for eulr characteristic of modules.
addHook((euler, Module), Strategy => Varieties,
    M -> if M.ring.?variety then return try euler sheaf(M.ring.variety, M))

-- TODO: do these make sense if the ring isn't standard graded?
eulers SheafOfRings  := O -> eulers module O.ring
eulers CoherentSheaf := F -> eulers module F

euler(SheafOfRings,  ZZ, ZZ) := RingElement => (O, b1, b2) -> euler(O^1, b1, b2)
euler(CoherentSheaf, ZZ, ZZ) := RingElement => (F, b1, b2) -> (
    -- Compute the Euler characteristic of a coherent sheaf F
    -- on a closed subspace of a weighted projective space X
    -- in a range of integers [b1, b2] as a Laurant polynomial
    --   sum_(c=b1)^b2 chi(X, F(c)) T^c in the "degree ring" of R.
    R := assertWeightedZZGraded ring variety F;
    if b1 > b2 then error "expected a lower bound less than or equal to the upper bound";
    -- If F = F0(twist), we compute chi(X, F0(twist)) and use F0 to cache
    (F0, twist) := if F.cache.?BaseTwist then F.cache.BaseTwist else (F, {0});
    b1 = {b1} + twist; b2 = {b2} + twist;
    -- TODO: if indeed we can get away with poincare module F0 here,
    -- be sure to still use the Hilbert series of ambient R below, not R!
    M := currentModuleBaseRing F0; -- pushforward the module and kill m-torsion
    S := ring M;
    n := numgens R; -- dim X = n-1
    w := sum degrees R; -- n+sigma in Symoonds's notation
    A := degreesRing R; -- the output is a Laurant polynomial in this ring
    hp := poincare M;
    if hp == 0 then return 0_A;
    -- We want to compute chi(P, O(c)) for a range of integers c that includes what we need.
    -- We store this as a Laurent polynomial, sum_c chi(P, O(c)) T^c for finitely many c.
    beta := sort listForm hp; -- Betti numbers of M
    e1 := first first beta; -- lowest exponent in hp
    e2 := first last  beta; -- largest exponent in hp
    -- We need (at least) to compute chi(P, O(c)) for b1-r2 <= c <= b2-r1.
    -- To do that, we'll first compute chi(P, O(c)) for   0 <= c <= d
    d := max({0}, b2-e1, -(b1-e2)-w);
    hs := hilbertSeries(S, Order => d#0 + 1);
    hs' := hs + (-1)^(n-1) * A_(-w) * sub(hs, A_{1} => A_{-1});
    A_(-twist) * part(b1#0, b2#0, hs' * hp))

--------------------------------------------------------------------------------
-- Hilbert Polynomial
--------------------------------------------------------------------------------

importFrom_Core "hilbertFunctionRing"

-- The Hilbert polynomial of a weighted projective space (not the Hilbert function, despite the name).
-- Namely, for P = Proj R1 such that R1 is a polynomial ring with generators in degrees a_0,...,a_(n-1),
-- chi(P, O(i)) is a quasipolynomial in i:
--   chi(P, O(i)) = c_n(i) i^n + ... + c_0(i)
-- with each c_j(i) a periodic function of i, of period dividing lcm(a_0,...,a_(n-1)).
-- (Equivalently, h^0(X, O(i)) has this description for i sufficiently large.)
-- We define the Hilbert polynomial f(i) as the polynomial in QQ[i] obtained by _averaging_ each of these coefficients.

hilbertFunctionQ = method()
hilbertFunctionQ Ring := RingElement => R -> (
    R.cache ??= new MutableHashTable;
    if R.cache.?hilbertPolynomial then return R.cache.hilbertPolynomial;
    if not isPolynomialRing R then error "For hilbertFunctionQ, the ring must be a polynomial ring.";
    degs := flatten degrees R; -- R should be singly graded.
    n := #degs; -- So the weighted projective space Proj R has dimension n-1.
    prod := lcm degs; -- The lcm of the weights.
    -- It suffices to compute the dimension of R_(j.prod+a) for j=0,1,...,n-1 and 0 <= a <= prod-1.
    truncatedseries := hilbertSeries(R, Order => n*prod); -- The Hilbert series is in a ring ZZ[T].
    T := (ring truncatedseries)_0;
    ringi := hilbertFunctionRing();
    i := ringi_0;
    wpoly := 0; k := 0; polylist := {}; value := 0; newcoeff := 0; evj := 0; interpolant := 0; total := 0;
    for a from 0 to prod-1 do ( -- Compute the polynomial of degree n-1 that interpolates dim R_(j.prod+a) for j=0,...,n-1.
	interpolant = coefficient(T^a, truncatedseries)*1_ringi;
	-- This is the polynomial of degree 0 that interpolates at 0.prod+a. We use Newton interpolation
	-- to construct the polynomial of degree j that interpolates at 0*prod+a,1*prod+a,...,j*prod+a, for j up to n-1.
	for j from 1 to n-1 do (
	    value = coefficient(T^(j*prod+a), truncatedseries);
	    polylist = apply(j, k -> i-k*prod-a); -- This gives the list {i-a, i-1*prod-a, ..., i-(j-1)*prod-a} in QQ[i].
	    wpoly = product polylist; -- This gives the polynomial (i-a)(i-1*prod-a)...(i-(j-1)*prod-a) in QQ[i].
	    evj = map(QQ, ringi, {j*prod+a});
	    newcoeff = (value - evj interpolant)/(evj wpoly);
	    interpolant = interpolant + newcoeff*wpoly);
	total=total+interpolant
	);
    -- Now average the polynomials "interpolant"; the number of them was "prod".
    R.cache.hilbertPolynomial = total/prod)

hilbertFunctionQ(Ring, ZZ) := memoize(
    (R, d) -> (
	if d === 0 then hilbertFunctionQ(R)
	else (
	    i := (hilbertFunctionRing())_0;
	    substitute(hilbertFunctionQ(R), {i => i+d}))))

-- c.f. the toric version in NormalToricVariety/Chow.m2
-- and the original definition and hook in m2/hilbert.m2
addHook((hilbertPolynomial, Module), Strategy => Varieties, (opts, M) ->
    if not isStandardGraded M.ring and M.ring.?variety
    then return try hilbertPolynomial(M.ring.variety, M, opts))
hilbertPolynomial(Variety, Module)        := o -> (X, M) -> hilbertPolynomial(X, sheaf(X, M), o)

-- these are defined for any Variety, including NormalToricVariety, etc.
hilbertPolynomial(Variety, Ring)          := o -> (X, S) -> hilbertPolynomial(X, module S, o)
hilbertPolynomial(Variety, Ideal)         := o -> (X, I) -> hilbertPolynomial(X, comodule I, o)
hilbertPolynomial(Variety, CoherentSheaf) := o -> (X, F) -> (
    error "variety does not have a method for computing Hilbert polynomial")
hilbertPolynomial          CoherentSheaf  := o ->     F  -> hilbertPolynomial(F.variety, module F, o)
-- FIXME: what definition are these based on?
hilbertPolynomial(Variety, SheafOfRings)  := o -> (X, O) -> degree O^1
hilbertPolynomial          SheafOfRings   := o ->     O  -> degree O^1

hilbertPolynomial(ProjectiveVariety, CoherentSheaf) := opts -> (X, F) -> (
    -- The Hilbert polynomial of a closed subspace X of a weighted projective space,
    -- or of a coherent sheaf F on X. Namely, for X = Proj R such that R has generators
    -- in degrees a_0,...,a_(n-1), chi(X, F(i)) is a quasipolynomial in i:
    --   chi(X, F(i)) = c_m(i) i^m + ... + c_0(i)
    -- with each c_j(i) a periodic function of i with period dividing lcm(a_0,...,a_(n-1)).
    -- (Equivalently, h^0(X, F(i)) has this description for i sufficiently large.)
    -- Note that the twist F(i) is defined by tensoring with the line bundle O(i)
    -- on X as a stack, when the weights a_j are not all 1.
    -- We define the Hilbert polynomial f(i) as the polynomial in QQ[i] obtained
    -- by _averaging_ each of the coefficients c_j(i).
    -- (If the weights a0,...,a_(n-1) are equal to 1, then the default is Projective => true,
    -- meaning that the output is given as a ProjectiveHilbertPolynomial, that is, a ZZ-linear combination
    -- of the Hilbert polynomials of projective spaces of dimensions 0,...,m.)
    --
    -- For X of dimension m, the Hilbert polynomial in QQ[i] has degree m,
    -- and its leading coefficient is degree(X) / m!; this agrees with the function "degree X".
    -- Note that the degree of a closed subspace in a weighted projective space is only a rational number.
    -- For example, PP^n(a_0,...,a_n) has degree 1/(a_0...a_n).
    R := ring X;
    z := degree 1_R;
    -- If F = F0(twist), we compute HP(X, F0(twist)) and use F0 to cache
    (F0, twist) := if F.cache.?BaseTwist then F.cache.BaseTwist else (F, z);
    -- if R is standard graded, Core's algorithm is probably faster
    hp := if isStandardGraded R then hilbertPolynomial(module F0, Projective => false)
    -- otherwise, we compute the Hilbert polynomial of F0 in QQ[i] and cache it
    else F0.cache.hilbertPolynomial ??= (
	M := currentModuleBaseRing F0; -- pushforward the module and kill m-torsion
	A := hilbertFunctionRing();
	S := ring M;
	-- FIXME: make this last piece multigraded friendly
	0_A + sum(listForm poincare M, (e, c) ->
	    c * hilbertFunctionQ(S, -e#0)));
    -- twist according to HP(F, i) = HP(F0(twist), i) = HP(F0, i+twist)
    if twist != z then hp = sub(hp, matrix { gens ring hp + twist });
    -- TODO: it's awkward that the option gets ignored depending on the grading input
    if opts.Projective and isStandardGraded R then projectiveHilbertPolynomial hp else hp)

-----------------------------------------------------------------------------
-- SumOfTwists type declarations and basic constructors
-----------------------------------------------------------------------------

-- used as a bound for sums of twists
LowerBound = new SelfInitializingType of BasicList
-- TODO: implement for multigraded twists
>  InfiniteNumber := >  ZZ            := LowerBound => b -> LowerBound{b+1}
>= InfiniteNumber := >= ZZ := >= List := LowerBound => b -> LowerBound{b}

SumOfTwists = new Type of BasicList
SumOfTwists.synonym = "sum of twists"

-- constructors
SheafOfRings(*)  := SumOfTwists => O -> O^1(>=-infinity)
CoherentSheaf(*) := SumOfTwists => F ->   F(>=-infinity)
SheafOfRings  LowerBound := SumOfTwists => (O, b) -> O^1(b)
CoherentSheaf LowerBound := SumOfTwists => (F, b) -> (checkProjective variety F; new SumOfTwists from {F, b})

-- basic methods
ring    SumOfTwists := S ->    ring S#0
variety SumOfTwists := S -> variety S#0

-- printing
expression SumOfTwists := S -> (expression S#0) (if S#1#0 === -infinity then expression symbol(*) else (expression symbol>=) (expression S#1#0))
net        SumOfTwists :=      net @@ expression
texMath    SumOfTwists :=  texMath @@ expression
toString   SumOfTwists := toString @@ expression

-----------------------------------------------------------------------------
-- cotangentSheaf, tangentSheaf, and canonicalSheaf
-----------------------------------------------------------------------------
-- TODO: this might be relevant for weighted projective spaces, see c564ec04
-- weightedVars = S -> (
--      map(S^1, S^-(degrees S), {apply(generators S, flatten degrees S, times)})
--      )

--- TODO: remove by M2 1.27
canonicalBundleWarn = true
canonicalBundle = x -> (if canonicalBundleWarn then (canonicalBundleWarn = false;
	printerr "Note: canonicalBundle is deprecated; use canonicalSheaf instead."); canonicalSheaf x)

-- TODO: remove MinimalGenerators option and let user call prune?
cotangentSheaf = method(TypicalValue => CoherentSheaf,
    Options => options exteriorPower ++ { MinimalGenerators => true })
cotangentSheaf ProjectiveVariety := opts -> X -> X.cache#(symbol cotangentSheaf => opts) ??= (
    -- This function computes the cotangent sheaf of a closed subscheme of a projective scheme
    -- over a base ring.
    --
    -- More generally, cotangentSheaf X works for X a closed subspace of a weighted projective space, viewed as a stack.
    -- For example, if the coarse moduli space of X is quasi-smooth and well-formed,
    -- then the pushforward of this sheaf to the coarse moduli space is the sheaf of reflexive differentials.
    -- If X is normal but not quasi-smooth, use the command "reflexiveDifferentials" instead,
    -- if you want the sheaf of reflexive differentials.
    --
    -- If the characteristic p is positive and p divides some of the weights, the WPS (and its substacks)
    -- need not be Deligne-Mumford stacks. In that situation, this program returns the cohomology sheaf
    -- in degree 0 of the cotangent complex.
    -- In that situation, it would be more natural to consider the truncation of the cotangent complex
    -- to cohomological degrees >= 0 (which lives in degrees 0 and 1), given by naiveCotangentComplex(X).
    R := assertWeightedZZGraded ring X;
    -- Here R is a graded ring with some positive integer grading,
    -- and X = Proj R is the corresponding subspace of a weighted projective space.
    S := ring (F := presentation R);
    -- Thus S is a graded polynomial ring, and R is the quotient of S by the ideal generated by the image of the matrix F over S.
    -- For example, S could be something like: S = QQ[x,y,z,w,Degrees=>{1,9,15,22}].
    degs := degrees S; -- This is a list of the form {a_0,...,a_(n-1)}.
    n := #degs; -- So P = Proj S has dimension n-1.
    M0 := R^(-degs);
    M1 := R^{0};
    d1 := matrix {apply(gens R, i -> first degree(i)*i)};
    -- Thus d: R^n -> R sends the basis element dx_i to a_i x_i, for 0 <= i <= n-1.
    d := map(M1, M0, d1, Degree => 0); -- This is d viewed as being homogeneous.
    e := jacobian F ** R; -- Thus e: R^r -> R^n sends the jth basis element (corresponding to the jth relation
    -- of f_1,...,f_r) to df_j = sum_{i=0}^(n-1) df_j/dx_i dx_i. It is viewed as being homogeneous.
    --assert(isHomogeneous d);
    --assert(isHomogeneous e);
    --assert(d * e == 0);
    om := sheaf homology(d,e);
    -- Here om is the cotangent sheaf of X. By default, we simplify its description, as follows.
    if opts.MinimalGenerators then minimalPresentation om else om)

cotangentSheaf AffineVariety := opts -> X -> X.cache#(symbol cotangentSheaf => opts) ??= (
    -- This function computes the cotangent sheaf of an affine scheme over a base ring.
    R := ring X;
    -- So X = Spec R.
    S := ring (F := presentation R);
    -- Thus S is a polynomial ring, and R is the quotient of S by the ideal (f_1,...,f_r)
    -- generated by the image of the matrix F over S.
    n := numgens S; -- So Y = Spec S has dimension n.
    M0 := R^n;
    e := jacobian F ** R; -- Thus e: R^r -> R^n sends the jth basis element (corresponding to the jth relation
    -- of f_1,...,f_r) to df_j = sum_{i=1}^n df_j/dx_i dx_i.
    om := sheaf(X, cokernel e);
    -- The module om represents the cotangent sheaf of X. By default, we simplify its description, as follows,
    -- although there is not a precise notion of minimal presentations in this ungraded situation.
    if opts.MinimalGenerators then minimalPresentation om else om)

cotangentSheaf(ZZ, Variety) := opts -> (i, X) -> (
    -- Computes the sheaf of i-forms on a variety over a base ring.
    -- Another possible definition would be: HH^0 naiveCotangentComplex(i, X, opts))
    -- These are the same if p does not divide any of the weights, in particular if all the weights are 1.
    prune' := if opts.MinimalGenerators then minimalPresentation else identity;
    prune' exteriorPower(i, cotangentSheaf(X, opts), Strategy => opts.Strategy))

tangentSheaf = method(TypicalValue => CoherentSheaf, Options => options cotangentSheaf)
tangentSheaf Variety := opts -> X -> (
    prune' := if opts.MinimalGenerators then minimalPresentation else identity;
    sheaf_X prune' module dual cotangentSheaf(X, opts))

idealSheaf = method(TypicalValue => CoherentSheaf)
idealSheaf Variety := X -> sheaf ideal (ring X).relations

canonicalSheaf = method(TypicalValue => CoherentSheaf, Options => options cotangentSheaf)
canonicalSheaf Variety := opts -> X -> (
    prune' := if opts.MinimalGenerators then minimalPresentation else identity;
    if isProjective X and not isStandardGraded ring X then omega1 := (naiveCotangentComplex(X, opts))_0
    else omega1 = cotangentSheaf(X, opts);
    sheaf_X prune' module dual dual determinant(omega1, Strategy => opts.Strategy))
-- In the weighted case, the determinant of the naive cotangent complex C_0 -> C_(-1)
-- is the determinant of C_0 (as computed here), since C_(-1) = O_X.

-- This function computes the sheaf of reflexive differentials of a given closed substack X of a weighted projective space.
-- At least when X is normal and well-formed (not necessarily quasi-smooth), its direct image sheaf
-- on the coarse moduli space Y of X is the sheaf of reflexive differentials on Y.
reflexiveDifferentials = method(TypicalValue => CoherentSheaf,
    Options => options exteriorPower ++ { MinimalGenerators => true })
reflexiveDifferentials Variety := opts -> X -> reflexiveDifferentials(1, X, opts)

-- This function computes the sheaf of reflexive i-forms (for i>=0) on a given closed substack X of a weighted projective space.
-- At least when X is normal and well-formed (not necessarily quasi-smooth), its direct image sheaf
-- on the coarse moduli space Y of X is the sheaf of reflexive i-forms on Y.
reflexiveDifferentials(ZZ, Variety) := opts -> (i, X) -> (
    prune' := if opts.MinimalGenerators then minimalPresentation else identity;
    sheaf_X prune' module dual dual exteriorPower(i, cotangentSheaf(X, opts), Strategy => opts.Strategy))

dualizingSheaf = method(TypicalValue => CoherentSheaf, Options => options cotangentSheaf)

-- For a scheme X of finite type over a field k, the dualizing complex omega_X^{\bullet} = f^!(O_(Spec k))
-- is concentrated in cohomological degrees at least -n, where n = dim X. We compute here H^(-n)(omega_X^{\bullet}),
-- known as the dualizing sheaf of X. Its support is the union of the n-dimensional irreducible components
-- of X. The output is also reasonable for X a closed subspace of a weighted projective space,
-- viewed as a stack.
dualizingSheaf ProjectiveVariety := opts -> X -> (
    prune' := if opts.MinimalGenerators then minimalPresentation else identity;
    A := ring X;
    if degreeLength A =!= 1 then error "expected degree length 1";
    S := ring presentation A; -- This is a graded polynomial ring.
    degs := degrees S; -- This is a list of the form {{1},{9},{15},{22}}, say.
    N := -1 + #degs; -- So P = Proj S has dimension N.
    sumOfWeights := sum degs; -- This is sum_i |x_i|, where S = k[x_0,...,x_(n-1)].
    S.cache ??= new MutableHashTable;
    w := S.cache.Dualizing ??= S^{-sumOfWeights};
    -- We fix the dualizing module w, as a graded S-module.
    sheaf_X prune'(Ext^(N-dim X)(cokernel presentation A,w) ** A))

dualizingSheaf AffineVariety := opts -> X -> (
    prune' := if opts.MinimalGenerators then minimalPresentation else identity;
    A := ring X;
    S := ring presentation A; -- This is a polynomial ring.
    N := numgens S; -- So Spec S is affine N-space.
    sheaf_X prune'(Ext^(N-dim X)(cokernel presentation A,S) ** A))

-----------------------------------------------------------------------------
-- isLocallyFree
-----------------------------------------------------------------------------

-- Check whether an ideal I is a factor of a ring R, meaning that R = I x J as rings for some J.
-- Since R is noetherian, it is equivalent to check whether I = I^2.
isFactor = I -> isSubset(I,I^2)

isLocallyFree = method(TypicalValue => Boolean)
isLocallyFree SumOfTwists   := S -> isLocallyFree S#0
isLocallyFree SheafOfRings  := O -> true

-- Check whether an module M over a ring R is locally free. Here M is finitely generated
-- and R is noetherian; so it is equivalent to check whether M is projective, or flat.
-- We do not require R to be a domain; so M may have different ranks on different components
-- of Spec R.
isLocallyFree Module := Boolean => M -> (
    -- M is locally free if and only if all its Fitting ideals are factors of R = ring M.
    -- Let us first check whether M is locally free of constant rank, the simplest case.
    if isFreeModule M then return true;
    rankM := rank M; -- This number is only guaranteed to be reasonable if R is a domain.
    if instance(rankM, ZZ) and rankM > 0 then (
	if isMember(1, J:=fittingIdeal(rankM, M)) and fittingIdeal(rankM - 1, M) == 0 then return true;
	if not isFactor J then return false); -- These cover all cases if R is a domain,
    -- which should be the most common case. Otherwise, we now check all Fitting ideals.
    persist := true; i := 0;
    while persist do (
	J = fittingIdeal(i, M);
	if isMember(1, J) then persist = false;
	if not isFactor J then return false;
	i = i+1);
    true)

-- Check whether a coherent sheaf F is locally free (that is, a vector bundle).
-- The following function works if X = variety F  is affine,
-- or if X is projective or weighted-projective over a field. We do not require X to be integral;
-- so F may have different ranks on different components of X.
isLocallyFree CoherentSheaf := F -> (
    M := module F;
    if not isProjective variety F then return isLocallyFree M;
    -- Now X = variety F is projective. We assume that X = Proj R with the ring R singly graded.
    -- Then F is locally free if and only if each of its Fitting ideals I has I equal to I^2
    -- outside the origin of Spec R, that is, if I is contained in the saturation of I^2.
    if isFreeModule M then return true;
    rankM := rank M; -- This number is only guaranteed to be reasonable if X is integral.
    if instance(rankM, ZZ) then (
	if rankM == 0 then (if dim M <= 0 then return true)
	else (
	    if dim(J := fittingIdeal(rankM, M)) <= 0 and fittingIdeal(rankM - 1, M) == 0 then return true;
	    if not isSubset(J, saturate J^2) then return false)); -- These cover all cases if R is a domain,
    -- which should be the most common case. Otherwise, we now check all Fitting ideals.
    persist := true; i := 0;
    while persist do (
	J = fittingIdeal(i, M);
	if dim J <= 0 then persist = false;
	if not isSubset(J, saturate J^2) then return false;
	i = i+1);
    true)
