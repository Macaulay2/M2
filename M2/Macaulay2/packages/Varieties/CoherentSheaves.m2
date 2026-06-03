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
-- TODO: sheaf Ring and sheaf Module should return a sheaf over variety of the ring rather than Proj,
-- and if a variety doesn't already exist then either Proj or Spec should be defined and cached.
sheaf Ring := Ring^~ := SheafOfRings =>     R  -> sheaf(variety R, R)
sheaf Variety        := SheafOfRings =>  X     -> sheaf(X, ring X)

sheaf(Variety, Ring) := SheafOfRings => (X, R) -> (
    if ring X =!= R then error "sheaf: expected ring of the variety";
    -- TODO: simplify when https://github.com/Macaulay2/M2/issues/3351 is fixed
    X.sheaf ??= (
	O := new SheafOfRings from { symbol variety => X, symbol ring => R };
	O.cache = new MutableHashTable;
	O.cache.sheaf = sheaf(O.variety, (ring variety O)^1); -- That is, O.cache.sheaf is the CoherentSheaf O^1.
	-- We cache this so that constructing O^1 at different times will yield the _same_ CoherentSheaf,
	-- which itself may have cached information over time.
	O)
    )


-- TODO: should the module of a sheaf be fixed, or should it be allowed to change?
-- TODO: https://github.com/Macaulay2/M2/issues/1358
sheaf Module := Module^~ := CoherentSheaf =>     M  -> sheaf(variety ring M, M)
sheaf(Variety, Module)   := CoherentSheaf => (X, M) -> (
    if M.cache#?(sheaf, X) then return M.cache#(sheaf, X);
    M.cache#(sheaf, X) = (
	if ring M =!= ring X then error "sheaf: expected module and variety to have the same ring";
	if instance(X, ProjectiveVariety) and not isHomogeneous M then error "sheaf: expected a homogeneous module";
	new CoherentSheaf from {
	    symbol variety => X,
	    symbol module => M,
	    symbol cache => new CacheTable
	    }
	))

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
module SheafOfRings  := Module => O -> module O.cache.sheaf
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
    if F.cache.?Twist then F = F.cache.Twist#1;
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
    if F.cache.?Twist then F = F.cache.Twist#1;
    M := currentModuleBaseRing F;
    R := assertWeightedZZGraded ring M;
    -- The degree is an integer if all weights are equal to 1, but otherwise a rational number.
    -- For example, on X = PP^n(a_0,...,a_n), the structure sheaf OO_X has degree 1/(a_0...a_n).
    -- This is compatible with the function hilbertPolynomial F.
    if isStandardGraded R then degree module F
    else degree M / product flatten degrees R)

genus   CoherentSheaf := F -> genus   module F
genera  CoherentSheaf := F -> genera  module F
-- TODO: this is incorrect in higher picard rank
pdim    CoherentSheaf := F -> tryHooks((pdim,  CoherentSheaf), F, pdim  @@ module)

-- c.f. the toric version in NormalToricVariety/Chow.m2
-- and the original definition and hook in m2/hilbert.m2
addHook((hilbertPolynomial, Module), Strategy => Varieties, (opts, M) ->
    if M.ring.?variety then return try hilbertPolynomial(M.ring.variety, M, opts))
hilbertPolynomial(Variety, Module)        := o -> (X, M) -> error "variety does not have a method for computing Hilbert polynomial"
-- TODO: should these be only for ProjectiveVariety and error for affine variety?
hilbertPolynomial(Variety, Ring)          := o -> (X, S) -> hilbertPolynomial(X, module S, o)
hilbertPolynomial(Variety, Ideal)         := o -> (X, I) -> hilbertPolynomial(X, comodule I, o)
hilbertPolynomial(Variety, SheafOfRings)  := o -> (X, O) -> hilbertPolynomial(X, module O, o)
hilbertPolynomial(Variety, CoherentSheaf) := o -> (X, F) -> hilbertPolynomial(X, module F, o)
hilbertPolynomial          CoherentSheaf  := o ->     F  -> hilbertPolynomial(module F, o)

-- twist and powers
-- TODO: sheaf should dehomogenize modules on Affine varieties
SheafOfRings(ZZ)   := SheafOfRings  Sequence := CoherentSheaf => (O, a) -> O^1(a)
CoherentSheaf(ZZ)  := CoherentSheaf Sequence := CoherentSheaf => (F, a) -> F ** (ring F)^{splice{a}}

SheafOfRings  ^ ZZ := SheafOfRings  ^ List   := CoherentSheaf => (O, n) -> (
    if instance(n, ZZ) and n == 1 then O.cache.sheaf -- Thus, O^1 always yields the _same_ coherent sheaf, which may contain cached information.
    -- We could consider transferring that information to direct sums, but at the moment that is not done.
    else sheaf(O.variety, (ring variety O)^n))

CoherentSheaf ^ ZZ := CoherentSheaf ^ List   := CoherentSheaf => (F, n) -> sheaf(F.variety, F.module^n)
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
CoherentSheaf ** CoherentSheaf := CoherentSheaf => (F, G) -> sheaf(F.variety, F.module ** G.module)
CoherentSheaf^** ZZ            := CoherentSheaf => (F, n) -> sheaf(F.variety, F.module ^** n)
tensor(CoherentSheaf, CoherentSheaf) := CoherentSheaf => {} >> opts -> (F, G) -> sheaf(F.variety, tensor(F.module, G.module, opts))
CoherentSheaf  / CoherentSheaf := CoherentSheaf => (F, G) -> sheaf(F.variety, F.module  / G.module)
CoherentSheaf  / Ideal         := CoherentSheaf => (F, I) -> sheaf(F.variety, F.module  / I)
Ideal * CoherentSheaf          := CoherentSheaf => (I, F) -> sheaf(F.variety, I * F.module)
directSum CoherentSheaf        := CoherentSheaf =>  F     -> CoherentSheaf.directSum(1 : F)

components CoherentSheaf := List => (cacheValue symbol components) (F -> apply(components module F, N -> sheaf(F.variety, N)))
component(CoherentSheaf, Thing) := (F, k) -> (
    if not F.cache.?indexComponents then error "expected Sheaf to be a direct sum with indexed components";
    if not F.cache.indexComponents#?k then error("expected "|toString k|" to be the index of a component");
    (components F)#(F.cache.indexComponents#k))

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
-- Hilbert polynomial, Euler characteristic, etc
-----------------------------------------------------------------------------

-- Compute the Euler characteristic of O(c) on a weighted projective space P, for an integer c.
-- Here O(c) can be viewed as a line bundle on the stack P. This agrees with the Euler characteristic of the direct image sheaf O(c)
-- on the coarse moduli space of P, P -> Y. (But be aware that the sheaves O(c) on Y behave better
-- when P = P^(n-1)(a_0,...,a_(n-1)) is well-formed, meaning that gcd(a_0,...,a_j omitted,...,a_(n-1)) = 1 for each j.
-- Namely, in that case, O(c+d) on Y is the reflexive tensor product of O(c) and O(d), meaning that O(c+d) = (O(c) tensor O(d))^**.)
-- The input is the integer c and a positively graded polynomial ring R1, with P = Proj(R1).
--
eulerCharOfTwistingSheaf = (c,R1) -> (
    degs := flatten degrees R1; -- This is a list of the form {1,9,15,22}.
    n := #degs; -- So P = Proj R1 has dimension n-1.
    sumOfWeights := fold(plus, degs); -- This is the sum of the weights, that is, n+sigma in Symonds's notation,
    -- for P = P^{n-1}(a_0,...,a_(n-1)).
    hilbshort := 0;
    A := degreesRing R1; -- This will be a ring of the form Z[T].
    T := A_0; -- This is the variable in the ring A.
    if c >= 0 then (
	hilbshort = hilbertSeries(R1, Order=>c+1); -- This is the Hilbert series of P in degrees at most c, as a polynomial.
	coefficient(T^c, hilbshort)
	)
    else (
	if c > -sumOfWeights then 0
	else (
	    d := -c-sumOfWeights; -- We have d >= 0.
	    hilbshort = hilbertSeries(R1, Order=>d+1);
	    (-1)^(n-1)*coefficient(T^d, hilbshort)
	    )
	)
    )

-- Compute the Euler characteristic of a coherent sheaf on a closed subspace of a weighted projective space.
--
-- The distinction between a WPS as a stack X and its associated coarse moduli space, e: X -> V, does not matter
-- for this purpose. Indeed, for a coherent sheaf F on the stack X, we have H^i(X, F) = H^i(V, e_*(F)) for every i.
--
-- TODO: should be hookified again
euler SheafOfRings  := ZZ => O -> euler O^1
euler CoherentSheaf := ZZ => F -> (
    -- Compute the Euler characteristic chi(X, F) for a coherent sheaf on a closed subspace X of a weighted projective space.
    shift := 0;
    if F.cache.?twist then
    (shift = first F.cache.twist#0;
	F = F.cache.twist#1);
    -- Thus we reduce to the case where the sheaf F was not defined as a twist. We now compute chi(X, F(shift)).
    M := currentModuleBaseRing F;
    R1 := ring M; -- R1 is a graded polynomial ring, and M is a simplified R1-module that represents the sheaf F.
    -- In particular, we have arranged that M has no m-torsion (where m is the maximal ideal R1_(>0)).
    -- Note that even if R1 is standard-graded, we would need euler(M(shift)) (in general), rather than euler(M).
    -- To use the cached information about M, we do not form M(shift) explicitly.
    if isStandardGraded R1 and shift == 0 then return euler M; -- The earlier algorithm should be faster, in the usual projective space.
    if degreeLength R1 =!= 1 then error "euler expected the ring to be singly graded";
    numerator := poincare M; -- Thus the Hilbert series of M is: numerator/((1-a_0)...(1-a_(n-1)), where a_0,...,a_(n-1) are the weights.
    -- This numerator is in ZZ[T], meaning Z[T,T^(-1)].
    termlist := terms numerator; -- E.g, if numerator = T^(-1)-T^5, then termlist = {T^(-1),-T^5}.
    thisterm := 0;
    thisdeg := 0;
    len := #termlist;
    i := 0;
    output := 0;
    for i from 0 to len-1 do (
	thisterm = termlist_i; -- This could be of the form -T^5, say.
	thisdeg = (degree(thisterm))_0; -- Here degree (-T^5) is a list with one element, {5}, and we just want that number.
	output = output+leadCoefficient(thisterm)*eulerCharOfTwistingSheaf(shift-thisdeg,R1));
    output)
-- TODO: should this call assertStandardGraded?
eulers CoherentSheaf := F -> eulers module F

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
-- cotangentSheaf, tangentSheaf, and canonicalBundle
-----------------------------------------------------------------------------
-- TODO: make this work for weighted projective spaces, see c564ec04
-- this would be useful for checking things about mirror symmetry
-- weightedVars = S -> (
--      map(S^1, S^-(degrees S), {apply(generators S, flatten degrees S, times)})
--      )

-- TODO: this is the slowest part of hh and euler, look into other strategies
-- TODO: simplify caching here and in minimalPresentation
cotangentSheaf = method(TypicalValue => CoherentSheaf, Options => options exteriorPower ++ { MinimalGenerators => true })
cotangentSheaf ProjectiveVariety := opts -> (cacheValue (symbol cotangentSheaf => opts)) (X -> (
	R := ring X; checkRing R;
	S := ring(F := presentation R);
	(d, e) := (vars S ** R, jacobian F ** R); -- assert(d * e == 0);
	om := sheaf(X, homology(d, e));
	if opts.MinimalGenerators
	then minimalPresentation om else om))
cotangentSheaf(ZZ, ProjectiveVariety) := opts -> (i, X) -> exteriorPower(i, cotangentSheaf(X, opts), Strategy => opts.Strategy)

tangentSheaf = method(TypicalValue => CoherentSheaf, Options => options cotangentSheaf)
tangentSheaf ProjectiveVariety := opts -> X -> dual cotangentSheaf(X, opts)

idealSheaf = method(TypicalValue => CoherentSheaf, Options => options cotangentSheaf)
idealSheaf ProjectiveVariety := opts -> X -> sheaf ideal (ring X).relations

-- TODO: document
canonicalBundle = method(TypicalValue => CoherentSheaf, Options => options cotangentSheaf)
canonicalBundle ProjectiveVariety := opts -> X -> dual dual determinant(cotangentSheaf(X, opts), Strategy => opts.Strategy)

-----------------------------------------------------------------------------
-- isLocallyFree
-----------------------------------------------------------------------------

isLocallyFree = method(TypicalValue => Boolean)
isLocallyFree SumOfTwists   := S -> isLocallyFree S#0
isLocallyFree SheafOfRings  := O -> true
isLocallyFree CoherentSheaf := F -> (
    if (d := rank F) == 0 then return F == 0;
    if isFreeModule module F then return true;
    dim fittingIdeal(d,   module F) <= 0
    and fittingIdeal(d-1, module F) == ideal 0_(ring variety F))
