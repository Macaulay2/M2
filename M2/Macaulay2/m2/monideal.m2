-- Copyright 1995-2002 by Michael Stillman

needs "hilbert.m2" -- for poincare
needs "matrix2.m2"

-----------------------------------------------------------------------------
-- MonomialIdeal type declaration and basic constructors
-----------------------------------------------------------------------------

MonomialIdeal = new Type of Ideal
MonomialIdeal.synonym = "monomial ideal"

newMonomialIdeal = (R, rawI) -> new MonomialIdeal from {
    symbol ring    => R,
    symbol numgens => rawNumgens rawI,
    symbol RawMonomialIdeal => rawI,
    symbol cache => new CacheTable,
    }

monomialIdealOfRow := (i, m) -> newMonomialIdeal(ring m, rawMonomialIdeal(raw m, i))

monomialIdeal = method(TypicalValue => MonomialIdeal, Dispatch => Thing)
monomialIdeal Matrix := f -> (
    if not isCommutative ring f    then error "expected a commutative ring";
    if not isPolynomialRing ring f then error "expected a polynomial ring without quotient elements";
    monomialIdealOfRow(0, flatten f))

monomialIdeal Ideal  := I -> monomialIdeal generators gb I
monomialIdeal Module := M -> monomialIdeal ideal M
monomialIdeal MonomialIdeal := identity

monomialIdeal List     := v -> monomialIdeal matrix {splice v}
monomialIdeal Sequence := v -> monomialIdeal toList v
monomialIdeal RingElement := v -> monomialIdeal {v}

MonomialIdeal#1 = I -> monomialIdeal 1_(ring I)
MonomialIdeal ^ ZZ    := MonomialIdeal => BinaryPowerMethod
MonomialIdeal ^ Array := MonomialIdeal => (I, e) -> monomialIdeal (ideal I)^e

MonomialIdeal + MonomialIdeal := MonomialIdeal => ((I, J) -> newMonomialIdeal(ring I, raw I + raw J)) @@ samering
MonomialIdeal * MonomialIdeal := MonomialIdeal => ((I, J) -> newMonomialIdeal(ring I, raw I * raw J)) @@ samering
MonomialIdeal - MonomialIdeal := MonomialIdeal => ((I, J) -> newMonomialIdeal(ring I, raw I - raw J)) @@ samering

MonomialIdeal * Ring := MonomialIdeal => (I, S) -> if ring I === S then I else monomialIdeal(generators I ** S)
Ring * MonomialIdeal := MonomialIdeal => (S, I) -> I ** S

RingElement * MonomialIdeal := ZZ * MonomialIdeal := MonomialIdeal => (r, I) -> monomialIdeal(r * generators I)

-----------------------------------------------------------------------------
-- Basic methods (specifically those which are distinct from Ideal)
-----------------------------------------------------------------------------
-- TODO: is degree(MonomialIdeal) faster with 'poincare' or with degree(Ideal)?

raw MonomialIdeal := I -> I.RawMonomialIdeal
ideal MonomialIdeal := I -> ideal generators I

numgens MonomialIdeal := I -> I.numgens

-- monomial ideals are trimmed by construction (c.f the == method below)
trim       MonomialIdeal := MonomialIdeal => o -> identity
mingens    MonomialIdeal := MonomialIdeal => o -> I -> sort generators I
-- FIXME: for Ideal, this is cached in I.generators
generators MonomialIdeal := o -> I -> I.cache.generators ??= map(ring I, rawMonomialIdealToMatrix raw I)

-- arithmetic operations
Matrix %  MonomialIdeal := Matrix => (f, I) -> f %  forceGB generators I
Matrix // MonomialIdeal := Matrix => (f, I) -> f // forceGB generators I

RingElement %  MonomialIdeal := ZZ %  MonomialIdeal := RingElement => (r, I) -> r_(ring I) %  forceGB generators I
RingElement // MonomialIdeal := ZZ // MonomialIdeal := RingElement => (r, I) -> r_(ring I) // forceGB generators I

MonomialIdeal == MonomialIdeal := (I, J) -> I === J
MonomialIdeal == ZZ := (I, i) -> (
    if i === 0 then numgens I == 0 else
    if i === 1 then 1 % I == 0     else
    error "attempted to compare monomial ideal to nonzero integer")
ZZ == MonomialIdeal := (i, I) -> I == i

isMonomialIdeal = method(TypicalValue => Boolean)
isMonomialIdeal Thing         := x -> false
isMonomialIdeal Ideal         := I -> isPolynomialRing ring I and all(I_*, r -> size r === 1 and leadCoefficient r == 1)
isMonomialIdeal Module        := M -> isIdeal M and isMonomialIdeal ideal M
isMonomialIdeal MonomialIdeal := I -> true

-- We use E. Miller's definition for non-square free monomial ideals.
isSquareFree = method(TypicalValue => Boolean)		    -- could be isRadical?
isSquareFree Module        :=
isSquareFree Ideal         := I -> isMonomialIdeal I and isSquareFree monomialIdeal I
isSquareFree MonomialIdeal := I -> all(I_*, m -> all(first exponents m, i -> i < 2))

-- printing methods
toExternalString MonomialIdeal := I -> "monomialIdeal " | toExternalString generators I
expression MonomialIdeal := I -> (expression monomialIdeal) unsequence apply(toSequence first entries generators I, expression)

MonomialIdeal#AfterPrint = MonomialIdeal#AfterNoPrint = I -> (MonomialIdeal, " of ", ring I)

-----------------------------------------------------------------------------
-- codim
-----------------------------------------------------------------------------

codimopts := { Generic => false }
codim PolynomialRing := codimopts >> opts -> R -> 0
codim QuotientRing   := codimopts >> opts -> R -> codim(cokernel presentation R, opts)
codim Ideal          := codimopts >> opts -> I -> codim(cokernel   generators I, opts)
codim MonomialIdeal  := codimopts >> opts -> I -> I.cache#(symbol codim => opts) ??= rawCodimension raw I
codim Module         := codimopts >> opts -> M -> M.cache#(symbol codim => opts) ??= tryHooks((codim, Module), (opts, M),
    (opts, M) -> (
     R := ring M;
     if M == 0 then infinity
     else if isField R then 0
     else if R === ZZ then if M ** QQ == 0 then 1 else 0
     else (
	  if not opts.Generic and not isAffineRing ring M
	  then error "codim: expected an affine ring (consider Generic=>true to work over QQ)";
	  p := leadTerm gb presentation M;
	  n := rank target p;
	  c := infinity;
	  for i from 0 to n-1 when c > 0 do c = min(c,codim(monomialIdealOfRow(i,p)));
	  c - codim monomialIdealOfRow(0,matrix{{0_R}}) -- same as c - codim R, except works for iterated rings
	  )))

dim MonomialIdeal := I -> dim ring I - codim I

-----------------------------------------------------------------------------
-- Specialized algorithms for monomial ideals
-----------------------------------------------------------------------------

borel MonomialIdeal := MonomialIdeal => (I) -> newMonomialIdeal(ring I, rawStronglyStableClosure raw I)
isBorel MonomialIdeal := Boolean => m -> rawIsStronglyStable raw m

poincare MonomialIdeal := I -> I.cache.poincare ??= new degreesRing ring I from rawHilbert rawMonomialIdealToMatrix raw I

independentSets = method(Options => { Limit => infinity })
independentSets Ideal         := List => opts -> I -> independentSets(monomialIdeal I, opts)
independentSets MonomialIdeal := List => opts -> I -> first entries generators newMonomialIdeal(
    ring I, rawMaximalIndependentSets(raw I, if opts.Limit === infinity then -1 else opts.Limit))

lcm MonomialIdeal := I -> I.cache.lcm ??= (ring I) _ (rawMonomialIdealLCM raw I)

-----------------------------------------------------------------------------

protect AlexanderDual
alexopts = { Strategy => 0 }

dual MonomialIdeal        := alexopts >> o ->  I     -> dual(I, first exponents lcm I, o)
dual(MonomialIdeal, List) := alexopts >> o -> (I, a) -> I.cache#(AlexanderDual, a) ??= (
    aI := first exponents lcm I;
    if aI =!= a then (
	if #aI =!= #a            then error("expected list of length ", #aI);
	if any(a-aI, i -> i < 0) then error "exponent vector not large enough");
    newMonomialIdeal(ring I, rawAlexanderDual(raw I, a, o.Strategy)) -- 0 is the default algorithm
    )
dual(MonomialIdeal, RingElement) := alexopts >> o -> (I, r) -> dual(I, first exponents r, o)

-----------------------------------------------------------------------------

--  STANDARD PAIR DECOMPOSITION  ---------------------------
-- algorithm 3.2.5 in Saito-Sturmfels-Takayama
standardPairs = method()
-- Note: (standardPairs, MonomialIdeal) is redefined in PrimaryDecomposition.m2, because it depends on associatedPrimes
standardPairs MonomialIdeal        :=  I    -> error "standardPairs(MonomialIdeal) will be redefined in the PrimaryDecomposition package"
standardPairs(MonomialIdeal, List) := (I,D) -> (
     R := ring I;
     X := generators R;
     S := {};
     k := coefficientRing R;
     scan(D, L -> (
     	       Y := X;
     	       m := vars R;
	       Lset := set L;
	       Y = select(Y, r -> not Lset#?r);
     	       m = substitute(m, apply(L, r -> r => 1));
	       -- using monoid to create ring to avoid
	       -- changing global ring.
     	       A := k (monoid [Y]);
     	       phi := map(A, R, substitute(m, A));
     	       J := ideal mingens ideal phi generators I;
     	       Jsat := saturate(J, ideal vars A);
     	       if Jsat != J then (
     	  	    B := flatten entries super basis (
			 trim (Jsat / J));
		    psi := map(R, A, matrix{Y});
		    S = join(S, apply(B, b -> {psi(b), L}));
	       	    )));
     S)

--  LARGEST MONOMIAL IDEAL CONTAINED IN A GIVEN IDEAL  -----
monomialSubideal = method();				    -- needs a new name?
monomialSubideal Ideal := (I) -> (
     t := local t;
     R := ring I;
     X := generators R;
     k := coefficientRing R;
     S := k( monoid [t, X, MonomialOrder => Eliminate 1]);
     J := substitute(I, S);
     scan(#X, i -> (
	       w := {1} | toList(i:0) | {1} | toList(#X-i-1:0);
	       J = ideal homogenize(generators gb J, (generators S)#0, w);
	       J = saturate(J, (generators S)#0);
	       J = ideal selectInSubring(1, generators gb J);
	       ));
     monomialIdeal substitute(J, R)
     )


polarize = method(Options => {VariableBaseName => "z"});
polarize (MonomialIdeal) := o -> I -> (
    n := #(generators ring I);
    u := apply(#(first entries mingens I), i -> first exponents I_i);
    Ilcm := max \ transpose u;
    z := getSymbol(o.VariableBaseName);
    Z := flatten apply(n, i -> apply(Ilcm#i, j -> z_{i,j}));
    R := QQ(monoid[Z]);
    G := generators R;
    p := apply(n, i -> sum((Ilcm)_{0..i-1}));
    monomialIdeal apply(u, e -> product apply(n, i -> product(toList(0..e#i-1), j -> G#(p#i+j))))
    )


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
