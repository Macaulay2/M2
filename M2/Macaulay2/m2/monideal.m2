-- Copyright 1995-2002 by Michael Stillman

MonomialIdeal = new Type of Ideal
MonomialIdeal.synonym = "monomial ideal"
monomialIdeal = method(TypicalValue => MonomialIdeal,Dispatch => Thing)
numgens MonomialIdeal := I -> I.numgens
raw MonomialIdeal := I -> I.RawMonomialIdeal
generators MonomialIdeal := opts -> (cacheValue symbol generators) ( (I) -> map(ring I, rawMonomialIdealToMatrix raw I) )
toExternalString MonomialIdeal := (I) -> "monomialIdeal " | toExternalString generators I

ideal MonomialIdeal := (I) -> ideal generators I
isIdeal MonomialIdeal := I -> true

newMonomialIdeal = (R,rawI) -> new MonomialIdeal from {
     symbol numgens => rawNumgens rawI,
     symbol RawMonomialIdeal => rawI,
     symbol cache => new CacheTable,
     symbol ring => R
     }

monomialIdealOfRow := (i,m) -> newMonomialIdeal(ring m,rawMonomialIdeal(raw m, i))

codimopts := { Generic => false }
codim MonomialIdeal := {  } >> opts -> m -> rawCodimension raw m
codim Module := codimopts >> opts -> (cacheValue (symbol codim => opts)) (M -> runHooks(Module, symbol codim, (opts,M)))
codim Ideal := codimopts >> opts -> I -> codim( cokernel generators I, opts)
codim PolynomialRing := codimopts >> opts -> R -> 0
codim QuotientRing := codimopts >> opts -> (R) -> codim( cokernel presentation R, opts)

addHook(Module, symbol codim, (opts,M) -> break (
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
	  c - codim(R,opts))))

MonomialIdeal ^ ZZ := MonomialIdeal => (I,n) -> (
     if n < 0 then error "expected nonnegative exponent"
     else if n === 0 then monomialIdeal 1_(ring I)
     else SimplePowerMethod(I,n)
     )

MonomialIdeal ^ Array := (I, e) -> (
   monomialIdeal (ideal I)^e
)

Ring / MonomialIdeal := (R,I) -> R / ideal I

monomialIdeal MonomialIdeal := identity

monomialIdeal Matrix := MonomialIdeal => f -> (
     if numgens target f =!= 1 then error "expected a matrix with 1 row";
     if not isCommutative ring f 
       then error "expected a commutative ring";
     if not isPolynomialRing ring f 
       then error "expected a polynomial ring without quotient elements";
     monomialIdealOfRow(0,f))

monomialIdeal List := MonomialIdeal => v -> monomialIdeal matrix {splice v}
monomialIdeal Sequence := v -> monomialIdeal toList v

MonomialIdeal == MonomialIdeal := (I,J) -> I === J

MonomialIdeal == ZZ := (I,i) -> (
     if i === 0 then numgens I == 0
     else if i === 1 then 1 % I == 0
     else error "asked to compare monomial ideal to nonzero integer")
ZZ == MonomialIdeal := (i,I) -> I == i

MonomialIdeal + MonomialIdeal := MonomialIdeal => (I,J) -> (
     if ring I =!= ring J then error "expected monomial ideals in the same ring";
     newMonomialIdeal(ring I, raw I + raw J))
MonomialIdeal * MonomialIdeal := MonomialIdeal => (I,J) -> (
     if ring I =!= ring J then error "expected monomial ideals in the same ring";
     newMonomialIdeal(ring I, raw I * raw J))
MonomialIdeal - MonomialIdeal := MonomialIdeal => (I,J) -> (
     if ring I =!= ring J then error "expected monomial ideals in the same ring";
     newMonomialIdeal(ring I, raw I - raw J))

radical MonomialIdeal := MonomialIdeal => options -> (I) -> newMonomialIdeal(ring I, rawRadical raw I)

quotient(MonomialIdeal, MonomialIdeal) := MonomialIdeal => opts -> (I,J) -> newMonomialIdeal(ring I, rawColon(raw I, raw J))
MonomialIdeal : MonomialIdeal := MonomialIdeal => quotient

quotient(MonomialIdeal, RingElement) := opts -> (I,f) -> I : monomialIdeal terms f
MonomialIdeal : RingElement := MonomialIdeal => quotient

saturate(MonomialIdeal, MonomialIdeal) := MonomialIdeal => o -> (I,J) -> newMonomialIdeal(ring I, rawSaturate(raw I, raw J))

saturate(MonomialIdeal, RingElement) := Ideal => o -> (I,f) -> (
     if size f === 1 and leadCoefficient f == 1 then saturate (I,monomialIdeal f)
     else saturate(ideal I, ideal f)
     )

int := (I,J) -> (
     if ring I =!= ring J then error "expected monomial ideals in the same ring";
     newMonomialIdeal(ring I, rawIntersect(raw I, raw J)))

intersect(List) := x -> intersect toSequence x

intersect(Sequence) := args -> (
    -- first check that all modules have the same target
    -- and the same base ring
    if #args === 0 then error "expected at least one argument";
    M := args#0;
    R := ring M;
    if class M === MonomialIdeal then (
	 if not all(args, M -> class M === MonomialIdeal and R === ring M)
	 then error "expected monomial ideals over the same ring";
	 i := 1;
	 while i < #args do (
	      M = int(M,args#i);
	      i = i+1;
	      );
	 M)
    else if class M === Module then (
    	 F := ambient args#0;
	 if not all(args, N -> ambient N == F)
	 or M.?relations 
	 and not all(args, N -> 
	      N.?relations 
	      and (N.relations == M.relations
		   or
		   image N.relations == image M.relations
		   )
	      )
    	 then error "all modules must be submodules of the same module";
    	 relns := directSum apply(args, N -> (
		   if N.?relations 
		   then generators N | N.relations
		   else generators N
		   )
	      );
    	 g := map(R^(#args),R^1, table(#args,1,x->1)) ** id_F;
	 h := modulo(g, relns);
	 if M.?relations then h = compress( h % M.relations );
    	 subquotient( h, if M.?relations then M.relations )
	 )
    else if class M === Ideal then (
	 ideal intersect apply(args,module)
	 )
    else error "expected modules, ideals, or monomial ideals"
    )

borel MonomialIdeal := MonomialIdeal => (I) -> newMonomialIdeal(ring I, rawStronglyStableClosure raw I)
isBorel MonomialIdeal := Boolean => m -> rawIsStronglyStable raw m

poincare MonomialIdeal := (cacheValue symbol poincare) (M -> new degreesRing M from rawHilbert rawMonomialIdealToMatrix M.RawMonomialIdeal)

independentSets = method(Options => { Limit => infinity })
independentSets MonomialIdeal := o -> (M) -> (
     result := newMonomialIdeal(ring M, 
	  rawMaximalIndependentSets(M.RawMonomialIdeal, 
	       if o.Limit === infinity then -1 else o.Limit));
     flatten entries generators result)
independentSets Ideal := o -> (M) -> independentSets(monomialIdeal M,o)

-----------------------------------------------------------------------------
-- this code below here is by Greg Smith (and partially Mike Stillman)
-----------------------------------------------------------------------------

expression MonomialIdeal := (I) -> (
     if numgens I === 0 then hold "0" 
     else new FunctionApplication from { monomialIdeal, expression unsequence toSequence first( entries generators I) }
     )

MonomialIdeal#{Standard,AfterPrint} = MonomialIdeal#{Standard,AfterNoPrint} = (I) -> (
     << endl;				  
     << concatenate(interpreterDepth:"o") << lineNumber << " : MonomialIdeal of " 
     << ring I << endl;
     )

monomialIdeal Ideal :=  MonomialIdeal => (I) -> monomialIdeal generators gb I

monomialIdeal Module := MonomialIdeal => (M) -> (
     if isSubmodule M and rank ambient M === 1 
     then monomialIdeal generators gb M
     else error "expected a submodule of a free module of rank 1"
     )

monomialIdeal RingElement := MonomialIdeal => v -> monomialIdeal {v}
ring MonomialIdeal := I -> I.ring
numgens MonomialIdeal := I -> I.numgens
MonomialIdeal _ ZZ := (I,n) -> (generators I)_(0,n)

isMonomialIdeal = method(TypicalValue => Boolean)
isMonomialIdeal Thing := x -> false
isMonomialIdeal MonomialIdeal := (I) -> true
isMonomialIdeal Ideal := (I) -> isPolynomialRing ring I and all(first entries generators I, r -> size r === 1 and leadCoefficient r == 1)

hilbertSeries MonomialIdeal := lookup(hilbertSeries,Module)

MonomialIdeal == Ideal := (I,J) -> ideal I == J
Ideal == MonomialIdeal := (I,J) -> I == ideal J

MonomialIdeal == Ring := (I,R) -> (
     if ring I =!= R then error "expected ideals in the same ring";
     1_R % I == 0)
Ring == MonomialIdeal := (R,I) -> I == R

MonomialIdeal + Ideal := Ideal => (I,J) -> ideal I + J
Ideal + MonomialIdeal := Ideal => (I,J) -> I + ideal J

RingElement * MonomialIdeal := MonomialIdeal => (r,I) -> monomialIdeal (r * generators I)
ZZ * MonomialIdeal := MonomialIdeal => (r,I) -> monomialIdeal (r * generators I)

MonomialIdeal * Ideal := Ideal => (I,J) -> ideal I * J
Ideal * MonomialIdeal := Ideal => (I,J) -> I * ideal J

MonomialIdeal * Module := Module => (I,M) -> ideal I * M

MonomialIdeal * Ring := Ideal => (I,S) -> if ring I === S then I else monomialIdeal(I.generators ** S)
Ring * MonomialIdeal := Ideal => (S,I) -> if ring I === S then I else monomialIdeal(I.generators ** S)

Matrix % MonomialIdeal := Matrix => (f,I) -> f % forceGB generators I
RingElement % MonomialIdeal := (r,I) -> r % forceGB generators I
ZZ % MonomialIdeal := (r,I) -> r_(ring I) % forceGB generators I

Matrix // MonomialIdeal := Matrix => (f,I) -> f // forceGB generators I
RingElement // MonomialIdeal := (r,I) -> r // forceGB generators I
ZZ // MonomialIdeal := (r,I) -> r_(ring I) // forceGB generators I

dim MonomialIdeal := I -> dim ring I - codim I

degree MonomialIdeal := I -> degree cokernel generators I   -- maybe it's faster with 'poincare'

jacobian MonomialIdeal := Matrix => (I) -> jacobian generators I

resolution MonomialIdeal := ChainComplex => options -> I -> resolution ideal I
betti MonomialIdeal := opts -> I -> betti(ideal I,opts)

lcm MonomialIdeal := (I) -> (if I.cache.?lcm 
  then I.cache.lcm
  else I.cache.lcm = (ring I) _ (rawMonomialIdealLCM raw I))

 -- We use E. Miller's definition for nonsquare 
 -- free monomial -- ideals.

alexanderDual = local alexanderDual
alexopts = {Strategy=>0}

dual(MonomialIdeal, List) := alexopts >> o -> (I,a) -> (
     aI := first exponents lcm I;
     if aI =!= a then (
     	  if #aI =!= #a then error ( "expected list of length ", toString (#aI));
	  scan(a, aI, (b,c) -> if b<c then error "exponent vector not large enough" );
	  );
     newMonomialIdeal(ring I, rawAlexanderDual(raw I, a, o.Strategy)) -- 0 is the default algorithm
     )

dual(MonomialIdeal,RingElement) := alexopts >> o -> (I,r) -> alexanderDual(I,first exponents r,o)

dual MonomialIdeal := alexopts >> o -> (I) -> (
  if I.cache#?alexanderDual
    then I.cache#alexanderDual
    else I.cache#alexanderDual = (
	 dual(I, first exponents lcm I, o)
    ))

--  TESTING IF A THING IS A SQUARE FREE MONOMIAL IDEAL  ----
isSquareFree = method(TypicalValue => Boolean)		    -- could be isRadical?
-- isSquareFree Thing := x -> false
isSquareFree MonomialIdeal := (I) -> all(first entries generators I, m -> all(first exponents m, i -> i<2))

--  STANDARD PAIR DECOMPOSITION  ---------------------------
-- algorithm 3.2.5 in Saito-Sturmfels-Takayama
standardPairs = method()
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
Delta := (I) -> (
     X := generators ring I;
     d := #X - pdim cokernel generators I;
     select( apply(associatedPrimes I, J -> set X - set first entries generators J), Y -> #Y >= d ) / toList
     )

standardPairs MonomialIdeal := (I) -> standardPairs(I,Delta I)

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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
