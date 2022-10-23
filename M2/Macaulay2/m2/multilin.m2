--		Copyright 1995-2002,2010 by Daniel R. Grayson and Michael Stillman
-- TODO: implement better caching here

needs "matrix1.m2"
needs "basis.m2"

-----------------------------------------------------------------------------
-- helper for exteriorPower and minors
-----------------------------------------------------------------------------

hasNoQuotients = method()
hasNoQuotients QuotientRing := (R) -> isField R
hasNoQuotients PolynomialRing := (R) -> hasNoQuotients coefficientRing R
hasNoQuotients Ring := (R) -> true

getMinorsStrategy := (R, opts) -> (
     bareiss := 0;  -- WARNING: these must match the engine!!
     cofactor := 1;
     strat := if opts.?Strategy then opts.Strategy else null;
     if strat === global Bareiss then bareiss
     else if strat === global Cofactor then cofactor
     else if strat =!= null then (
	  error "'Strategy' keyword must be 'Cofactor' or 'Bareiss'";
	  )
     else (
	  -- Use the Bareiss algorithm unless R is a quotient of
	  -- a polynomial ring.  Note that if R is non-commutative
	  -- then either algorithm is incorrect.  What is the correct
	  -- thing to do in this case?
          if hasNoQuotients R and precision R === infinity then
            bareiss
          else
     	    cofactor
     ))

-----------------------------------------------------------------------------
-- symmetricAlgebra
-----------------------------------------------------------------------------

rep := (meth, opts, args) -> prepend_opts nonnull apply(args, arg ->
    if instance(arg, Option) and #arg == 2 and instance(arg#1, Function) then (
	if (options meth)#(arg#0) === opts#(arg#0) then arg#0 => arg#1())
    else arg)

symmetricAlgebra = method( Options => monoidDefaults )
symmetricAlgebra Module := Ring => opts -> (cacheValue (symmetricAlgebra => opts)) (M -> (
	k := ring M;
	N := monoid rep(symmetricAlgebra, opts, [
		Variables => () -> numgens M,
		Degrees   => () -> degrees M / prepend_1,
		Join      => false]);
	S := k N;
	S  = S / ideal(vars S * promote(presentation M, S));
	S.Module = M;
	S))

symmetricAlgebra(Ring, Ring, Matrix) := RingMap => o -> (T, S, f) -> (
    if f.cache#?(key := (symmetricAlgebra, T, S, f)) then f.cache#key else f.cache#key = (
	p := map(T, S, vars T * promote(cover f, T));
	if f.cache.?inverse then p.cache.inverse = (
	    map(S, T, vars S * promote(cover inverse f, S)));
	p))
symmetricAlgebra                   Matrix  := RingMap => o ->        f  -> symmetricAlgebra(symmetricAlgebra target f, symmetricAlgebra source f, f)
symmetricAlgebra(Nothing, Nothing, Matrix) := RingMap => o -> (T, S, f) -> symmetricAlgebra(symmetricAlgebra target f, symmetricAlgebra source f, f)
symmetricAlgebra(Nothing, Ring,    Matrix) := RingMap => o -> (T, S, f) -> symmetricAlgebra(symmetricAlgebra target f, S, f)
symmetricAlgebra(Ring,    Nothing, Matrix) := RingMap => o -> (T, S, f) -> symmetricAlgebra(T, symmetricAlgebra source f, f)

-----------------------------------------------------------------------------
-- symmetricPower, exteriorPower, and wedgeProduct
-----------------------------------------------------------------------------

symmetricPower = method()
symmetricPower(ZZ, Module) := Module => (p, M) -> (
    if M.cache#?(key := (symmetricPower, p)) then M.cache#key else M.cache#key = (
	R := ring M;
	if p   < 0 then R^0 else
	if p === 0 then R^1 else
	if p === 1 then M   else
	if isFreeModule M   then new Module from (R, rawSymmetricPower(p, raw M))
	else coimage basis(p, symmetricAlgebra M,
	    SourceRing => ring M, Degree => {p, degreeLength R:0})
    ))
symmetricPower(ZZ, Matrix) := Matrix => (i, m) -> map(ring m, rawSymmetricPower(i, raw m))

exteriorPower = method(Options => { Strategy => null })
exteriorPower(ZZ, Module) := Module => opts -> (p, M) -> (
    if M.cache#?(exteriorPower, p) then M.cache#(exteriorPower, p) else M.cache#(exteriorPower, p) = (
	R := ring M;
	if p   < 0 then R^0 else
	if p === 0 then R^1 else
	if p === 1 then M   else
	if isFreeModule M   then new Module from (R, rawExteriorPower(p, M.RawFreeModule))
	else (
	    m := presentation M;
	    F := target m;
	    Fp1 := exteriorPower(p-1, F, opts);
	    h1 := m ** id_Fp1;
	    h2 := wedgeProduct(1,p-1,F);
	    cokernel(h2 * h1))
    ))

exteriorPower(ZZ, Matrix) := Matrix => opts -> (p, m) -> (
    R := ring m;
    if p   < 0 then id_(R^0) else
    if p === 0 then id_(R^1) else
    if p === 1 then m
    -- TODO: should this be cached, too?
    else map(
	exteriorPower(p, target m, opts),
	exteriorPower(p, source m, opts),
	rawExteriorPower(p, raw m, getMinorsStrategy(R, opts)))
    )

wedgeProduct = method()
wedgeProduct(ZZ, ZZ, Module) := Matrix => (p, q, M) -> (
     if isFreeModule M then map(ring M, rawWedgeProduct(p,q,raw M))
     else map(exteriorPower(p+q,M),exteriorPower(p,M)**exteriorPower(q,M),wedgeProduct(p,q,cover M)))

-----------------------------------------------------------------------------
-- ideals of minors, permanents, and pfaffians
-----------------------------------------------------------------------------

-- TODO: singularLocus uses minors, but it is defined in quotring.m2
-- TODO: Minors with prescribed rows or columns, https://github.com/Macaulay2/M2/issues/1705
minors = method(
    Options => {
	Limit    => infinity,
	First    => null,
	Strategy => null,
	}
    )
minors(ZZ, Matrix) := Ideal => opts -> (j, m) -> (
     f := opts.First;
     if not (
	  f === null or (
	       class f === List
	       and #f == 2
	       and all(f, s -> class s === List)
	       )
	  ) then error "expected a list of 2 lists of integers";
     if j <= 0 then ideal 1_(ring m)
     else ideal map(ring m, rawMinors(j, raw m, getMinorsStrategy(ring m,opts), 
	       if opts.Limit === infinity then -1 else opts.Limit,
	       if f =!= null then f#0, 
	       if f =!= null then f#1)))

pfaffians = method(TypicalValue => Ideal)
pfaffians(ZZ, Matrix) := (j, m) -> (
     ideal(map(ring m, rawPfaffians(j,raw m))))

-----------------------------------------------------------------------------
-- trace and determinant
-----------------------------------------------------------------------------

trace = method()
trace Matrix := RingElement => f -> (
     if rank source f != rank target f
     or not isFreeModule source f
     or not isFreeModule target f
     then error "expected a square matrix";
     sum(rank source f, i -> f_(i,i)))

-- declared in mutablemat.m2
determinant Matrix := RingElement => opts -> f -> (
     if rank source f != rank target f
     or not isFreeModule source f
     or not isFreeModule target f
     then error "expected a square matrix";
     if hasEngineLinearAlgebra ring f and isBasicMatrix f and numRows f === numColumns f then
       basicDet f
     else 
       (exteriorPower(numgens source f, f, opts))_(0,0))

-- TODO: permanent
-- TODO: immanant

-----------------------------------------------------------------------------

fittingIdeal = method(TypicalValue => Ideal)
-- TODO: document this
fittingIdeal(ZZ, Matrix) := (i, p) -> (
     n := rank target p;
     if n <= i
     then ideal 1_(ring p)
     else trim minors(n-i,p))
fittingIdeal(ZZ, Module) := (i, M) -> fittingIdeal(i, presentation M)

-- TODO: fittingImage of a ring map

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
