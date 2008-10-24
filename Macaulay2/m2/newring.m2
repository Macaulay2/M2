-- Copyright 1996 Michael E. Stillman

----------------------------------
-- new polynomial ring or quotient ring from old --
----------------------------------

nothing := symbol nothing
newRing = method( Options => applyValues(monoidDefaults, x -> nothing), TypicalValue => Ring )
newRing PolynomialRing := opts -> (R) -> (
     opts = new MutableHashTable from select(opts, v -> v =!= nothing);
     nullify := k -> if not opts#?k then opts#k = monoidDefaults#k;
     if opts.?DegreeRank then (nullify Degrees;    nullify Heft);
     if opts.?Degrees and opts.Degrees =!= {} then (nullify DegreeRank; nullify Heft);
     if opts.?Variables then (
	  if instance(opts.Variables,List) then (
	       opts.Variables = splice opts.Variables;
	       if # opts.Variables =!= numgens R then nullify Degrees;
	       )
	  else if instance(opts.Variables,ZZ) then (
	       if opts.Variables =!= numgens R then nullify Degrees;
	       ));
     (coefficientRing R)(monoid [merge(options R,new OptionTable from opts,last)]))
newRing QuotientRing := opts -> R -> (
     p := presentation R;
     if not instance(R,PolynomialRing) then error "newRing: expected ambient ring of quotient ring to be a polynomial ring";
     S := newRing(ring p, opts);
     if numgens S != numgens R then error "newRing: cannot change the number of variables";
     S / image substitute(presentation R,vars S))

-----------------------------
-- tensor product of rings --
-----------------------------

-- made a method and documented elsewhere.
Ring ** Ring := Ring => (R,S) -> tensor(R,S)
tensor(Ring,Ring) := Ring => opts -> (R,S) -> (
     if R === (try coefficientRing S) then return S;
     if S === (try coefficientRing R) then return R;
     if R === QQ and ZZ === (try coefficientRing S) then (
	  (A,f) := flattenRing S;
	  T := QQ monoid A;
	  I := ideal A;
	  B := ring I;
	  p := map(T,B);
	  return T/p I);
     if S === QQ and ZZ === (try coefficientRing R) then return tensor(S,R);
     error "tensor product not implemented for these rings"
     )

PolynomialRing ** PolynomialRing :=
QuotientRing ** PolynomialRing :=
PolynomialRing ** QuotientRing :=
QuotientRing ** QuotientRing := (R,S) -> tensor(R,S)

tensor(PolynomialRing, PolynomialRing) :=
tensor(QuotientRing, PolynomialRing) :=
tensor(PolynomialRing, QuotientRing) :=
tensor(QuotientRing, QuotientRing) := optns -> (R,S) -> (
     k := coefficientRing R;
     if k =!= coefficientRing S 
     then error "expected rings to have the same coefficient ring";
     f := presentation R; A := ring f; M := monoid A; m := numgens M;
     g := presentation S; B := ring g; N := monoid B; n := numgens N;
     AB := k tensor(M, N, optns);
     fg := substitute(f,(vars AB)_{0 .. m-1}) | substitute(g,(vars AB)_{m .. m+n-1});
     -- forceGB fg;  -- if the monomial order chosen doesn't restrict, then this
                     -- is an error!! MES
     AB/image fg)

-------------------------
-- Graph of a ring map --
-------------------------

graphIdeal = method( Options => apply( {MonomialOrder, MonomialSize, VariableBaseName}, o -> o => monoidDefaults#o ))
graphRing = method( Options => options graphIdeal )

graphIdeal RingMap := Ideal => opts -> (f) -> (
     -- return the ideal in the tensor product of the graph of f.
     -- if f is graded, then set the degrees correctly in the tensor ring.
     -- return the ideal (y_i - f_i : all i) in this ring.
     S := source f;
     R := target f;
     m := numgens R;
     n := numgens S;
     k := coefficientRing R;
     if not ( isAffineRing R and isAffineRing S and k === coefficientRing S ) then error "expected polynomial rings over the same ring";
     gensk := generators(k, CoefficientRing => ZZ);
     if not all(gensk, x -> promote(x,R) == f promote(x,S)) then error "expected ring map to be identity on coefficient ring";
     RS := tensor(R,S, opts, MonomialOrder => Eliminate m, Degrees => join(degrees R, apply(degrees S, f.DegreeMap)));
     v := vars RS;
     yv := v_{m .. m+n-1};
     xv := v_{0 .. m-1};
     h := f.matrix_{0 .. n - 1};
     I := ideal(yv - substitute(h, xv));
     assert(not isHomogeneous f or isHomogeneous I);
     I)

graphRing RingMap := QuotientRing => opts -> (f) -> ( I := graphIdeal(f,opts); (ring I)/I )

-----------------------
-- Symmetric Algebra --
-----------------------

rep := (meth,opts,args) -> prepend_opts nonnull apply(args, arg -> 
     if instance(arg,Option) and #arg == 2 and instance(arg#1,Function) then (
	  if (options meth)#(arg#0) === opts#(arg#0)
     	  then arg#0 => arg#1()
	  )
     else arg)

symmetricAlgebra = method( Options => monoidDefaults )
symmetricAlgebra Module := Ring => opts -> (cacheValue (symmetricAlgebra => opts)) (M -> (
	  k := ring M;
	  N := monoid rep(symmetricAlgebra, opts, [Join => false, Variables => () -> numgens M, Degrees => () -> degrees M / prepend_1]);
	  S := k N;
	  S  = S / ideal(vars S * promote(presentation M,S));
	  S.Module = M;
	  S))
symmetricAlgebra(Ring,Ring,Matrix) := RingMap => opts -> (T,S,f) -> (
     key := (T,S,f) ;
     if f.cache#?key then f.cache#key else f.cache#key = (
     	  map(T,S,vars T * promote(f,T))))
symmetricAlgebra Matrix := RingMap => opts -> f -> symmetricAlgebra(symmetricAlgebra target f,symmetricAlgebra source f,f)
symmetricAlgebra(Ring,Nothing,Matrix) := RingMap => opts -> (T,S,f) -> symmetricAlgebra(T,symmetricAlgebra source f,f)
symmetricAlgebra(Nothing,Ring,Matrix) := RingMap => opts -> (T,S,f) -> symmetricAlgebra(symmetricAlgebra target f,S,f)
symmetricAlgebra(Nothing,Nothing,Matrix) := RingMap => opts -> (T,S,f) -> symmetricAlgebra f

-----------------------------------------------------------------------------
-- flattenRing
-----------------------------------------------------------------------------
-- Copyright 2006 by Daniel R. Grayson

-- flattening rings (like (QQ[a,b]/a^3)[x,y]/y^6 --> QQ[a,b,x,y]/(a^3,y^6)
flattenRing = method(
     Options => {					    -- return value = (new ring, ring map to it, ring map from it)
	  CoefficientRing => null			    -- the default is to take the latest (declared) field or basic ring in the list of base rings
	  })

unable := () -> error "unable to flatten ring over given coefficient ring"

-- in general, fraction fields are not finitely presented over smaller coefficient rings
-- maybe we'll do something later when we have localization as something more intrinsic
-- flattenRing FractionField := opts -> F -> ...

triv := R -> ( 
     idR := map(R,R);
     idR.cache.inverse = idR;
     (R,idR))

flattenRing Ring := opts -> R -> (
     k := opts.CoefficientRing;
     if k === R or k === null and (R.?isBasic or isField R) then triv R
     else unable())

flattenRing GaloisField := opts -> F -> (
     A := ambient F;
     (R,p) := flattenRing(A, opts);
     q := p^-1;
     p' := p * map(A,F);
     q' := map(F,A) * q;
     p'.cache.inverse = q';
     q'.cache.inverse = p';
     (R,p'))

flattenRing PolynomialRing := opts -> R -> (
     A := coefficientRing R;
     M2 := monoid R;
     zerdeg := toList (degreeLength M2 : 0);
     n2 := numgens M2;
     if opts.CoefficientRing === A or opts.CoefficientRing === null and (isField A or A.?isBasic) then return triv R;
     if # generators R === 0 then return flattenRing(A,opts);
     (S',p) := flattenRing(coefficientRing R, opts);
     q := p^-1;
     I := ideal S';
     S := ring I;
     (n1,T) := 
     if instance(S,PolynomialRing)
     then ( numgens monoid S, (coefficientRing S) tensor(M2,monoid S) )
     else ( 0, S M2 );
     inc := map(T,S,(vars T)_(toList (n2 .. n2 + n1 - 1)), DegreeMap => M2.Options.DegreeMap);
     I' := inc I;
     T' := T/I';
     inc' := map(T',S', promote(matrix inc,T'), DegreeMap => M2.Options.DegreeMap);
     pr := map(T',T);
     p' := map(T',R, (vars T')_(toList (0 .. n2-1)) | inc' matrix p);
     q' := map(R,T', vars R | promote(matrix q,R));
     p'.cache.inverse = q';
     q'.cache.inverse = p';
     (T',p'))

flattenRing QuotientRing := opts -> R -> (
     if instance(ambient R, PolynomialRing) and (
	  k := coefficientRing R;
	  opts.CoefficientRing === null and (isField k or k.?isBasic)
	  or opts.CoefficientRing === k
	  )
     then return triv R;
     I := ideal R;
     A := ring I;
     (B',p) := flattenRing(A,opts);
     q := p^-1;
     J := ideal B';
     B := ring J;
     J' := lift(p I, B);				    -- adds in J automatically
     S := B/J';
     p' := map(S,R, promote( lift( matrix p, B ), S ));
     q' := map(R,S, promote( matrix q, R ));
     p'.cache.inverse = q';
     q'.cache.inverse = p';
     (S,p'))

isWellDefined RingMap := f -> (
     R := source f;
     (S,p) := flattenRing(R,CoefficientRing=>ZZ);
     q := p^-1;
     T := ambient S;
     I := ideal S;
     g := f * q * map(S,T);
     g I == 0)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
