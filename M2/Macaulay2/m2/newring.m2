-- Copyright 1996 Michael E. Stillman

needs "galois.m2"
needs "ofcm.m2"
needs "quotring.m2"
needs "ringmap.m2"
needs "rings.m2"
needs "matrix2.m2" -- for lift

----------------------------------
-- new polynomial ring or quotient ring from old --
----------------------------------

nothing := symbol nothing
newRing = method( Options => applyValues(monoidDefaults, x -> nothing), TypicalValue => Ring )
newRing PolynomialRing := opts -> (R) -> (
     opts = new MutableHashTable from select(new HashTable from opts, v -> v =!= nothing);
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
     A := ring p;
     if not instance(A,PolynomialRing) then error "newRing: expected ambient ring of quotient ring to be a polynomial ring";
     S := newRing(A, opts);
     if numgens S != numgens R then error "newRing: cannot change the number of variables";
     S / image substitute(p,vars S))

-----------------------------
-- tensor product of rings --
-----------------------------

-- made a method and documented elsewhere.
Ring ** Ring := Ring => (R,S) -> tensor(R,S)
tensor(Ring, Ring) := Ring => monoidTensorDefaults >> opts -> (R, S) -> (
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
tensor(QuotientRing,   PolynomialRing) :=
tensor(PolynomialRing, QuotientRing) :=
tensor(QuotientRing,   QuotientRing) := monoidTensorDefaults >> optns -> (R, S) -> (
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

graphIdeal RingMap := Ideal => opts -> (cacheValue (symbol graphIdeal => opts)) ((f) -> (
     -- return the ideal in the tensor product of the graph of f.
     -- if f is graded, then set the degrees correctly in the tensor ring.
     -- return the ideal (y_i - f_i : all i) in this ring.
     S := source f;
     R := target f;
     m := numgens R;
     n := numgens S;
     k := coefficientRing R;
     if not isCommutative S then error "expected source of ring map to be a commutative ring";
     if S === k then return ideal map(R^1,R^0,0);
     if not isAffineRing R then error "expected an affine ring";
     if not isAffineRing S then error "expected an affine ring";
     if not ( k === coefficientRing S ) then error "expected polynomial rings over the same ring";
     gensk := generators(k, CoefficientRing => ZZ);
     if not all(gensk, x -> promote(x,R) == f promote(x,S)) then error "expected ring map to be identity on coefficient ring";
     RS := tensor(R,S, opts, 
	  MonomialOrder => Eliminate m, 
	  Degrees => join(degrees R, apply(degrees S, f.cache.DegreeMap)),
	  Heft => heft R);
     v := vars RS;
     yv := v_{m .. m+n-1};
     xv := v_{0 .. m-1};
     h := f.matrix_{0 .. n - 1};
     I := ideal(yv - substitute(h, xv));
     assert(not isHomogeneous f or isHomogeneous I);
     I))

graphRing RingMap := QuotientRing => opts -> (f) -> ( I := graphIdeal(f,opts); (ring I)/I )

-----------------------------------------------------------------------------
-- flattenRing
-----------------------------------------------------------------------------
-- Copyright 2006 by Daniel R. Grayson

coerce = method(Dispatch=>{Thing,Type})	-- this might be generally useful to the user, as the mathematical analogue of new...from...
coerce(Thing,Thing) := (x,Y) -> if instance(x,Y) then x else error("no method for coercing result to type ", toString Y)
coerce(Ideal,Ring) := quotient @@ first
coerce(Thing,Nothing) := (x,Nothing) -> null		    -- avoid using this one, to save time earlier
coerce(Ring,Ideal) := (R,Ideal) -> ideal R		    -- avoid using this one, to save time earlier
preprocessResultTemplate = (narrowers,r) -> (
     if instance(r,ZZ) then r = r:null;
     r = apply(sequence r,x -> if x === null then Thing else x);
     if #r == 0 then return r;
     if not all(r, T -> instance(T,Type)) then error "expected Result option to be a type or a sequence of types or nulls";
     r = apply(#r, i -> if narrowers#?i and ancestor(r#i,narrowers#i) then narrowers#i else r#i );
     toSequence r)
coerceResults = (resultTemplate,results) -> (
     if #resultTemplate > #results then error(
	  if #results === 1 
	  then ( "only 1 result is available" )
	  else ( "only ", toString(#results), " results are available" )
	  );
     unsequence apply(take(results,#resultTemplate),resultTemplate,coerce))
flatCoerce = (R,resultTemplate,results) -> (
     (X,p,q) := results;
     -- assert( instance(X,Ideal) or instance(X,Ring));
     A := if instance(X,Ideal) then ring X else X;
     -- assert( instance(p,RingMap) );
     -- assert( instance(q,RingMap) );
     -- assert( A === target p and A === source q );
     -- assert( R === source p and R === target q );
     if instance(X,Ideal) and ancestor(Ring,resultTemplate#0) then (
	  X = quotient X;
	  if X =!= target p then p = map(X,source p,promote(matrix p,X));
	  if X =!= source q then q = map(target q,X,q);
	  );
     if instance(X,Ring) and not ancestor(Ring,resultTemplate#0) then (
	  X = ideal X;
	  B := ring X;
	  if B =!= target p then p = map(B,source p,lift(matrix p,B));
	  if B =!= source q then q = map(target q,B,q);
	  );
     if instance(X,Ring) then (
     	  p.cache.inverse = q;
     	  q.cache.inverse = p;
	  );
     coerceResults(resultTemplate,(X,p,q)))     

-- flattening rings (like (QQ[a,b]/a^3)[x,y]/y^6 --> QQ[a,b,x,y]/(a^3,y^6)
flattenRing = method(
     Options => {
     	  Result => (Thing,RingMap),
	  CoefficientRing => null			    -- the default is to take the latest (declared) field or basic ring in the list of base rings
	  })

unable := () -> error "unable to flatten ring over given coefficient ring"

-- in general, fraction fields are not finitely presented over smaller coefficient rings
-- maybe we'll do something later when we have localization as something more intrinsic
-- flattenRing FractionField := opts -> F -> ...

triv := R -> ( 
     idR := map(R,R);
     idR.cache.inverse = idR;
     (R,idR,idR))

flattenRing Ring := opts -> R -> (
     resultTemplate := preprocessResultTemplate(1:Ring, opts.Result);
     k := opts.CoefficientRing;
     if k === R or k === null and (R.?isBasic or isField R) then flatCoerce(R,resultTemplate,triv R)
     else unable())

flattenRing GaloisField := opts -> (cacheValue (symbol flattenRing => opts)) (F -> (
     resultTemplate := preprocessResultTemplate(1:Ring, opts.Result);
     A := ambient F;
     (X,p,q) := flattenRing(A, opts, Result => (resultTemplate#0,,));
     (p,q) = (map(target p, F, p), map(F, source q, q));
     flatCoerce(F,resultTemplate,(X,p,q))))

flatQuotient := method()
flatQuotient(Ring,Ideal) := (R,I) -> R/I
flatQuotient(QuotientRing,Ideal) := (R,I) -> flatQuotient(ambient R,lift(I,ambient R))

flattenRing Ideal := opts -> (cacheValue (symbol flattenRing => opts)) (J -> (
     	  resultTemplate := preprocessResultTemplate(1:Ideal, opts.Result);
	  R := ring J;
	  (I,p,q) := flattenRing(R,opts,Result => (Ideal,,));
	  I = if ring I === R then J else I + p J;
	  flatCoerce(ring J, resultTemplate,(I,p,q))))

flattenRing QuotientRing := opts -> (cacheValue (symbol flattenRing => opts)) (R -> (
     	  resultTemplate := preprocessResultTemplate(1:Ring, opts.Result);
	  if instance(ambient R, PolynomialRing) and (
	       k := coefficientRing R;
	       opts.CoefficientRing === null and (isField k or k.?isBasic)
	       or opts.CoefficientRing === k
	       )
	  then return flatCoerce(R, resultTemplate,triv R);
	  J := ideal presentation R;
	  A := ring J;
	  (I,p,q) := flattenRing(A,opts,Result => (Ideal,,));
	  I = I + p J;
	  p = map(target p, R, matrix p);
	  q = map(R, source q, promote(matrix q,R));
	  r := flatCoerce(R, resultTemplate,(I,p,q))))

flattenRing PolynomialRing := opts -> (cacheValue (symbol flattenRing => opts)) (R -> (
     resultTemplate := preprocessResultTemplate(1:Ring, opts.Result);
     if instance(resultTemplate,VisibleList) then apply(resultTemplate,x -> if x === null then Thing else x);
     A := coefficientRing R;
     Q := ultimate(ambient,R);
     J := lift(ideal map(R^1,R^0,0), Q);
     M := monoid R;
     n2 := numgens M;
     if opts.CoefficientRing === A or opts.CoefficientRing === null and (isField A or A.?isBasic)
     then return flatCoerce(R, resultTemplate,triv R);
     (I,p,q) := flattenRing(A, opts, Result => (Ideal,,));
     S := ring I;
     (n1,T) := (
     	  if instance(S,PolynomialRing)
     	  then ( numgens monoid S, (coefficientRing S) tensor(M,monoid S) )
     	  else ( 0, S M ));
     I' := (
	  (map(T, Q, vars T)) J
	  + 
	  (map(T,S,(vars T)_(toList (n2 .. n2 + n1 - 1)))) I
	  );
     (p,q) = (map(T,R, vars T), map(R,T, vars R | promote(matrix q,R)));
     r := flatCoerce(R, resultTemplate,(I',p,q))))

isWellDefined RingMap := f -> (
     R := source f;
     (S,p,q) := flattenRing(R,Result=>3);
     T := ambient S;
     I := ideal S;
     g := f * q * map(S,T);
     g I == 0)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
