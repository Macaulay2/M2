--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

complement = method()

mingens Module := Matrix => options -> (M) -> if M.cache.?mingens then M.cache.mingens else M.cache.mingens = (
     if M.?generators then (
	  if M.?relations then (
	       c := mingens gb (M.generators|M.relations,
		    options,
		    StopWithMinimalGenerators=>true,Syzygies=>false,ChangeMatrix=>false);
	       c * complement(M.relations // c))
	  else mingens gb (M.generators, 
	       options,
	       StopWithMinimalGenerators=>true,Syzygies=>false,ChangeMatrix=>false)
	  )
     else (
	  if M.?relations then complement M.relations
	  else id_M
	  )
     )

trim Ring := Ring => options -> (R) -> R
trim QuotientRing := options -> (R) -> (
     f := presentation R;
     A := ring f;
     A/(trim(ideal f,options)))
trim Module := Module => options -> (M) -> if M.cache.?trim then M.cache.trim else M.cache.trim = (
     if isFreeModule M
     then M
     else (
          -- this helps sometimes, even if M is *not* homogeneous : see test/trim.m2 and figure it out
	  g := mingens(M,options);
	  relns := if M.?relations then mingens(image M.relations,options);
	  N := (
	       if not isSubset(target g, image g)
	       then subquotient( g, relns )
	       else if relns === null then ambient M
	       else cokernel relns
	       );
	  N.cache.trim = N;
	  N)
     )

syz Matrix := Matrix => options -> (f) -> (
     if not isFreeModule target f or not isFreeModule source f
     then error "expected map between free modules";
     if ring f === ZZ or not isHomogeneous f
     then syz gb (f, options, Syzygies=>true)
     else mingens image syz gb (f, options, Syzygies=>true)
     )


modulo = method(
     Options => {
     	  -- DegreeLimit => {}
	  }
     )
modulo(Matrix,Nothing) := Matrix => options -> (m,null) -> syz(m,options)
modulo(Nothing,Matrix) := Matrix => options -> (null,n) -> n
modulo(Matrix,Matrix)  := Matrix => options -> (m,n) -> (
     P := target m;
     Q := target n;
     if P != Q then error "expected maps with the same target";
     if not isFreeModule P or not isFreeModule Q
     or not isFreeModule source m or not isFreeModule source n
     then error "expected maps between free modules";
     syz(m|n, options, SyzygyRows => numgens source m)
     )

quotientRemainder'(Matrix,Matrix) := Matrix => (f,g) -> (
     if not isFreeModule source f or not isFreeModule source g
     or not isFreeModule source g or not isFreeModule source g then error "expected maps between free modules";
     (q,r) := quotientRemainder(dual g, dual f);
     (dual q, dual r))

quotientRemainder(Matrix,Matrix) := Matrix => (f,g) -> (
     if ring g =!= ring f then error "expected maps over the same ring";
     M := target f;
     if M != target g then error "expected maps with the same target";
     L := source f;
     N := source g;
     f = matrix f;
     g = matrix g;
     G := (
	  if M.?relations 
	  then gb(g | presentation M, ChangeMatrix => true, SyzygyRows => rank source g)
	  else gb(g,                  ChangeMatrix => true)
	  );
     (rem,quo) := rawGBMatrixLift(raw G, raw f);
     (
	  map(N, L, quo, Degree => degree f - degree g),
	  map(M, L, rem)
     ))

Matrix // Matrix := Matrix => (f,g) -> quotient(f,g)
quotient'(Matrix,Matrix) := Matrix -> (f,g) -> (
     if not isFreeModule source f or not isFreeModule source g
     or not isFreeModule source g or not isFreeModule source g then error "expected maps between free modules";
     dual quotient(dual g, dual f))
quotient(Matrix,Matrix) := Matrix => opts -> (f,g) -> (
     -- if ring g =!= ring f then error "expected maps over the same ring";
     M := target f;
     if M != target g then error "expected maps with the same target";
     L := source f;
     N := source g;
     f = matrix f;
     g = matrix g;
     map(N, L, f //
	  if M.?relations 
	  then gb(g | presentation M, ChangeMatrix => true, SyzygyRows => rank source g)
	  else gb(g,                  ChangeMatrix => true),
	  Degree => degree f - degree g  -- do this in the engine instead
	  ))

RingElement // Matrix := (r,f) -> (r * id_(target f)) // f
ZZ           // Matrix := (r,f) -> promote(r,ring f) // f

Matrix // RingElement := (f,r) -> f // (r * id_(target f))

Matrix // ZZ           := (f,r) -> f // promote(r,ring f)

remainder'(Matrix,Matrix) := Matrix => (f,g) -> (
     if not isFreeModule source f or not isFreeModule source g
     or not isFreeModule source g or not isFreeModule source g then error "expected maps between free modules";
     dual remainder(dual g, dual f))
remainder(Matrix,Matrix) := Matrix % Matrix := Matrix => (n,m) -> (
     R := ring n;
     if R =!= ring m then error "expected matrices over the same ring";
     if not isFreeModule source n or not isFreeModule source m
     or not isFreeModule target n or not isFreeModule target m
     then error "expected maps between free modules";
     n % gb m)

Matrix % Module := Matrix => (f,M) -> f % gb M

RingElement % Matrix := (r,f) -> ((r * id_(target f)) % f)_(0,0)
RingElement % Ideal := (r,I) -> r % gb I
ZZ % Ideal := (r,I) -> r_(ring I) % gb I

Matrix % RingElement := (f,r) -> f % (r * id_(target f))

complement Matrix := Matrix => (m) -> (
     if not isHomogeneous m then error "expected homogeneous matrix";
     n := transpose syz transpose substitute(m,0);
     id_(target n) // n)

-------------------------------------
-- index number of a ring variable --
-------------------------------------
index = method()
index RingElement := f -> rawIndexIfVariable raw f

indices RingElement := (f) -> rawIndices raw f

support = method()
support RingElement := (f) -> (
     x := rawIndices raw f;
     apply(x, i -> (ring f)_i))

--------------------
-- homogenization --
--------------------
homogenize = method()

listZ := v -> (
     if not all(v,i -> class i === ZZ) then error "expected list of integers";
     )

homogCheck := (f, v, wts) -> (
    if ring f =!= ring v then error "homogenization requires variable in the same ring";
    listZ wts;
    if # wts != numgens ring f 
       then error "homogenization weight vector has incorrect length";)

homogenize(RingElement, RingElement, List) := RingElement => (f,v,wts) -> (
     R := ring f;
     wts = flatten wts;
     homogCheck(f,v,wts);
     new R from rawHomogenize(f.RawRingElement, index v, wts))

homogenize(Matrix, RingElement, List) := Matrix => (f,v,wts) -> (
     R := ring f;
     wts = flatten wts;
     homogCheck(f,v,wts);
     map(target f, source f, rawHomogenize(f.RawMatrix, index v, wts)))

homogenize(Matrix, RingElement) := Matrix => (f,n) -> (
     wts := (transpose (monoid ring f).Options.Degrees)#0;
     homogenize(f,n,wts)
     )

homogenize(Module,RingElement) := Module => (M,z) -> (
     if isFreeModule M then M
     else subquotient(
	  if M.?generators then homogenize(M.generators,z),
	  if M.?relations then homogenize(M.relations,z)))

homogenize(Ideal,RingElement) := Ideal => (I,z) -> ideal homogenize(module I, z)

homogenize(Module,RingElement,List) := Module => (M,z,wts) -> (
     if isFreeModule M then M
     else subquotient(
	  if M.?generators then homogenize(M.generators,z,wts),
	  if M.?relations then homogenize(M.relations,z,wts)))

homogenize(RingElement, RingElement) := RingElement => (f,n) -> (
     wts := (transpose (monoid ring f).Options.Degrees)#0;
     homogenize(f,n,wts)
     )

homogenize(Vector, RingElement, List) := Vector => (f,v,wts) -> (
     p := homogenize(f#0,v,wts);
     new target p from {p})
homogenize(Vector, RingElement) := Vector => (f,v) -> (
     p := homogenize(f#0,v);
     new target p from {p})

listOfVars := method()
listOfVars(Ring,Thing) := (R,x) -> 
     error(expected "'Variables=>' argument to be a List, Sequence, integer, or RingElement")
listOfVars(Ring,Nothing) := (R,x) -> toList(0 .. numgens R-1)
listOfVars(Ring,List) := (R,x) -> (
     vrs := splice x;
     types := unique apply(vrs,class);
     if #types != 1 then error "expected a list or sequence of integers or variables in the same ring";
     if first types =!= ZZ then vrs = index \ vrs;
     if any(vrs,i -> i === null) then error "expected a list of variables";
     vrs
     )
listOfVars(Ring,Sequence) := (R,x) -> listOfVars(R,toList x)
listOfVars(Ring,ZZ) := (R,x) -> (
     if x < 0 or x >= numgens R then
         error("expected an integer in the range 0 .. "|numgens R-1)
     else {x})
listOfVars(Ring,RingElement) := (R,x) -> (
     if class x === R 
     then {index x}
     else error "expected a ring element of the same ring")

coefficient(MonoidElement,RingElement) := (m,f) -> (
     RM := ring f;
     R := coefficientRing RM;
     M := monoid RM;
     if M =!= class m then error "expected monomial from same ring";     
     new R from rawCoefficient(raw R, raw f, raw m))
coefficient(RingElement,RingElement) := (m,f) -> (
     if size m != 1 or leadCoefficient m != 1 then error "expected a monomial";
     m = leadMonomial m;
     RM := ring f;
     R := coefficientRing RM;
     new R from rawCoefficient(raw R, raw f, raw m))
RingElement _ MonoidElement := RingElement => (f,m) -> coefficient(m,f)
RingElement _ RingElement := RingElement => (f,m) -> coefficient(m,f)

coefficients = method(Options => {Variables => null, Monomials => null})
coefficients(RingElement) := o -> (f) -> coefficients(matrix{{f}},o)
coefficients(Matrix) := o -> (f) -> (
     m := raw f;
     vrs := listOfVars(ring f,o.Variables);
     rawmonoms := if o.Monomials === null then
                    rawMonomials(vrs,m)
	          else if class o.Monomials === Matrix then
	            raw o.Monomials
	       else if class o.Monomials === List then raw matrix{o.Monomials}
	       else if class o.Monomials === Sequence then raw matrix{toList o.Monomials}
	       else error "expected 'Monomials=>' argument to be a list, sequence, or matrix";
     monoms := map(target f,,rawTensor(rawIdentity(raw target f,0),rawmonoms));
     (monoms,map(source monoms,source f,rawCoefficients(vrs,rawmonoms,m)))
     )

--coefficients(Matrix) := coefficients(RingElement) := (m) -> coefficients(toList (0 .. numgens ring m - 1), m)
--coefficients(List, RingElement) := (v,m) -> coefficients(v,matrix{{m}})
--coefficients(Sequence, RingElement) := (v,m) -> coefficients(toList v,matrix{{m}})
--coefficients(ZZ, Matrix) := coefficients(ZZ, RingElement) := (v,m) -> coefficients({v},m)
--coefficients(Sequence, Matrix) := (vrs,f) -> coefficients(toList vrs,f)
--coefficients(List, Matrix) := (vrs,f) -> (
--     m := raw f;
--     vrs = splice vrs;
--     types := unique apply(vrs,class);
--     if #types != 1 then error "expected a list or sequence of integers or variables in the same ring";
--     R := first types;
--     if R =!= ZZ then vrs = index \ vrs;
--     if any(vrs,i -> i === null) then error "expected a list of variables";
--     monoms := map(target f,,rawTensor(rawIdentity(raw target f,0),rawMonomials(vrs, m)));
--     (monoms,map(source monoms,source f,rawCoefficients(vrs,raw monoms,m))))

monomials = method(Options => {Variables => null})
monomials(RingElement) := o -> (f) -> monomials(matrix{{f}},o)
monomials(Matrix) := o -> (f) -> (
     vrs := listOfVars(ring f,o.Variables);
     map(target f,,rawMonomials(vrs, raw f))
     )

--monomials(Matrix) := monomials(RingElement) := (m) -> monomials(toList (0 .. numgens ring m - 1), m)
--monomials(List, RingElement) := (v,m) -> monomials(v,matrix{{m}})
--monomials(Sequence, RingElement) := (v,m) -> monomials(toList v,matrix{{m}})
--monomials(ZZ, Matrix) := monomials(ZZ, RingElement) := (v,m) -> monomials({v},m)
--monomials(Sequence, Matrix) := (vrs,f) -> monomials(toList vrs,f)
--monomials(List, Matrix) := (vrs,f) -> (
--     m := raw f;
--     vrs = splice vrs;
--     types := unique apply(vrs,class);
--     if #types != 1 then error "expected a list or sequence of integers or variables in the same ring";
--     R := first types;
--     if R =!= ZZ then vrs = index \ vrs;
--     if any(vrs,i -> i === null) then error "expected a list of variables";
--     map(target f,,rawMonomials(vrs, m)))

terms RingElement := f -> (
     (m,c) := flatten \ entries \ coefficients f;
     apply(m,c,times))

sortColumns Matrix := o -> (f) -> toList rawSortColumns(
	  raw f,
	  if o.DegreeOrder === Ascending then 1 else
	  if o.DegreeOrder === Descending then -1 else
	  if o.DegreeOrder === null then 0 else
	  error "expected DegreeOrder option value to be Ascending, Descending, or null",
	  if o.MonomialOrder === Ascending then 1 else
	  if o.MonomialOrder === Descending then -1 else
	  error "expected MonomialOrder option value to be Ascending or Descending")

sort Matrix := o -> (f) -> f_(sortColumns(f,o))
rsort Matrix := o -> (f) -> f_(reverse sortColumns(f,o))

-----------------------------
-- Matrix utility routines --
-----------------------------

selectInSubring = method()

selectInSubring(ZZ, Matrix) := Matrix => (i,m) -> (
     p := rawEliminateVariables(i,m.RawMatrix);
     submatrix(m, p))

divideByVariable = method()

divideByVariable(Matrix, RingElement) := Matrix => (m,v) -> (
     if ring v =!= ring m then 
         error("must divide by a variable in the ring ", ring m);
     (m1,topdegree) := rawDivideByVariable(m.RawMatrix, index v, -1);
     (map(ring m, m1), topdegree))

divideByVariable(Matrix, RingElement, ZZ) := Matrix => (m,v,d) -> (
     if ring v =!= ring m then 
         error("must divide by a variable in the ring ", ring m);
     (m1,topdegree) := rawDivideByVariable(m.RawMatrix, index v, d);
     (map(ring m, m1), topdegree))

compress = method()

compress Matrix := Matrix => (m) -> map(ring m, rawMatrixCompress m.RawMatrix)

diagonalMatrix = method()
diagonalMatrix Matrix := Matrix => (m) -> (
     R := ring m;
     nrows := numgens target m;
     if nrows === 0 then
       error "expected at least one row";
     if nrows > 1 then m = flatten m;
     a := numgens source m;
     m1 := mutableZero(R,a,a);
     for i from 0 to a-1 do m1_(i,i) = m_(0,i);
     matrix m1)

newCoordinateSystem = method()

newCoordinateSystem(PolynomialRing, Matrix) := (S,x) -> (
  -- x should be a one row matrix of linear forms
  -- S should be a ring, with the same number of variables as ring x.
  -- MES will document this and maybe change its name
  R := ring x;
  if numgens R != numgens S 
  then error "newCoordinateSystem requires input rings to have the same number of variables";
     -- probably should also check:
     -- (a) entries of 'x' are linear and independent
     -- (b) what if R,S, are quotient rings
  m := contract(transpose vars R, x);
  n := complement m | m;
  { map(S,R,vars S * substitute(n, S)), map(R,S,vars R * n^(-1))}
  )

lift(Matrix,Ring) := Matrix => (f,S) -> (
     -- this will be pretty slow and stupid
     if ring target f === S then f
     else if isQuotientOf(ring f,S) and
	     isFreeModule source f and
	     isFreeModule target f then
	 map(S^(-degrees target f), S^(-degrees source f), 
	     applyTable(entries f, r -> lift(r,S)))
     else matrix(S, applyTable(entries f, r -> lift(r,S)))
     )

lift(Ideal,Ring) := Ideal => (I,S) -> (
     -- provisional, just for quotient rings
     T := ring I;
     if T === S then I
     else ideal lift(I.generators,S) + ideal presentation(S,T));

-- computes the ideal of p x p permanents of the matrix M
permanents = method()
permanents(ZZ,Matrix) := Ideal => (p,M) -> (
     r:=numgens target M;
     c:=numgens source M;
     xxX := symbol xxX;
     R1:=ZZ/2[xxX_(1,1)..xxX_(r,c)];
     M1:= transpose genericMatrix(R1,xxX_(1,1),c,r);
     D1:= minors(p,M1);
     R2:=ZZ[xxX_(1,1)..xxX_(r,c)];
     D1=substitute(D1,R2);
     F = map(ring M, R2,flatten entries M);
     F D1)

-- promote(Matrix,Ring) := (f,S) -> (
--      error "this use of 'promote' has been replaced by '**'";
--      );
-- 
-- promote(Ideal,Ring) := (I,S) -> (
--      error "this use of 'promote' has been replaced by '*'";
--      );

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
