--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

complement = method()

mingens Module := Matrix => options -> (M) -> if M.?mingens then M.mingens else M.mingens = (
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
trim Module := Module => options -> (M) -> if M.?trim then M.trim else M.trim = (
     if isFreeModule M -- or not isHomogeneous M
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
	  N.trim = N;
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

Matrix // Matrix := Matrix => (f,g) -> (
     -- if ring g =!= ring f then error "expected maps over the same ring";
     M := target f;
     if M != target g then error "expected maps with the same target";
     L := source f;
     N := source g;
     f = matrix f;
     g = matrix g;
     map(N, L, f //
	  if M.?relations 
	  then gb(g | presentation M, 
	       ChangeMatrix => true, SyzygyRows => rank source g)
	  else gb(g,
	       ChangeMatrix => true),
	  Degree => degree f - degree g  -- do this in the engine instead
	  ))

RingElement // Matrix := (r,f) -> (r * id_(target f)) // f
ZZ           // Matrix := (r,f) -> promote(r,ring f) // f

Matrix // RingElement := (f,r) -> f // (r * id_(target f))

Matrix // ZZ           := (f,r) -> f // promote(r,ring f)

Matrix % Matrix := Matrix => (n,m) -> (
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

index RingElement := f -> (
    v := try (baseName f) else error("expected a ring variable but received ",toString f);
    (monoid ring f).index#v)

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

homogenize(Vector, RingElement, List) := Vector => (f,v,wts) -> (
     M := class f;
     wts = flatten wts;
     homogCheck(f,v,wts);
     new M from rawHomogenize(f.RawVector, index v, wts))

homogenize(Matrix, RingElement, List) := Matrix => (f,v,wts) -> (
     R := ring f;
     wts = flatten wts;
     homogCheck(f,v,wts);
     error "IM2_Matrix_homogenize not re-implemented yet";
     newMatrix(target f, source f, rawHomogenize(f.RawMatrix, index v, wts)))

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

homogenize(Vector, RingElement) := Vector => (f,n) -> (
    wts := (transpose (monoid ring f).Options.Degrees)#0;
    homogenize(f,n,wts)
    )

oldCoefficients(List, Matrix) := (v,m) -> (
    sendgg(ggPush m, ggPush v, ggcoeffs); 
    m1 := getMatrix ring m; 
    {m1, getMatrix ring m})

oldCoefficients(List, RingElement) := (v,m) -> (
     f := matrix{{m}};
     sendgg(ggPush f, ggPush v, ggcoeffs); 
     m1 := getMatrix ring m; 
     {m1, getMatrix ring m})

oldCoefficients(Matrix) := 
oldCoefficients(RingElement) := (m) -> (
     R := ring m;
     n := numgens R;
     coefficients(splice {0 .. n-1}, m))

coefficients(ZZ, Matrix) := coefficients(ZZ, RingElement) := (v,m) -> coefficients({v},m)

coefficients(List, Matrix) := (v,m) -> (
     R := ring m;
     v = splice v;
     error "IM2_Matrix_coeffs not implemented yet";
     sendgg(ggPush m, ggPush v, ggcoeffs); 
     {getMatrix R, getMatrix R})

coefficients(List, RingElement) := (v,m) -> coefficients(v,matrix{{m}})

coefficients(Matrix) := coefficients(RingElement) := (m) -> coefficients(toList (0 .. numgens ring m - 1), m)

terms RingElement := f -> (
     s := coefficients f;
     apply(first entries s#0,first entries s#1, times))

sortColumns = method ( 
     Options => {
	  DegreeOrder => Ascending,
	  MonomialOrder => Ascending
	  }
     )

sortColumns Matrix := options -> (f) -> (
     callgg(ggsortcolumns, f,
	  (
	       if options.DegreeOrder === Ascending then 1 else
	       if options.DegreeOrder === Descending then -1 else
	       if options.DegreeOrder === null then 0 else
	       error "expected DegreeOrder option value to be Ascending, Descending, or null"),
	  (
	       if options.MonomialOrder === Ascending then 1 else
	       if options.MonomialOrder === Descending then -1 else
	       error "expected MonomialOrder option value to be Ascending or Descending"));
     eePopIntarray())

-----------------------------
-- Matrix utility routines --
-----------------------------

selectInSubring = method()

selectInSubring(ZZ, Matrix) := Matrix => (i,m) -> (
     sendgg(ggPush m, ggdup, ggPush i, ggelim, ggsubmatrix);
     getMatrix ring m)

divideByVariable = method()

divideByVariable(Matrix, RingElement) := Matrix => (m,v) -> (
     if ring v =!= ring m then 
         error("must divide by a variable in the ring ", ring m);
     sendgg(ggPush m, ggPush index v, ggPush (-1), ggsat);
     getMatrix ring m)

divideByVariable(Matrix, RingElement, ZZ) := Matrix => (m,v,d) -> (
     if ring v =!= ring m then 
         error("must divide by a variable in the ring ", ring m);
     sendgg(ggPush m, ggPush index v, ggPush d, ggsat);
     getMatrix ring m)

compress = method()
--compress Matrix := (m) -> (
--     R := ring m;
--     sendgg( ggPush m, ggcompress );
--     getMatrix R)
compress Matrix := Matrix => (m) -> (
     R := ring m;
     submatrix(m, select(toList(0..numgens source m-1), i -> m_i != 0)))

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

-- promote(Matrix,Ring) := (f,S) -> (
--      error "this use of 'promote' has been replaced by '**'";
--      );
-- 
-- promote(Ideal,Ring) := (I,S) -> (
--      error "this use of 'promote' has been replaced by '*'";
--      );
