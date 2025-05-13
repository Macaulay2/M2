--		Copyright 1995 by Daniel R. Grayson

importFrom_Core {
    "generatorSymbols",
}

Ext(ZZ, Module, Module) := Module => opts -> (i,M,N) -> (
     R := ring M;
     if not isCommutative R then error "'Ext' not implemented yet for noncommutative rings.";
     if R =!= ring N then error "expected modules over the same ring";
     if i < 0 then R^0
     else if i === 0 then Hom(M, N, opts)
     else (
	  C := resolution(M,LengthLimit=>i+1);
	  b := C.dd;
	  complete b;
	prune' := if opts.MinimalGenerators then prune else identity;
	prune' if b#?i then (
	    if b#?(i+1)
	    then homology(Hom(b_(i+1), N, opts), Hom(b_i, N, opts))
	    else cokernel Hom(b_i,     N, opts))
	else (
	    if b#?(i+1)
	    then kernel Hom(b_(i+1), N, opts)
	    else Hom(C_i, N, opts))))

Ext(ZZ, Matrix, Module) := Matrix => opts -> (i,f,N) -> (
     R := ring f;
     if not isCommutative R then error "'Ext' not implemented yet for noncommutative rings.";
     if R =!= ring N then error "expected modules over the same ring";
     prune' := if opts.MinimalGenerators then prune else identity;
     if i < 0 then map(R^0, R^0, {})
     else if i === 0 then Hom(f, N, opts)
     else prune'(
	  g := resolution(f,LengthLimit=>i+1);
	  Es := Ext^i(source f, N, opts);
	  Et := Ext^i(target f, N, opts);
	  psi := if Es.cache.?pruningMap then Es.cache.pruningMap else id_Es;
	  phi := if Et.cache.?pruningMap then Et.cache.pruningMap else id_Et;
	  psi^-1 * inducedMap(target psi, target phi, Hom(g_i, N, opts)) * phi))

-- TODO: is this correct?
-- c.f. https://github.com/Macaulay2/M2/issues/246
Ext(ZZ, Module, Matrix) := Matrix => opts -> (i,N,f) -> (
     R := ring f;
     if not isCommutative R then error "'Ext' not implemented yet for noncommutative rings.";
     if R =!= ring N then error "expected modules over the same ring";
     prune' := if opts.MinimalGenerators then prune else identity;
     if i < 0 then map(R^0, R^0, {})
     else if i === 0 then Hom(N, f, opts)
     else prune'(
	  C := resolution(N,LengthLimit=>i+1);
	  Es := Ext^i(N, source f, opts);
	  Et := Ext^i(N, target f, opts);
	  psi := if Es.cache.?pruningMap then Es.cache.pruningMap else id_Es;
	  phi := if Et.cache.?pruningMap then Et.cache.pruningMap else id_Et;
	  phi^-1 * inducedMap(target phi, target psi, Hom(C_i, f, opts)) * psi))

-- total ext over complete intersections

-- change 2009/1/12:
--  some modifications contributed 23 Mar 2007 by "Paul S. Aspinwall" <psa@cgtp.duke.edu>,
--  but we also get rid of the fudge factor entirely, depending instead on automatic
--  computation of the heft vector

-- also see Complexes/Ext.m2
Ext(Module, Module) := Module => opts -> (M, N) -> (
    -- c.f. caching in Hom(Module,Module)
    cacheModule := youngest(M.cache.cache, N.cache.cache);
    cacheKey := (Ext,M,N);
    if cacheModule#?cacheKey then return cacheModule#cacheKey;
  B := ring M;
  if B =!= ring N
  then error "expected modules over the same ring";
  if not isCommutative B
  then error "'Ext' not implemented yet for noncommutative rings.";
  if not isHomogeneous B
  then error "'Ext' received modules over an inhomogeneous ring";
  if not isHomogeneous N or not isHomogeneous M
  then error "'Ext' received an inhomogeneous module";
  if N == 0 or M == 0 then return cacheModule#cacheKey = B^0;
  p := presentation B;
  A := ring p;
  I := ideal mingens ideal p;
  n := numgens A;
  c := numgens I;
  if c =!= codim B 
  then error "total Ext available only for complete intersections";
  f := apply(c, i -> I_i);
  pM := lift(presentation M,A);
  pN := lift(presentation N,A);
  M' := cokernel ( pM | p ** id_(target pM) );
  N' := cokernel ( pN | p ** id_(target pN) );
  assert isHomogeneous M';
  assert isHomogeneous N';
  C := complete resolution M';
  X := getSymbol "X";
  K := coefficientRing A;
  S := K(monoid [X_1 .. X_c, toSequence A.generatorSymbols,
    Degrees => {
      apply(0 .. c-1, i -> prepend(-2, - degree f_i)),
      apply(0 .. n-1, j -> prepend( 0,   degree A_j))
      }]);
  -- make a monoid whose monomials can be used as indices
  Rmon := monoid [X_1 .. X_c,Degrees=>{c:{2}}];
  -- make group ring, so 'basis' can enumerate the monomials
  R := K Rmon;
  -- make a hash table to store the blocks of the matrix
  blks := new MutableHashTable;
  blks#(exponents 1_Rmon) = C.dd;
  scan(0 .. c-1, i -> 
       blks#(exponents Rmon_i) = nullhomotopy (- f_i*id_C));
  -- a helper function to list the factorizations of a monomial
  factorizations := (gamma) -> (
    -- Input: gamma is the list of exponents for a monomial
    -- Return a list of pairs of lists of exponents showing the
    -- possible factorizations of gamma.
    if gamma === {} then { ({}, {}) }
    else (
      i := gamma#-1;
      splice apply(factorizations drop(gamma,-1), 
	(alpha,beta) -> apply (0..i, 
	     j -> (append(alpha,j), append(beta,i-j))))));
  scan(4 .. length C + 1, 
    d -> if even d then (
      scan( flatten \ exponents \ leadMonomial \ first entries basis(d,R), 
	gamma -> (
	  s := - sum(factorizations gamma,
	    (alpha,beta) -> (
	      if blks#?alpha and blks#?beta
	      then blks#alpha * blks#beta
	      else 0));
	  -- compute and save the nonzero nullhomotopies
	  if s != 0 then blks#gamma = nullhomotopy s;
	  ))));
  -- make a free module whose basis elements have the right degrees
  spots := C -> sort select(keys C, i -> class i === ZZ);
  Cstar := S^(apply(spots C,
      i -> toSequence apply(degrees C_i, d -> prepend(i,d))));
  -- assemble the matrix from its blocks.
  -- We omit the sign (-1)^(n+1) which would ordinarily be used,
  -- which does not affect the homology.
  toS := map(S,A,apply(toList(c .. c+n-1), i -> S_i),
    DegreeMap => prepend_0);
  Delta := map(Cstar, Cstar, 
    transpose sum(keys blks, m -> S_m * toS sum blks#m),
    Degree => { -1, degreeLength A:0 });
  DeltaBar := Delta ** (toS ** N');
  if debugLevel > 10 then (
       assert isHomogeneous DeltaBar;
       assert(DeltaBar * DeltaBar == 0);
       stderr << describe ring DeltaBar <<endl;
       stderr << toExternalString DeltaBar << endl;
       );
  -- now compute the total Ext as a single homology module
  prune' := if opts.MinimalGenerators then prune else identity;
  cacheModule#cacheKey =
  prune' homology(DeltaBar,DeltaBar))
