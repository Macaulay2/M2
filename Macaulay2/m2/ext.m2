--		Copyright 1995 by Daniel R. Grayson

Ext = new ScriptedFunctor from {
     argument => (
	  (M,N) -> (
	       f := lookup(Ext,class M,class N);
	       if f === null then error "no method available"
	       else f(M,N)
	       )
	  ),	  
     superscript => (
	  i -> new ScriptedFunctor from {
	       argument => (X -> (
	       	    	 (M,N) -> (
		    	      f := lookup(Ext,class i,class M,class N);
		    	      if f === null then error "no method available"
		    	      else f(i,M,N)
		    	      )
	       	    	 ) X
	       	    )
	       }
	  )
     }
	  
Ext(ZZ, Module, Module) := Module => (i,M,N) -> (
     R := ring M;
     if not isCommutative R then error "'Ext' not implemented yet for noncommutative rings.";
     if R =!= ring N then error "expected modules over the same ring";
     if i < 0 then R^0
     else if i === 0 then Hom(M,N)
     else (
	  C := resolution(M,LengthLimit=>i+1);
	  b := C.dd;
	  complete b;
	  prune if b#?i then (
	       if b#?(i+1) 
	       then homology(Hom(b_(i+1),N), Hom(b_i,N))
	       else cokernel Hom(b_i,N))
	  else (
	       if b#?(i+1) 
	       then kernel Hom(b_(i+1),N)
	       else Hom(C_i,N))))

Ext(ZZ, Matrix, Module) := Matrix => (i,f,N) -> (
     R := ring f;
     if not isCommutative R then error "'Ext' not implemented yet for noncommutative rings.";
     if R =!= ring N then error "expected modules over the same ring";
     if i < 0 then R^0
     else if i === 0 then Hom(f,N)
     else (
	  g := resolution(f,LengthLimit=>i+1);
	  Es := Ext^i(source f, N);
	  Es':= target Es.pruningMap;	  -- Ext prunes everything, so get the original subquotient
	  Et := Ext^i(target f, N);
	  Et':= target Et.pruningMap;
	  Es.pruningMap^-1 * inducedMap(Es',Et',Hom(g_i,N)) * Et.pruningMap))

Ext(ZZ, Module, Matrix) := Matrix => (i,N,f) -> (
     R := ring f;
     if not isCommutative R then error "'Ext' not implemented yet for noncommutative rings.";
     if R =!= ring N then error "expected modules over the same ring";
     if i < 0 then R^0
     else if i === 0 then Hom(N,f)
     else (
	  C := resolution(N,LengthLimit=>i+1);
	  Es := Ext^i(N, source f);
	  Es':= target Es.pruningMap;	  -- Ext prunes everything, so get the original subquotient
	  Et := Ext^i(N, target f);
	  Et':= target Et.pruningMap;
	  Et.pruningMap^-1 * inducedMap(Et',Es',Hom(C_i,f)) * Es.pruningMap))

Ext(ZZ, Matrix, Ring) := (i,f,R) -> Ext^i(f,R^1)
Ext(ZZ, Matrix, Ideal) := (i,f,J) -> Ext^i(f,module J)
Ext(ZZ, Module, Ring) := (i,M,R) -> Ext^i(M,R^1)
Ext(ZZ, Module, Ideal) := (i,M,J) -> Ext^i(M,module J)
Ext(ZZ, Ideal, Ring) := (i,I,R) -> Ext^i(module I,R^1)
Ext(ZZ, Ideal, Ideal) := (i,I,J) -> Ext^i(module I,module J)
Ext(ZZ, Ideal, Module) := (i,I,N) -> Ext^i(module I,N)
Ext(ZZ, Ring, Ring) := (i,S,R) -> Ext^i(S^1,R^1)
Ext(ZZ, Ring, Ideal) := (i,S,J) -> Ext^i(S^1,module J)
Ext(ZZ, Ring, Module) := (i,S,N) -> Ext^i(S^1,N)

-- total ext over complete intersections

Ext(Module,Module) := Module => (N,M) -> (
  R := ring N;
  if R =!= ring M
  then error "expected modules over the same ring";
  if not isCommutative R
  then error "'Ext' not implemented yet for noncommutative rings.";
  if M == 0 then R^0
  else if N == 0 then R^0
  else (
    p := presentation R;
    Q := ring p;
    I := ideal mingens ideal p;
    n := numgens Q;
    c := numgens I;
    if c =!= codim R 
    then error "total Ext available only for complete intersections";
    f := apply(c, i -> I_i);
    toR := map(R,Q);
    pN := lift(presentation N,Q);
    pM := lift(presentation M,Q);
    N' := cokernel ( pN | p ** id_(target pN) );
    M' := cokernel ( pM | p ** id_(target pM) );
    E := resolution N';
    s := f / (g -> nullhomotopy (g*id_E));
    X := local X;
    k := coefficientRing Q;
    -- Make a linear transformation of the degrees to ensure
    -- the first component of each degree is positive.
    adjust := (
      if #f > 0
      then (
	fudge := 1 + max(first \ degree \ f) // 2;
  	v -> {- fudge * v#1 + v#0, - v#1}
	)
      else identity);
    degs := splice {
      apply(0 .. c-1,i -> adjust { - first degree f_i, -2}), 
      n : adjust {1,0}
      };
    T := k[X_0 .. X_(c-1), toSequence Q.generatorSymbols, 
      Degrees => degs];
    toT := map(T,Q,apply(toList(c .. c+n-1), i -> T_i));
    S := k[X_0 .. X_(c-1),Degrees=>{c:{2}}];
    mS := monoid S;
    use S;
    spots := E -> sort select(keys E, i -> class i === ZZ);
    -- make a hash table to store the blocks of the matrix
    Delta := new MutableHashTable;
    Delta#(exponents 1_mS) = -E.dd;
    scan(c, i -> Delta#(exponents mS_i) = s_i);
    -- a helper function to list the factorizations of a monomial
    factorizations := (m) -> (
      -- input: m is the list of exponents for a monomial
      -- Return a list of pairs of lists showing the factorizations.
      -- E.g., if m is {1,1} we return
      -- {({1,0},{0,1}),({0,1},{1,0}),({1,1},{0,0}),({0,0},{1,1})}
      if m === {} then { ({}, {}) }
      else (
	i := m#-1;
	splice apply(factorizations drop(m,-1), 
	  (n,o) -> apply (0 .. i, j -> (append(n,j), append(o,i-j))))));
    scan(4 .. length E + 1, 
      d -> if even d then (
	scan( exponents \ leadMonomial \ first entries basis(d,S), 
	  m -> (
	    h := sum(factorizations m,
	      (n,o) -> (
		if Delta#?n and Delta#?o
		then Delta#n * Delta#o
		else 0));
	    if h != 0 then (
	      -- compute and save the null homotopies
	      Delta#m = nullhomotopy h;
	      )))));
    -- make a free module whose basis elements have the right degrees
    DMT := T^(apply(spots E, 
	i -> toSequence apply(degrees E_i, d -> adjust {first d,i})));
    -- assemble the matrix from its blocks
    DT := map(DMT, DMT, 
      transpose sum ( keys Delta, m -> T_m * toT sum Delta#m),
      Degree => adjust {0,-1});
    D := DT ** toT M';
    -- now compute the total Ext as a single homology module
    ext := prune homology(D,D);
    -- stash the 'adjust' function we used
    ext.adjust = adjust;
    ext))

adjust					  -- just use it again
