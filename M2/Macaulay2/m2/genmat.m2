--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

needs "modules.m2" -- for Matrix

getIndex := (R,x) -> (
     M := try monoid R else error "expected a polynomial ring or quotient of one";
     if class x =!= R then error "expected an element of the ring";
     x = try baseName x else error "expected a variable of the ring";
     M.index#x)

genericMatrix = method(TypicalValue => Matrix)
genericMatrix(Ring,ZZ,ZZ) := (R,nrows,ncols) -> genericMatrix(R,R_0,nrows,ncols)
genericMatrix(Ring,RingElement,ZZ,ZZ) := (R,first,nrows,ncols) -> (
     first = getIndex(R,first);
     if not instance(nrows,ZZ) or not instance(ncols,ZZ) or nrows < 0 or ncols < 0
     then error "expected nonnegative integers";
     if first + nrows * ncols > numgens R
     then error "not enough variables in this ring";
     matrix table(nrows, ncols, (i,j)->R_(first + i + nrows*j)))

genericSkewMatrix = method(TypicalValue => Matrix)
genericSkewMatrix(Ring,ZZ) := (R,n) -> genericSkewMatrix(R,R_0,n)
genericSkewMatrix(Ring,RingElement,ZZ) := (R,first,n) -> (
     first = getIndex(R,first);
     vars := new MutableHashTable;
     nextvar := first;
     scan(0..n-1, 
	  i -> scan(i+1..n-1, 
	       j -> (vars#(i,j) = R_nextvar; nextvar = nextvar+1)));
     matrix table(n,n,(i,j) -> 
	  if i>j then - vars#(j,i) 
	  else if i<j then vars#(i,j)
	  else 0_R))

genericSymmetricMatrix = method(TypicalValue => Matrix)
genericSymmetricMatrix(Ring,ZZ) := (R,n) -> genericSymmetricMatrix(R,R_0,n)
genericSymmetricMatrix(Ring,RingElement,ZZ) := (R,first,n) -> (
     first = getIndex(R,first);
     vars := new MutableHashTable;
     nextvar := first;
     scan(0..n-1, i -> scan(i..n-1, j -> (
		    vars#(i,j) = R_nextvar; 
		    nextvar = nextvar+1)));
     matrix table(n,n, (i,j) -> if i>j then vars#(j,i) else vars#(i,j)))

randommat := opts -> (R,r,c) -> (
     s := randomHeight;
     randomHeight = opts.Height;
     m := map(R, rawRandomConstantMatrix(R.RawRing, r, c, 
	  opts.Density, 
	  if opts.UpperTriangular then 1 else 0,
	  0));
     randomHeight = s;
     m)

random(List,Ring) := RingElement => opts -> (deg,R) -> (
     if #deg =!= degreeLength R then error ("expected length of degree vector to be ", degreeLength R);
     if deg === {} then return random(R,opts);
     k := coefficientRing R;
     m := basis(deg, R);
     if m == 0 then 0_R
     else (
     	  n := matrix table(numgens source m,1, x -> promote(random(k,opts),R));
     	  (m*n)_(0,0)))

random(ZZ,Ring) := RingElement => opts -> (n,R) -> random({n},R,opts)

randomMR := opts -> (F,G) -> (
     R := ring F;
     m := numgens F;
     n := numgens G;
     k := min(m,n);
     d1 := toList ( degreeLength F : 1 );
     d0 := toList ( degreeLength F : 0 );
     f := id_(R^k);
     if m>k then f = f || random(R^(toList( m-k : d1 )), R^n, opts)
     else if n > k then f = f | random(R^m, R^(toList (n-k : -d1)), opts);
     f = mutableMatrix f;
     other := (i,m) -> (i + random(m-1)) % m;
     if m>k then (
	  for i to k-1 do rowAdd(f, i, random(d0,R,opts), random(k,m-1));
	  for i to k-1 do rowSwap(f, i, other(i,m)))
     else if n>k then (
	  for j to k-1 do columnAdd(f, j, random(d0,R,opts), random(k,n-1));
	  for j to k-1 do columnSwap(f, j, other(j,n)));
     if m>1 then (
	  for i to m-1 do (
	       rowAdd(f, i, random(d0,R,opts), other(i,m));
	       rowAdd(f, other(i,m), random(d0,R,opts), i);
	       );
	  for i to m-1 do (
	       rowAdd(f, i, random(d1,R,opts), other(i,m));
	       rowAdd(f, other(i,m), random(d1,R,opts), i);
	       );
	  );
     if n>1 then (
	  for j to n-1 do (
	       columnAdd(f, j, random(d0,R,opts), other(j,n));
	       columnAdd(f, other(j,n), random(d0,R,opts), j);
	       );
	  for j to n-1 do (
	       columnAdd(f, j, random(d1,R,opts), other(j,n));
	       columnAdd(f, other(j,n), random(d1,R,opts), j);
	       );
	  );
     map(F,G,new Matrix from f))

random(Module, Module) := Matrix => opts -> (F,G) -> (
     if not isFreeModule F or not isFreeModule G then error "random: expected free modules";
     R := ring F;
     if R =!= ring G then error "modules over different rings";
     if opts.MaximalRank then return (randomMR opts)(F,G);
     p := char R;
     if p === 0 then p = opts.Height;
     degreesTable := table(degrees F, degrees G, 
	  (i,j) -> toList apply(j,i,difference));
     degreesTally := tally flatten degreesTable;
     if #degreesTally === 0 then map(F,G,0)
     else if #degreesTally === 1 then (
	  deg := first keys degreesTally;
	  if all(deg,i->i===0) 
	  then (randmat := (randommat opts)(R,numgens F, numgens G);
	        map(F,G,randmat))
	  else (
	       m := basis(deg,R);
	       s := degreesTally#deg;
	       f := reshape(F,G, 
		    m * (randommat opts)(R, numgens source m, s));
	       -- note: "reshape" does not preserve homogeneity, so we restore it here:
	       f = map(F,G,f, Degree => toList (degreeLength R:0));
	       f))
     else (
	  randomElement := memoize(
	       deg -> (
		    numused := 0;
		    if deg === 0 then (
			 n := apply(degreesTally#deg, x -> random p);
			 () -> (
			      r := n#numused;
			      numused = numused + 1;
			      r))
		    else (
			 m := basis(deg,R);
			 k := numgens source m;
			 if k === 0
			 then () -> 0_R
			 else (
			      n = first entries (
				   m * matrix (R, table(
					     k, degreesTally#deg, 
					     (i,j)->random p)));
			      () -> (
				   r := n#numused;
				   numused = numused + 1;
				   r)))));
	  map(F, G, applyTable(degreesTable, k -> (randomElement k)()))))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
