--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

needs "basis.m2"
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
     if nrows < 1 or ncols < 1
     then error "expected positive integers";
     if first + nrows * ncols > numgens R
     then error "not enough variables in this ring";
     matrix table(nrows, ncols, (i,j)->R_(first + i + nrows*j)))

genericSkewMatrix = method(TypicalValue => Matrix)
genericSkewMatrix(Ring,ZZ) := (R,n) -> genericSkewMatrix(R,R_0,n)
genericSkewMatrix(Ring,RingElement,ZZ) := (R,first,n) -> (
     if n < 1 then error "expected a positive integer";
     first = getIndex(R,first);
     if numgens R - first < binomial(n, 2)
     then error "not enough variables in this ring";
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
     if n < 1 then error "expected a positive integer";
     first = getIndex(R,first);
     if numgens R - first < binomial(n + 1, 2)
     then error "not enough variables in this ring";
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

other := (i, m) -> (i + random m) % m
randomMR := opts -> (F,G) -> (
     R := ring F;
     m := numgens F;
     n := numgens G;
     k := min(m,n);
     d1 := toList ( degreeLength R : 1 );
     d0 := toList ( degreeLength R : 0 );
     f := id_(R^k);
     if m>k then f = f || random(R^(toList( m-k : d1 )), R^n, opts)
     else if n > k then f = f | random(R^m, R^(toList (n-k : -d1)), opts);
     f = mutableMatrix f;
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
    if not isFreeModule G
    then return homomorphism random(Hom(G, F, DegreeLimit => 0), opts);
    if not isFreeModule F
    then return map(F, G, random(cover F, G, opts));
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

-- give a random vector in a module over a local (non-homogeneous) ring
localRandom = (M, opts) -> (
    R := ring M;
    -- TODO: which coefficient ring do we want?
    K := try coefficientRing R else R;
    v := random(cover M ** K, module K, opts);
    -- TODO: sub should be unnecessary, but
    -- see https://github.com/Macaulay2/M2/issues/3638
    vector inducedMap(M, , generators M * substitute(v, R)))

random(ZZ,   Module) :=
random(List, Module) := Vector => o -> (d, M) -> vector map(M, , random(cover M, (ring M)^{-d}, o))
random       Module  := Vector => o ->     M  -> (
    if isHomogeneous M then random(degree 1_(ring M), M, o) else localRandom(M, o))

random(ZZ,   Ideal) := RingElement => opts -> (d, I) -> random({d}, I, opts)
random(List, Ideal) := -* RingElement or List => *- opts -> (L, I) -> (
    m := generators I;
    r := degreeLength ring I;
    -- TODO: what should random({}, ideal 2_ZZ) do?
    if r == 0 then error "not yet implemented";
    if r == 1 and #L > 1 and isListOfIntegers L then L = transpose {L};
    if isListOfListsOfIntegers L then     first entries(m * random(source m, (ring I)^(-L), opts))
    else if isListOfIntegers L then first first entries(m * random(source m, (ring I)^{-L}, opts))
    else error("expected a list of integers or a list of lists of integers"))

------------------------------------
-- Code donated by Frank Schreyer --
------------------------------------

randomKRationalPoint = method()
randomKRationalPoint Ideal := I -> (
     R:=ring I;
     if char R == 0 then error "expected a finite ground field";
     if not class R === PolynomialRing then error "expected an ideal in a polynomial ring";
     if not isHomogeneous I then error "expected a homogeneous ideal";
     n:=dim I;
     if n<=1 then error "expected a positive dimensional scheme";
     c:=codim I;
     Rs:=R;
     Re:=R;
     f:=I;
     if not c==1 then (
         -- projection onto a hypersurface
         parametersystem:=ideal apply(n,i->R_(i+c));
         if not dim(I+parametersystem)== 0 then return print "make coordinate change";
         kk:=coefficientRing R;
         Re=kk(monoid[apply(dim R,i->R_i),MonomialOrder => Eliminate (c-1)]);
         rs:=(entries selectInSubring(1,vars Re))_0;
         Rs=kk(monoid[rs]);
         f=ideal substitute(selectInSubring(1, generators gb substitute(I,Re)),Rs);
         if not degree I == degree f then return print "make coordinate change"
         );
     H:=0;pts:=0;pts1:=0;trial:=1;pt:=0;ok:=false;
     while (
         H=ideal random(Rs^1,Rs^{dim Rs-2:-1});
         pts=decompose (f+H);
         pts1=select(pts,pt-> degree pt==1 and dim pt ==1);
         ok=( #pts1>0);
         if ok then (pt=saturate(substitute(pts1_0,R)+I);ok==(degree pt==1 and dim pt==0));
         not ok) do (trial=trial+1);
     pt
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
