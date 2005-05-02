--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

Module + Module := Module => (M,N) -> (
     if ring M =!= ring N
     then error "expected modules over the same ring";
     R := ring M;
     if ambient M != ambient N
     or M.?relations and N.?relations and M.relations != N.relations
     or M.?relations and not N.?relations
     or not M.?relations and N.?relations
     then error "expected submodules of the same module";
     subquotient(
	  if not M.?generators or not N.?generators then null else M.generators | N.generators,
	  if M.?relations then M.relations else null
	  )
     )

tensor(Module, Module) := Module => options -> (M,N) -> M**N
Module ** Module := Module => (M,N) -> (
-- don't cache it any more
--      P := youngest(M,N);
--      key := (M,N,symbol **);
--      if P#?key then P#key
--      else M**N = 
     (
	  if M.?generators and not isFreeModule N
	  or N.?generators and not isFreeModule M then (
	       if M.?generators then M = cokernel presentation M;
	       if N.?generators then N = cokernel presentation N;
	       );
	  R := ring M;
	  if R =!= ring N then error "expected modules over the same ring";
	  if isFreeModule M then (
	       if M == R^1 then N
	       else if isFreeModule N then (
		    if N == R^1 then M
		    else newModule(R, raw M ** raw N)
		    )
	       else subquotient(
		    if N.?generators then M ** N.generators,
		    if N.?relations then M ** N.relations))
	  else (
	       if isFreeModule N then (
		    if N == R^1 then M
		    else subquotient(
			 if M.?generators then M.generators ** N,
			 if M.?relations then M.relations ** N))
	       else cokernel map(R, rawModuleTensor( raw M.relations, raw N.relations )))))

Matrix ** Module := Matrix => (f,M) -> (
     -- P := youngest(f,M);
     -- key := (f,M,symbol **);
     -- if P#?key then P#key else 
     -- f**M = (
     	  f ** id_M
     --	  )
     )
Module ** Matrix := Matrix => (M,f) -> (
--      P := youngest(M,f);
--      key := (M,f,symbol **);
--      if P#?key then P#key
--      else M**f = 
     (
     	  id_M ** f
	  )
     )

-----------------------------------------------------------------------------
-- base change
-----------------------------------------------------------------------------
Module ** Ring := Module => (M,R) -> (
--      P := youngest(M,R);
--      key := (M,R,symbol **);
--      if P#?key then P#key
--      else M**R = 
     (
	  k := ring M;
	  if k === R then M
	  else (
	       try promote(1_k, R) else error "can't tensor by this ring";
	       if M.?generators then cokernel presentation M ** R
	       else if M.?relations then cokernel (M.relations ** R)
	       else if isQuotientOf(R,k) then R^(- degrees M)
	       else R^(rank M)
	       )
	  ))

Matrix ** Ring := Matrix => (f,R) -> (
     k := ring source f;
     S := ring target f;
     if k === R and S === R then f
     else if S === R then (
	  -- map(target f, (source f ** R) ** R^(-degree f), f)
	  map(target f, source f ** R, f, Degree => degree f)
	  )
     else map(
	  -- this will be pretty slow
	  target f ** R, source f ** R, applyTable(entries f, r -> promote(r,R)),
	  Degree => if isQuotientOf(R,k) then degree f else degree 1_R
	  )
     )

-----------------------------------------------------------------------------       
poincare Module := M -> (
     R := ring M;
     n := degreeLength R;
     if n == 0 then error "expected nonzero degree length";
     M = cokernel presentation M;
     -- if not isHomogeneous relations M then error "expected a homogeneous module";
     if M.cache.?poincare then M.cache.poincare else M.cache.poincare = (
     	  ZZn := degreesRing R;
	  g := leadTerm gb presentation M;
	  GGGG = g;
	  p := new ZZn from rawHilbert raw g;
	  assert( raw ring p === rawRing raw p );	    -- fix this!
	  if R.?Repair and R.Repair =!= identity then (
	       repair := R.Repair;
	       p = substitute(p,
		    toList apply( 0 .. n-1, 
		       	 i -> ZZn_i => ZZn_(
			      repair toList apply( 0 .. n-1,  -- should just have the matrix of Repair available! 
				   j -> if j === i then 1 else 0 ) ) ) ) );
          p))

hilbertFunction(ZZ,Module) :=
hilbertFunction(ZZ,Ring) :=
hilbertFunction(ZZ,Ideal) := (d,M) -> (
     f := hilbertSeries(M, Order => d+1);
     U := monoid ring f;
     u := U_0;
     f_(u^d))

hilbertFunction(List,Ring) := 
hilbertFunction(List,Ideal) := 
hilbertFunction(List,Module) := (d,M) -> (
     if not all(d,i->class i === ZZ) then error "expected degree to be an integer or list of integers";
     -- hilbertSeries to finite order doesn't work yet for multi-degrees
     -- we need more flexible power-series handling functions
     rank source basis(d,M)
     )

geometricSeries := (x,n) -> sum(n, i -> x^i)

trimm := (f,n) -> (
     R := ring f;
     sum(select(listForm f, (deg,coe) -> first deg < n), (deg,coe) -> coe * R_deg))

hilbertSeries PolynomialRing := options -> (R) -> hilbertSeries(R^1, options)
hilbertSeries Module := options -> (M) -> (
     ord := options.Order;
     if ord === infinity then (
	  if M.cache#?"exact hilbertSeries" then return M.cache#"exact hilbertSeries";
	  )
     else if class ord === ZZ then (
	  if M.cache#?"approximate hilbertSeries" then (
	       (ord2,ser) := M.cache#"approximate hilbertSeries";
	       if ord == ord2 then return ser;
	       if ord < ord2 then return trimm(ser,ord);
	       )
	  )
     else error "expected infinity or an integer as value of Order option";
     A := ring M;
     num := poincare M;
     T := degreesRing A;
     denom := tally (degree \ generators A);
     if ord === infinity then (
	  y := flatten apply(pairs denom, (i,e) -> Power {(1 - T_i),e});
	  M.cache#"exact hilbertSeries" = Divide{num, Product y})
     else if class ord === ZZ then (
	  s := if num == 0 then 0_T else (
	       m := min min(listForm num / first);
	       n := ord;
	       N := n - m;
	       f := num * product apply(
		    pairs denom,
		    (i,e) -> (geometricSeries( product(#i, j -> T_j ^ (i_j)), N)) ^ e
		    );
	       trimm(f,n));
	  M.cache#"approximate hilbertSeries" = (ord,s);
	  s))

nonnull := x -> select(x, i -> i =!= null)
reduceHilbert = method()
reduceHilbert Divide := ser -> (
     num := numerator ser;				    -- an element of the degrees ring
     den := denominator ser;				    -- a Product of Powers
     newden := Product nonnull apply(toList den, pwr -> (
	       fac := pwr#0;				    -- 1-T_i
	       ex  := pwr#1;	 			    -- exponent
	       while ex > 0 and num % fac == 0 do (
		    num = num // fac;
		    ex = ex - 1;
		    );
	       if ex > 0 then Power {fac,ex}));
     Divide {num, newden})

ProjectiveHilbertPolynomial = new Type of HashTable
ProjectiveHilbertPolynomial.synonym = "projective Hilbert polynomial"

ProjectiveHilbertPolynomial ZZ := (P,i) -> sum(pairs P, (n,c) -> c * binomial(n+i,n))

hilbertPolynomial = method(
     Options => { Projective => true }, 
     TypicalValue => ProjectiveHilbertPolynomial )

hilbertPolynomial Ideal := options -> (I) -> hilbertPolynomial((ring I)^1/I,options)

euler ProjectiveHilbertPolynomial := (P) -> P(0)
diff(ProjectiveHilbertPolynomial,ZZ) := ProjectiveHilbertPolynomial => (P,i) -> (
     new ProjectiveHilbertPolynomial from select(
     	  apply(pairs P, (n,c) -> (n-i,c)),
	  (n,c) -> n >= 0
	  ))
diff ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (P) -> diff(P,1)
ProjectiveHilbertPolynomial + ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (h,k) -> (
     select( merge(h,k,plus), c -> c =!= 0 )
     )
- ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => h -> applyValues(h,minus)
ProjectiveHilbertPolynomial - ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (h,k) -> h + -k
ProjectiveHilbertPolynomial == ProjectiveHilbertPolynomial := (h,k) -> h === k
dim ProjectiveHilbertPolynomial := (P) -> if #P === 0 then -1 else max keys P
degree ProjectiveHilbertPolynomial := (P) -> if #P === 0 then 0 else P#(dim P)
ZZ * ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (b,h) -> (
     if b === 1 then h 
     else if b === 0 then new ProjectiveHilbertPolynomial from {}
     else applyValues(h,c -> b*c)
     )

hilbertSeries ProjectiveHilbertPolynomial := options -> h -> (
     T := degreesRing 1;
     t := T_0;
     d := max keys h;
     new Divide from {
	  sum( apply( pairs h, (n,a) -> a * (1-t)^(d-n) ) ),
	  new Power from {1-t,d+1}
	  }
     )

PPP := new Holder from {"P"}
expression ProjectiveHilbertPolynomial := (h) -> (
     sum(sort pairs h, (n,c) -> c * new Subscript from {PPP, n})
     )	  
net ProjectiveHilbertPolynomial := (h) -> net expression h

projectiveHilbertPolynomial = method()
projectiveHilbertPolynomial ZZ := ProjectiveHilbertPolynomial => (n) -> (
     new ProjectiveHilbertPolynomial from { n => 1 }
     )
projectiveHilbertPolynomial(ZZ,ZZ) := ProjectiveHilbertPolynomial => memoize(
     (n,d) -> new ProjectiveHilbertPolynomial from (
     	  if d <= 0 
	  then apply(min(-d+1,n+1), j -> n-j => (-1)^j * binomial(-d,j))
     	  else apply(n+1, j -> n-j => binomial(d-1+j,j))))

i := symbol i
hilbertFunctionRing := QQ[i]

hilbertFunctionQ := method()
hilbertFunctionQ(ZZ) := (n) -> (
     if n === 0 then 1_hilbertFunctionRing
     else (1/n) * (n+i) * hilbertFunctionQ(n-1))
hilbertFunctionQ(ZZ,ZZ) := memoize(
     (n,d) -> (
     	  if d === 0 then hilbertFunctionQ(n)
     	  else substitute(hilbertFunctionQ(n), {i => i+d})))

hilbertPolynomial Module := ProjectiveHilbertPolynomial => o -> (M) -> (
    R := ring M;
    if degreeLength R != 1 
    then error "expected a singly graded ring";
    if not all((options R).Degrees, d -> d === {1})
    then error "expected a ring whose variables all have degree 1";
    n := numgens ring M - 1;
    f := poincare M;
    T := (ring f)_0;
    p := pairs standardForm f;
    if o.Projective 
    then (
	 if #p===0 
	 then new ProjectiveHilbertPolynomial from {}
	 else sum(p, (d,c) -> (
	      	   if #d === 0 then d = 0 else d = d#0;
	      	   c * projectiveHilbertPolynomial(n,-d))))
    else (
	 if #p===0
	 then 0_hilbertFunctionRing
	 else sum(p, (d,c) -> (
	      	   if #d === 0 then d = 0 else d = d#0;
	      	   c * hilbertFunctionQ(n,-d)))))

hilbertPolynomial Ring := ProjectiveHilbertPolynomial => options -> (R) -> hilbertPolynomial(R^1, options)

Ideal * Ring := Ideal => (I,S) -> if ring I === S then I else ideal(I.generators ** S)
Ring * Ideal := Ideal => (S,I) -> if ring I === S then I else ideal(I.generators ** S)

ZZ == Ideal := (n,I) -> I == n
Ideal == ZZ := (I,n) -> (
     if n === 0
     then I.generators == 0
     else if n === 1
     then 1_(ring I) % I == 0
     else error "attempted to compare ideal to integer not 0 or 1"
     )
ZZ == Module := (n,M) -> M == n
Module == ZZ := (M,n) -> (
     if n =!= 0 then error "attempted to compare module to nonzero integer";
     if M.?generators then (
	  if M.?relations then M.generators % M.relations == 0
	  else M.generators == 0
	  )
     else (
	  if M.?relations then (
	       f := M.relations;
	       id_(target f) % f == 0
	       )
	  else M.numgens === 0
	  )
     )

-- dim Module := M -> (
--      if degreeLength ring M === 0 
--      then error "can't compute dimension over a ring with zero degree length";
--      if not isHomogeneous M
--      then M = cokernel leadTerm generators gb presentation M;
--      if poincare M == 0
--      then -1
--      else 1 + dim hilbertPolynomial M + dim coefficientRing ring M
--      )

dim Module := M -> (
     c := codim M;
     if c === infinity then -1 else dim ring M - c
     )

degree Ring := R -> degree R^1
degree Module := M -> (
  hf := poincare M;
  T := (ring hf)_0;
  if hf == 0 then 0
  else (
       while substitute(hf,{T=>1}) == 0 do hf = hf // (1-T);
       -- while hf % (1 - T) == 0 do hf = hf // (1-T);
       substitute(hf,{T=>1})))

-----------------------------------------------------------------------------

presentation(Module) := Matrix => M -> (
     if M.cache.?presentation then M.cache.presentation else M.cache.presentation = (
	  if M.?generators then (
	       modulo( M.generators, if M.?relations then M.relations)
	       )
	  else relations M))
-----------------------------------------------------------------------------  

prune(Module) := Module => M -> (
     if M.cache.?pruningMap then M
     else if M.cache.?prune then M.cache.prune else M.cache.prune = (
	  R := ring M;
	  oR := options R;
	  if isFreeModule M then (
	       M.cache.pruningMap = id_M;
	       M)
	  else if (isAffineRing R and isHomogeneous M)
	         or (oR.?SkewCommutative and oR.SkewCommutative and isHomogeneous M) then (
	       f := presentation M;
	       g := complement f;
	       N := cokernel modulo(g, f);
	       N.cache.pruningMap = map(M,N,g);
	       N)
	  else (
	       f = generators gb presentation M;
	       -- MES: can't it do more here?
	       N = cokernel f;
	       N.cache.pruningMap = map(M,N,id_(cover M));
	       N)
	  )
     )

prune(Matrix) := Matrix => (m) -> (
     M := source m;
     if not M.cache.?pruningMap then m = m * (prune M).cache.pruningMap;
     N := target m;
     if not N.cache.?pruningMap then m = (prune N).cache.pruningMap^-1 * m;
     m)

-----------------------------------------------------------------------------

dual Module := Module => F -> if F.cache.?dual then F.cache.dual else F.cache.dual = (
     if not isFreeModule F then kernel transpose presentation F
     else newModule(ring F,rawDual raw F))

-----------------------------------------------------------------------------
Hom(Ideal, Ideal) := Module => (I,J) -> Hom(module I, module J)
Hom(Ideal, Module) := Module => (I,M) -> Hom(module I, M)
Hom(Module, Ideal) := Module => (M,I) -> Hom(M, module I)

Hom(Module, Ring) := Module => (M,R) -> Hom(M, R^1)
Hom(Ring, Module) := Module => (R,M) -> Hom(R^1, M)
Hom(Ideal, Ring) := Module => (I,R) -> Hom(module I, R^1)
Hom(Ring, Ideal) := Module => (R,I) -> Hom(R^1, module I)

Hom(Module, Module) := Module => (M,N) -> (
     if isFreeModule M 
     then dual M ** N
     else kernel Hom(presentation M, N)
     )
-- An alternate Hom routine:
Hom(Module, Module) := Module => (M,N) -> (
     -- This version is perhaps less transparent, but is
     -- easier to determine the link with homomorphisms.
     m := presentation M;
     mdual := transpose m;
     n := presentation N;
     h1 := modulo(mdual ** target n, target mdual ** n);
     MN := trim subquotient(h1,source mdual ** n);
     -- Now we store the information that 'homomorphism'
     -- will need to reconstruct the map corresponding to
     -- an element.
     MN.cache.Hom = {M,N,source mdual,target n};
     MN)

homomorphism = method()
homomorphism Matrix := Matrix => (f) -> (
     if not isFreeModule(source f) or 
        numgens source f =!= 1 or
        not (target f).cache.?Hom
	then error "homomorphism may only be determined for maps R --> Hom(M,N)";
     MN := (target f).cache.Hom;
     M := MN#0;
     N := MN#1;
     M0 := MN#2;
     N0 := MN#3;
     deg := (degrees source f)#0;
     map(N,M,adjoint1(super f, M0, N0),Degree=>deg))
-----------------------------------------------------------------------------
pdim Module := M -> length resolution M

Module / Module := Module => (M,N) -> (
     L := ambient M;
     if L != ambient N then error "expected modules with the same ambient module";
     R := ring M;
     if N.?generators
     then (
	  p := N.generators;
	  if M.?relations then (
	       p = p | M.relations;
	       );
	  subquotient(
	       if M.?generators then M.generators,
	       -- mingens image -- do we need this ???
	       p))
     else cokernel id_L)

Module / RingElement := Module => (M,x) -> M / (x * M)
Module / Sequence := Module / List := Module => (M,v) -> (
     R := ring M;
     v = toList v;
     if all(v, w -> class w === M)
     then M / image matrix v
     else if all(v, w -> class w === R)
     then M / (ideal v * M)
     else error("expected a list of elements of ", toString M, " or of ", toString R)
     )
Module / Vector := Module => (M,v) -> (
     if class v =!= M 
     then error("expected ", toString v, " to be an element of ", toString M);
     M / image matrix {v})
-----------------------------------------------------------------------------
--top Module := M -> (
--     R := ring M;
--     c := codim M; 
--     annihilator prune Ext^c(M, R))
--document { top,
--     TT "top M", "produce the annihilator of Ext^c(M, R), where c
--     is the codimension of the support of the module M."
--     }
-----------------------------------------------------------------------------

ann = annihilator

ann' = method()

ann' Module := Ideal => M -> (
     f := presentation M;
     image f : target f )

annihilator Module := Ideal => (M) -> (
     if M == 0 then ideal 1_(ring M)
     else (
	  P := presentation M;
	  F := target P;
	  intersect apply(numgens F, i-> ideal modulo(matrix{F_i},P))))

annihilator Ideal := Ideal => I -> annihilator module I
annihilator RingElement := Ideal => f -> annihilator ideal f

-----------------------------------------------------------------------------
ZZ _ Module := Vector => (i,M) -> (
     if i =!= 0 then error "expected 0 as element of module";
     m := map(M,(ring M)^1,0);
     new target m from {m})
Module _ ZZ := Vector => (M,i) -> (
     R := ring M;
     p := M_{i};
     d := first degrees source p;
     p = map(M,R^1,p,Degree => d);
     new target p from {p})
-----------------------------------------------------------------------------
Module ^ Array := Matrix => (M,w) -> if M.cache#?(symbol ^,w) then M.cache#(symbol ^,w) else M.cache#(symbol ^,w) = (
     -- we don't splice any more because natural indices include pairs (i,j).
     w = toList w;
     if not M.cache.?components then error "expected a direct sum module";
     if M.cache.?indexComponents then (
	  ic := M.cache.indexComponents;
	  w = apply(w, i -> if ic#?i 
		    then ic#i 
		    else error "expected an index of a component of a direct sum"));
     -- if the components of M have 3,4,5 generators, then
     -- we want to construct { (0,1,2), (3,4,5,6), (7,8,9,10,11) } for quick access
     k := 0;
     v := apply(M.cache.components, N -> k .. (k = k + numgens N) - 1);
     map(directSum M.cache.components_w, M, (cover M)^(splice apply(w, i -> v#i))))

Module _ Array := Matrix => (M,w) -> if M.cache#?(symbol _,w) then M.cache#(symbol _,w) else M.cache#(symbol _,w) = (
     -- we don't splice any more because natural indices include pairs (i,j).
     w = toList w;
     if not M.cache.?components then error "expected a direct sum module";
     if M.cache.?indexComponents then (
	  ic := M.cache.indexComponents;
	  w = apply(w, i -> if ic#?i 
		    then ic#i 
		    else error "expected an index of a component of a direct sum"));
     -- if the components of M have 3,4,5 generators, then
     -- we want to construct { (0,1,2), (3,4,5,6), (7,8,9,10,11) } for quick access
     k := 0;
     v := apply(M.cache.components, N -> k .. (k = k + numgens N) - 1);
     map(M, directSum M.cache.components_w, (cover M)_(splice apply(w, i -> v#i))))

-----------------------------------------------------------------------------
Module ^ List := Matrix => (M,rows) -> submatrix(id_M,rows,)
-----------------------------------------------------------------------------
Module _ List := Matrix => (M,v) -> (
     N := cover M;
     f := id_N_v;
     map(M, source f, f))
-----------------------------------------------------------------------------
basis(List,Module) := Matrix => opts -> (deg,M) -> basis(deg,deg,M,opts)
basis(List,List,Module) := Matrix => opts -> (lo,hi,M) -> (
     R := ring M;
     if #lo != 0 and #lo =!= degreeLength R
     or #hi != 0 and #hi =!= degreeLength R then error "expected degree length to match that of ring";
     if lo =!= hi and #lo > 1 then error "encountered a range of multi-degrees";
     heft := opts.Heft;
     var := opts.Variables;
     if var === null then var = 0 .. numgens R - 1
     else if class var === List then (
	  var = apply(var, v -> if class v === R then 
	                           index v 
				else if class v === ZZ 
				then v
				else error "expected list of ring variables or integers")
	  )
     else error "expected list of ring variables or integers";
     if R.?Adjust then (
	  lo = R.Adjust lo;
	  hi = R.Adjust hi;
	  if R.Adjust =!= identity and heft =!= null then error "encountered both Heft and Adjust options";
	  );
     if heft === null then heft = splice(1, degreeLength R - 1 : 0);
     A := ultimate(ambient,R);
     if not (
	  isAffineRing A 
	  or
	  isPolynomialRing A and isField coefficientRing A and (options A).SkewCommutative
	  or
	  isPolynomialRing A and ZZ === coefficientRing A
	  or
	  ZZ === A
	  ) then error "'basis' can't handle this type of ring";
     k := coefficientRing A;
     pres := generators gb presentation M;
     f := map(M,,rawBasis(raw pres, lo, hi, heft, var, opts.Truncate, opts.Limit));
     --s := sortColumns f;
     --f = f_s;
     f)

basis(ZZ,Module) := Matrix => opts -> (deg,M) -> basis({deg},M,opts)
basis(ZZ,ZZ,Module) := Matrix => opts -> (lo,hi,M) -> basis({lo},{hi},M,opts)
basis(List,Ideal) := basis(ZZ,Ideal) := Matrix => opts -> (deg,I) -> basis(deg,module I,opts)
basis(List,List,Ideal) := basis(ZZ,ZZ,Ideal) := Matrix => opts -> (lo,hi,I) -> basis(lo,hi,module I,opts)
basis(List,Ring) := Matrix => opts -> (deg,R) -> basis(deg, R^1,opts)
basis(List,List,Ring) := Matrix => opts -> (lo,hi,R) -> basis(lo,hi,R^1,opts)

basis(ZZ,Ring) := Matrix => opts -> (deg,R) -> basis({deg}, R^1,opts)
basis(ZZ,ZZ,Ring) := Matrix => opts -> (lo,hi,R) -> basis({lo},{hi}, R^1,opts)

basis Module := Matrix => opts -> M -> if M.cache.?basis then M.cache.basis else M.cache.basis = (
     -- check the following:
     --     R = ring m is a polynomial ring
     --     
     R := ring M;
     A := ultimate(ambient,R);
     if not isField coefficientRing A then error "expected ring to be an algebra over a field";
     -- the engine better catch this now:
     -- if dim M != 0 then error "expected module to be a finite dimensional module";
     k := coefficientRing A;
     pres := generators gb presentation M;
     map(M,,rawBasis(raw pres, {}, {}, splice(1, degreeLength R - 1 : 0), 0 .. numgens R - 1, false, -1)))

basis Ring := Matrix => opts -> R -> if R.?basis then R.basis else R.basis = basis(R^1,opts)
basis Ideal := Matrix => opts -> I -> if I.cache.?basis then I.cache.basis else I.cache.basis = basis(module I,opts)
-----------------------------------------------------------------------------

truncate(List,Ideal) := Ideal => (deg,I) -> ideal truncate(deg,module I)

truncate(List,Module) := Module => (deg,M) -> (
     if M.?generators then (
	  b := M.generators * cover basis(deg,deg,cokernel presentation M,Truncate=>true);
	  if M.?relations
	  then subquotient(b, M.relations)
	  else image b
	  )
     else image basis(deg,deg,M,Truncate=>true)
     )

truncate(ZZ,Module) := Module => (deg,M) -> truncate({deg},M)
truncate(ZZ,Ideal) := Ideal => (deg,M) -> truncate({deg},M)

issub := (f,g) -> -1 === rawGBContains(raw gb g,raw f)

isSubset(Module,Module) := (M,N) -> (
     -- here is where we could use gb of a subquotient!
     ambient M == ambient N and
     if M.?relations and N.?relations then (
	  image M.relations == image N.relations
	  and
	  issub(M.relations | generators M, N.relations | generators N))
     else if not M.?relations and not N.?relations then (
	  issub(generators M, generators N))
     else false
     )
isSubset(Ideal,Ideal) := (I,J) -> isSubset(module I, module J)
isSubset(Module,Ideal) := (M,J) -> isSubset(M, module J)
isSubset(Ideal,Module) := (I,N) -> isSubset(module I, N)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
