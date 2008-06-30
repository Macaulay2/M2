--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

Ideal * Vector := (I,v) -> (
     image((generators I) ** v#0)
     )

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
	       else new Module from (R, raw M ** raw N)
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
	  else cokernel map(R, rawModuleTensor( raw M.relations, raw N.relations ))))

Matrix ** Module := Matrix => (f,M) -> f ** id_M
Module ** Matrix := Matrix => (M,f) -> id_M ** f

-----------------------------------------------------------------------------
-- base change
-----------------------------------------------------------------------------
Module ** Ring := Module => (M,R) -> R ** M		    -- grandfathered, even though our modules are left modules

Ring ** Module := Module => (R,M) -> (
     A := ring M;
     try promote(1_A, R) else error "can't tensor by this ring";
     if A === R then M
     else if M.?generators then cokernel promote(presentation M, R)
     else if M.?relations then cokernel promote(M.relations,R)
     else R^(promote(- degrees M,A,R)))

Matrix ** Ring := Matrix => (f,R) -> R ** f		    -- grandfathered, even though our modules are left modules

Ring ** Matrix := Matrix => (R,f) -> (
     B := ring source f;
     A := ring target f;
     if B === R and A === R then f
     else map( target f ** R, source f ** R, promote(cover f, R), Degree => first promote({degree f}, A, R) )
     )

-----------------------------------------------------------------------------       
poincare Module := (cacheValue symbol poincare) (
     M -> (
	  -- some examples compute degrees of inhomogeneous modules, so we can't refuse to compute when the module is not homogeneous.
	  -- is it guaranteed to work in some sense?
	  -- if not isHomogeneous M then error "expected a homogeneous module";
	  new degreesRing M from rawHilbert raw leadTerm gb presentation cokernel presentation M))

hilbertFunction(ZZ,Module) := hilbertFunction(ZZ,Ring) := hilbertFunction(ZZ,Ideal) := (d,M) -> hilbertFunction({d},M)
hilbertFunction(List,Ring) := hilbertFunction(List,Ideal) := hilbertFunction(List,Module) := (d,M) -> (
     if not all(d,i->class i === ZZ) then error "expected degree to be an integer or list of integers";
     if degreeLength M =!= #d then error "degree length mismatch";
     f := hilbertSeries(M, Order => sum d+1);
     U := monoid ring f;
     coefficient(U_d,f))

geometricSeries := (x,n) -> sum(n, i -> x^i)

trimm := (f,n) -> (
     R := ring f;
     0_R + sum(select(listForm f, (deg,coe) -> first deg < n), (deg,coe) -> coe * R_deg))

hilbertSeries PolynomialRing := options -> (R) -> hilbertSeries(R^1, options)
hilbertSeries Ring := options -> (R) -> error "no method for computing hilbert series for this ring"
hilbertSeries Module := options -> (M) -> (
     -- some examples compute degrees of inhomogeneous modules, so we can't refuse to compute when the module is not homogeneous.
     -- is it guaranteed to work in some sense?
     -- if not isHomogeneous M then error "expected a homogeneous module";
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
     denom := tally (degree \ select(generators A, x -> x != 0));
     if ord === infinity then (
	  y := apply(pairs denom, (i,e) -> {1 - T_i,e});
	  y = sort y;
	  y = apply(y, t -> Power t);
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

expression ProjectiveHilbertPolynomial := (h) -> (
     sum(sort pairs h, (n,c) -> c * new Subscript from {"P", n})
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

hilbertFunctionRing <- QQ(monoid [getGlobalSymbol "i"])
i := hilbertFunctionRing_0

hilbertFunctionQ = method()
hilbertFunctionQ(ZZ) := (n) -> (
     if n === 0 then 1_hilbertFunctionRing
     else (1/n) * (n+i) * hilbertFunctionQ(n-1))
hilbertFunctionQ(ZZ,ZZ) := memoize(
     (n,d) -> (
     	  if d === 0 then hilbertFunctionQ(n)
     	  else substitute(hilbertFunctionQ(n), {i => i+d})))

hilbertPolynomial Module := ProjectiveHilbertPolynomial => o -> (M) -> (
    if not isHomogeneous M then error "expected a homogeneous module";
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

issub := (f,g) -> -1 === rawGBContains(raw gb g,raw f)	    -- we can do better in the homogeneous case!

ZZ == Module := (n,M) -> M == n
Module == ZZ := (M,n) -> (
     if n =!= 0 then error "attempted to compare module to nonzero integer";
     if M.?generators then (
	  if M.?relations then issub(M.generators, M.relations)
	  else M.generators == 0
	  )
     else (
	  if M.?relations then issub(id_(ambient M), M.relations)
	  else M.numgens === 0
	  )
     )

dim Module := M -> (
     c := codim M;
     if c === infinity then -1 else dim ring M - c
     )

fixZZ := x -> if liftable(x,ZZ) then lift(x,ZZ) else x

degree Ring := R -> degree R^1
degree Module := (
     () -> (
     	  -- constants:
	       ZZ1 := degreesRing 1;
	       T := ZZ1_0;
	       h := 1 - T;
	  M -> (
	       ev := map(ZZ,ZZ1,{1});			    -- ring maps are defined later
	       hs := hilbertSeries M;
	       hn := numerator hs;
	       hd := value denominator hs;
	       if hn == 0 then return 0;
	       n := degreeLength M;
	       if n === 0 then return lift(hn,ZZ);		    -- assert( hd == 1 );
	       if n > 1 then (
		    ZZn := degreesRing n;
		    to1 := map(ZZ1,ZZn,toList(n:T));
		    hn = to1 hn;
	       	    if hn == 0 then return 0;
		    hd = to1 hd;
		    );
	       while hn % h == 0 do hn = hn // h;
	       while hd % h == 0 do hd = hd // h;
	       fixZZ(ev hn/ev hd))))()

length Module := M -> (
     if not isHomogeneous M then notImplemented();
     if dim M > 0 then return infinity;
     degree M)

-----------------------------------------------------------------------------

presentation(Module) := Matrix => M -> (
     if M.cache.?presentation then M.cache.presentation else M.cache.presentation = (
	  if M.?generators then (
	       modulo( M.generators, if M.?relations then M.relations)
	       )
	  else relations M))
-----------------------------------------------------------------------------  

minimalPresentation(Module) := Module => opts -> (cacheValue symbol minimalPresentation) (M -> (
	  if isFreeModule M then (
	       M.cache.pruningMap = id_M;
	       return M);
	  homog := isHomogeneous M;
	  if homog then pushvar(symbol flagInhomogeneity,true);
	  C := runHooks(Module,symbol minimalPresentation,(opts,M));
	  if homog then popvar symbol flagInhomogeneity;
	  if C =!= null then return C;
	  error "minimalPresentation: internal error: no method for this type of module"
	  ))

addHook(Module, symbol minimalPresentation, (opts,M) -> (
	  -- we try to handle any module here, without any information about the ring
	  f := mutableMatrix mingens gb presentation M;
	  row := 0;
	  piv := new MutableHashTable;
	  pivColumns := new MutableHashTable;
	  scan(numRows f, row -> (
	       scan(numColumns f, col -> if not pivColumns#?col and isUnit f_(row,col) then (
			      piv##piv = (row,col);
			      pivColumns#col = true;
			      columnMult(f,col,1//f_(row,col));
			      scan(numColumns f, j -> if j != col then columnAdd(f,j,-f_(row,j),col));
			      break))));
	  piv = values piv;
	  f = matrix f;
	  rows := first \ piv;
	  cols := last \ piv;
	  f = submatrix'(f,rows,cols);
	  N := cokernel f;
	  N.cache.pruningMap = map(M,N,submatrix'(id_(cover M),rows));
	  break N))

addHook(Module, symbol minimalPresentation, (opts,M) -> (
     	  R := ring M;
	  if (isAffineRing R and isHomogeneous M) or (R.?SkewCommutative and isField coefficientRing R and isHomogeneous M) then (
	       f := presentation M;
	       g := complement f;
	       N := cokernel modulo(g, f);
	       N.cache.pruningMap = map(M,N,g);
	       break N)))

addHook(Module, symbol minimalPresentation, (opts,M) -> (
     	  R := ring M;
	  if R === ZZ then (
	       f := presentation M;
	       (g,ch) := smithNormalForm(f, ChangeMatrix => {true, false});
	       piv := select(pivots g,ij -> abs g_ij === 1);
	       rows := first \ piv;
	       cols := last \ piv;
	       (g,ch) = (submatrix'(g,rows,cols),submatrix'(ch,rows,));
	       N := cokernel g;
	       N.cache.pruningMap = map(M,N,id_(target ch) // ch);	    -- yuk, taking an inverse here, gb should give inverse change matrices, or the pruning map should go the other way
	       break N)))

addHook(Module, symbol minimalPresentation, (opts,M) -> (
     	  R := ring M;
	  if instance(R,PolynomialRing) and numgens R === 1 and isField coefficientRing R and not isHomogeneous M then (
	       f := presentation M;
	       k := coefficientRing R;
	       x := local x;
	       S := k[x, MonomialOrder => {Position => Down}];
	       p := map(S,R,vars S);
	       p' := map(R,S,vars R);
	       f = p f;
	       (g,ch) := smithNormalForm(f, ChangeMatrix => {true, false});
	       isunit := r -> r != 0 and degree r === {0};
	       piv := select(pivots g,ij -> isunit g_ij);
	       rows := first \ piv;
	       cols := last \ piv;
	       (g,ch) = (submatrix'(g,rows,cols),submatrix'(ch,rows,));
	       (g,ch) = (p' g,p' ch);
	       N := cokernel g;
	       N.cache.pruningMap = map(M,N,id_(target ch) // ch);	    -- yuk, taking an inverse here, gb should give inverse change matrices, or the pruning map should go the other way
	       break N)))

minimalPresentation(Matrix) := Matrix => opts -> (m) -> (
     M := source m;
     if not M.cache.?pruningMap then m = m * (minimalPresentation M).cache.pruningMap;
     N := target m;
     if not N.cache.?pruningMap then m = (minimalPresentation N).cache.pruningMap^-1 * m;
     m)

factor Module := opts -> (M) -> (
     R := ring M;
     if isField R then Sum { Power { expression R, rank M } }
     else if R === ZZ or instance(R,PolynomialRing) and numgens R == 1 and isField coefficientRing R then (
	  p := presentation minimalPresentation M;
	  m := numgens target p;
	  n := numgens source p;
	  t := tally apply(pivots p, (i,j) -> if R === ZZ then abs p_(i,j) else p_(i,j));
	  if m > n then t = t + new Tally from { 0 => m-n };
	  eR := if hasAttribute(R,ReverseDictionary) then getAttribute(R,ReverseDictionary) else expression R;
	  Sum apply(sort pairs t, (d,e) -> Power { if d === 0 then hold eR else Divide{ eR, factor d}, e }))
     else error "expected module over ZZ, k[x], or a field"
     )

-----------------------------------------------------------------------------

dual Module := Module => F -> if F.cache.?dual then F.cache.dual else F.cache.dual = (
     if not isFreeModule F then kernel transpose presentation F
     else new Module from (ring F,rawDual raw F))

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
     homog := isHomogeneous M and isHomogeneous N;
     if homog then pushvar(symbol flagInhomogeneity,true);
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
     if homog then popvar symbol flagInhomogeneity;
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
--topComponents Module := M -> (
--     R := ring M;
--     c := codim M; 
--     annihilator minimalPresentation Ext^c(M, R))
--document { topComponents,
--     TT "topComponents M", "produce the annihilator of Ext^c(M, R), where c
--     is the codimension of the support of the module M."
--     }
-----------------------------------------------------------------------------

annihilator = method(
     Options => {
	  Strategy => Intersection			    -- or Quotient
	  }
     )

annihilator Module := Ideal => o -> (M) -> (
     f := presentation M;
     if o.Strategy === Intersection then (
	  F := target f;
	  if numgens F === 0 then ideal 1_(ring F)
	  else intersect apply(numgens F, i -> ideal modulo(F_{i},f)))
     else if o.Strategy === Quotient then image f : target f
     else error "annihilator: expected Strategy option to be Intersection or Quotient")

annihilator Ideal := Ideal => o -> I -> annihilator(module I, o)
annihilator RingElement := Ideal => o -> f -> annihilator(ideal f, o)

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
basis = method(
     TypicalValue => Matrix,
     Options => new OptionTable from {
	  Truncate => false,
	  Limit => -1,
	  Variables => null,
	  SourceRing => null -- defaults to ring of the module, but accepts the coefficient ring
     	  })
basis(InfiniteNumber,InfiniteNumber,Module) := 
basis(List,InfiniteNumber,Module) := 
basis(InfiniteNumber,List,Module) := 
basis(List,List,Module) := opts -> (lo,hi,M) -> (
     R := ring M;
     if lo === infinity then error "incongruous lower degree bound: infinity";
     if hi === neginfinity then error "incongruous upper degree bound: -infinity";
     if lo === neginfinity then lo = {};
     if hi === infinity then hi = {};
     if #lo != 0 and #lo =!= degreeLength R or #hi != 0 and #hi =!= degreeLength R then error "expected degree length to match that of ring";
     if lo =!= hi and #lo > 1 then error "degree rank > 1 and degree bounds differ";
     var := opts.Variables;
     if var === null then var = 0 .. numgens R - 1
     else if class var === List then (
	  var = apply(var, v -> if instance(v,R) then index v 
				else if instance(v,ZZ) then v
				else error "expected list of ring variables or integers"))
     else error "expected list of ring variables or integers";
     A := ultimate(ambient,R);
     if not (
	  isAffineRing A 
	  or
	  isPolynomialRing A and isField coefficientRing A and A.?SkewCommutative
	  or
	  isPolynomialRing A and ZZ === coefficientRing A
	  or
	  ZZ === A
	  ) then error "'basis' can't handle this type of ring";
     k := coefficientRing A;
     pres := generators gb presentation M;
     heft := (
     	  optR := options R;
	  if optR =!= null then optR.Heft
	  );
     heft = if heft === null then 1:1 else toSequence heft;
     M.cache#"rawBasis log" = log := FunctionApplication { rawBasis, (raw pres, lo, hi, heft, var, opts.Truncate, opts.Limit) };
     if opts.SourceRing === null or opts.SourceRing === A then map(M,,value log)
     else map(M,,map(A,opts.SourceRing),value log))

basis(List,Module) := opts -> (deg,M) -> basis(deg,deg,M,opts)
basis(ZZ,Module) := opts -> (deg,M) -> basis({deg},M,opts)
basis(InfiniteNumber,ZZ,Module) := opts -> (lo,hi,M) -> basis(lo,{hi},M,opts)
basis(ZZ,InfiniteNumber,Module) := opts -> (lo,hi,M) -> basis({lo},hi,M,opts)
basis(ZZ,ZZ,Module) := opts -> (lo,hi,M) -> basis({lo},{hi},M,opts)

basis(List,Ideal) := basis(ZZ,Ideal) := opts -> (deg,I) -> basis(deg,module I,opts)

basis(InfiniteNumber,InfiniteNumber,Ideal) := 
basis(List,InfiniteNumber,Ideal) := 
basis(InfiniteNumber,List,Ideal) := 
basis(InfiniteNumber,ZZ,Ideal) := 
basis(ZZ,InfiniteNumber,Ideal) := 
basis(InfiniteNumber,InfiniteNumber,Ideal) := 
basis(List,ZZ,Ideal) := 
basis(ZZ,List,Ideal) := 
basis(List,List,Ideal) := 
basis(ZZ,ZZ,Ideal) := opts -> (lo,hi,I) -> basis(lo,hi,module I,opts)

basis(InfiniteNumber,InfiniteNumber,Ring) := 
basis(List,InfiniteNumber,Ring) := 
basis(InfiniteNumber,List,Ring) := 
basis(List,ZZ,Ring) := 
basis(ZZ,List,Ring) := 
basis(List,List,Ring) := 
basis(InfiniteNumber,ZZ,Ring) := 
basis(ZZ,InfiniteNumber,Ring) := 
basis(InfiniteNumber,InfiniteNumber,Ring) := 
basis(ZZ,ZZ,Ring) := opts -> (lo,hi,R) -> basis(lo,hi,module R,opts)

basis(ZZ,Ring) := 
basis(List,Ring) := opts -> (deg,R) -> basis(deg, module R, opts)

basis Module := opts -> (M) -> basis(-infinity,infinity,M,opts)
basis Ring := opts -> R -> basis(R^1,opts)
basis Ideal := opts -> I -> basis(module I,opts)
-----------------------------------------------------------------------------
truncate = method()
truncate(List,Module) := Module => (deg,M) -> (
     if M.?generators then (
	  b := M.generators * cover basis(deg,deg,cokernel presentation M,Truncate=>true);
	  if M.?relations then subquotient(b, M.relations)
	  else image b)
     else image basis(deg,deg,M,Truncate=>true))
truncate(List,Ideal) := Ideal => (deg,I) -> ideal truncate(deg,module I)
truncate(ZZ,Module) := Module => (deg,M) -> truncate({deg},M)
truncate(ZZ,Ideal) := Ideal => (deg,I) -> truncate({deg},I)
-----------------------------------------------------------------------------
isSubset(Module,Module) := (M,N) -> (
     -- here is where we could use gb of a subquotient!
     ambient M == ambient N and
     if M.?relations and N.?relations then (
	  image M.relations == image N.relations
	  and
	  issub(M.relations | generators M, N.relations | generators N))
     else if not M.?relations and not N.?relations then (
	  issub(generators M, generators N))
     else (
	  -- see the code for subquotient: if present, M.relations is nonzero; same for N
	  -- so one of the modules has nonzero relations and the other doesn't
	  false
	  )
     )
isSubset(Ideal,Ideal) := (I,J) -> isSubset(module I, module J)
isSubset(Module,Ideal) := (M,J) -> isSubset(M, module J)
isSubset(Ideal,Module) := (I,N) -> isSubset(module I, N)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
