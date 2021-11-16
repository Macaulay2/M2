--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

needs "matrix1.m2"  -- for Ideal
needs "matrix2.m2"  -- for modulo
needs "quotring.m2" -- for QuotientRing

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
	  ambient M,
	  if not M.?generators or not N.?generators then null else M.generators | N.generators,
	  if M.?relations then M.relations else null
	  )
     )

-- TODO: remove this when all tensor methods are installed on 'tensor' instead of **
tensor(Thing, Thing) := true >> opts -> (M, N) -> M ** N
undocumented' (tensor, Thing, Thing)

Module ** Module := Module => (M, N) -> tensor(M, N)
tensor(Module, Module) := Module => {} >> opts -> (M, N) -> (
     (oM,oN) := (M,N);
     Y := youngest(M.cache.cache,N.cache.cache);
     if Y#?(symbol **,M,N) then return Y#(symbol **,M,N);
     if M.?generators and not isFreeModule N
     or N.?generators and not isFreeModule M then (
	  if M.?generators then M = cokernel presentation M;
	  if N.?generators then N = cokernel presentation N;
	  );
     R := ring M;
     if R =!= ring N then error "expected modules over the same ring";
     T := if isFreeModule M then (
	       if isFreeModule N then (
		    new Module from (R, raw M ** raw N)
		    )
	       else subquotient(
		    if N.?generators then M ** N.generators,
		    if N.?relations then M ** N.relations))
	  else (
	       if isFreeModule N then (
		    subquotient(
			 if M.?generators then M.generators ** N,
			 if M.?relations then M.relations ** N))
	       else cokernel map(R, rawModuleTensor( raw M.relations, raw N.relations )));
     Y#(symbol **,oM,oN) = T;
     -- we do not set T.cache.components, as "components" is for sums, not tensor products
     T.cache.formation = FunctionApplication (tensor, (M,N));
     T)

Matrix ** Module := Matrix => (f,M) -> if isFreeModule M and M == (ring M)^1 and ring M === ring f then f else  f ** id_M
Module ** Matrix := Matrix => (M,f) -> if isFreeModule M and M == (ring M)^1 and ring M === ring f then f else id_M ** f

Option ** Option := (x,y) -> (
     (a,b) := (x#0,y#0);			 -- the labels
     (M,N) := (x#1,y#1);			 -- the objects (modules, etc.)
     T := M ** N;
     labels := T.cache.indices = {a,b};
     ic := T.cache.indexComponents = new HashTable from {a => 0, b => 1};
     -- now, in case T is a map (i.e., has a source and target), then label the source and target objects of the tensor product
     if T.?source and T.?target then (
	  T.source.cache.indexComponents = T.target.cache.indexComponents = ic; 
	  T.source.cache.indices = T.target.cache.indices = labels;
	  );
     T)

-----------------------------------------------------------------------------
-- base change
-----------------------------------------------------------------------------
Module ** Ring := Module => (M,R) -> R ** M		    -- grandfathered, even though our modules are left modules

Ring ** Module := Module => (R,M) -> (
     A := ring M;
     if A === R then return M;
     pr := try (promote(1_A, R); true) else false;
     if pr then (
     	  if M.?generators then cokernel promote(presentation M, R)
     	  else if M.?relations then cokernel promote(M.relations,R)
     	  else R^(promote(- degrees M,A,R)))
     else map(R,A) ** M)

Matrix ** Ring := Matrix => (f,R) -> R ** f		    -- grandfathered, even though our modules are left modules

Ring ** Matrix := Matrix => (R,f) -> (
     B := ring source f;
     A := ring target f;
     if B === R and A === R then f
     else map( target f ** R, source f ** R, promote(cover f, R), Degree => first promote({degree f}, A, R) )
     )

Ideal ** Ring := Ideal => (I, R) -> R ** I

Ring ** Ideal := Ideal => (R, I) -> ideal(generators I ** R)

-----------------------------------------------------------------------------       
poincare Module := (cacheValue symbol poincare) (
     M -> (
	  -- see the comment in the documentation for (degree,Ideal) about what this means when M is not homogeneous
	  new degreesRing M from rawHilbert raw leadTerm gb -* presentation cokernel ?? *- presentation M))

recipN = (n,wts,f) -> (
     -- n is a positive integer
     -- wts is a weight vector
     -- f is a polynomial of the form 1 plus terms of positive weight, which we verify
     -- we compute the terms of the expansion of 1/f of weight less than n
     if n <= 0 then error "expected a positive integer";
     if part(,0,wts,f) != 1 then error "expected a polynomial of the form 1 plus terms of positive weight";
     g := 1_(ring f);  -- g always has the form 1 plus terms weight 1,2,...,m-1
     m := 1;			   -- 1-f*g always has terms of wt m and higher
     tr := h -> part(,m-1,wts,h);
     while m < n do (
	  m = 2*m;
	  g = g + tr(g * (1 - tr(g * tr f)));
	  );
     if m === n then g else part(,n-1,wts,g))

heft = method()
heft Ring := R -> ( o := options R; if o =!= null and o.?Heft then o.Heft )
heft PolynomialRing := R -> (options R.FlatMonoid).Heft
heft QuotientRing := R -> heft ambient R
heft Module := M -> heft ring M

exactKey := "exact hilbertSeries"
reducedKey := "reduced exact hilbertSeries"
approxKey := "approximate hilbertSeries"

reduceHilbert = method()
reduceHilbert Divide := ser -> (
     num := numerator ser;				    -- an element of the degrees ring
     if num == 0 then return Divide {num, 1_(ring num)};
     den := denominator ser;				    -- a Product of Powers
     newden := Product nonnull apply(toList den, pwr -> (
	       fac := pwr#0;				    -- 1-T_i
	       ex  := pwr#1;	 			    -- exponent
	       while ex > 0
	       and num % fac == 0			    -- this works because of Mike's magic in the engine
	       do (
		    num = num // fac;
		    ex = ex - 1;
		    );
	       if ex > 0 then Power {fac,ex}));
     Divide {num, newden})

protect symbol Order
assert( class infinity === InfiniteNumber )
hilbertSeries = method(Options => {
     	  Order => infinity,
	  Reduce => false
	  }
     )

hilbertSeries QuotientRing := 
hilbertSeries PolynomialRing := opts -> R -> hilbertSeries(R^1, opts)
hilbertSeries Ideal := opts -> (I) -> hilbertSeries(comodule I,opts)
hilbertSeries Module := opts -> (M) -> (
     -- some examples compute degrees of inhomogeneous modules, so we can't refuse to compute when the module is not homogeneous.
     -- is it guaranteed to work in some sense?
     -- if not isHomogeneous M then error "expected a homogeneous module";
     A := ring M;
     if (options A).Heft === null then error "hilbertSeries: ring has no heft vector";
     ord := opts.Order;
     reduced := opts.Reduce;
     if ord === infinity then (
	  if reduced then (
	       if M.cache#?reducedKey then return M.cache#reducedKey;
	       if M.cache#?exactKey then return (M.cache#reducedKey = reduceHilbert M.cache#exactKey);
	       )
	  else (
	       if M.cache#?exactKey then return M.cache#exactKey;
	       )
	  )
     else if class ord === ZZ then (
	  if M.cache#?approxKey then (
	       (ord2,ser) := M.cache#approxKey;
	       if ord == ord2 then return ser
	       else if ord < ord2 then return part(,ord-1,heft M,ser)))
     else error "hilbertSeries: expected infinity or an integer as value of Order option";
     T := degreesRing A;
     if ord === infinity then (
     	  num := poincare M; -- 'poincare' treats monomial ideals correctly (as the corresponding quotient module)
     	  denom := tally degrees A.FlatMonoid;
	  r := Divide{
	       num,
	       Product apply(sort apply(pairs denom, (i,e) -> {1 - T_i,e}), t -> Power t)};
	  M.cache#exactKey = r;
	  if reduced then M.cache#reducedKey = reduceHilbert r else r)
     else (
	  h := hilbertSeries(M,Reduce => true);
	  s := (
	       num = numerator h;
	       if num == 0 then 0_T else (
		    wts := (options ring M).Heft;
		    (lo,hi) := weightRange(wts,num);
		    if ord <= lo then 0_T else (
		    	 num = part(,ord-1,wts,num);
			 scan(denominator h, denom -> (
				   rec := recipN(ord-lo,wts,denom#0);
				   scan(denom#1, i -> num = part(,ord-1,wts,num * rec))));
			 num)));
	  M.cache#approxKey = (ord,s);
	  s))

hilbertFunction(ZZ,Module) := hilbertFunction(ZZ,Ring) := hilbertFunction(ZZ,Ideal) := (d,M) -> hilbertFunction({d},M)
hilbertFunction(List,Ring) := (d,R) -> hilbertFunction(d,R^1)
hilbertFunction(List,Ideal) := hilbertFunction(List,Module) := (d,M) -> (
     if not all(d,i->class i === ZZ) then error "expected degree to be an integer or list of integers";
     if degreeLength M =!= #d then error "degree length mismatch";
     h := (options ring M).Heft;
     if h === null then error "hilbertFunction: ring has no heft vector";
     f := hilbertSeries(M, Order => 1 + sum(h,d,times));
     U := monoid ring f;
     coefficient(U_d,f))

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
ProjectiveHilbertPolynomial + ProjectiveHilbertPolynomial := ProjectiveHilbertPolynomial => (h,k) -> merge(h,k,continueIfZero @@ plus)
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

hilbertSeries ProjectiveHilbertPolynomial := opts -> h -> (
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
texMath ProjectiveHilbertPolynomial := x -> texMath expression x

projectiveHilbertPolynomial = method()
projectiveHilbertPolynomial ZZ := ProjectiveHilbertPolynomial => (n) -> (
     new ProjectiveHilbertPolynomial from { n => 1 }
     )
projectiveHilbertPolynomial(ZZ,ZZ) := ProjectiveHilbertPolynomial => memoize(
     (n,d) -> new ProjectiveHilbertPolynomial from (
     	  if d <= 0 
	  then apply(min(-d+1,n+1), j -> n-j => (-1)^j * binomial(-d,j))
     	  else apply(n+1, j -> n-j => binomial(d-1+j,j))))

hilbertFunctionRing = memoize(() -> QQ(monoid [getSymbol "i"]))
hilbertFunctionQ = method()
hilbertFunctionQ(ZZ) := (n) -> (
     if n === 0 then 1_(hilbertFunctionRing())
     else (
	  i := (hilbertFunctionRing())_0;
	  (1/n) * (n+i) * hilbertFunctionQ(n-1)))
hilbertFunctionQ(ZZ,ZZ) := memoize(
     (n,d) -> (
     	  if d === 0 then hilbertFunctionQ(n)
     	  else (
	       i := (hilbertFunctionRing())_0;
	       substitute(hilbertFunctionQ(n), {i => i+d}))))

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
	 then 0_(hilbertFunctionRing())
	 else sum(p, (d,c) -> (
	      	   if #d === 0 then d = 0 else d = d#0;
	      	   c * hilbertFunctionQ(n,-d)))))

hilbertPolynomial Ring := ProjectiveHilbertPolynomial => options -> (R) -> hilbertPolynomial(R^1, options)

Ideal * Ring := Ideal => (I,S) -> if ring I === S then I else ideal(I.generators ** S)
Ring * Ideal := Ideal => (S,I) -> if ring I === S then I else ideal(I.generators ** S)

-- the key for issub hooks under GlobalHookStore
protect ContainmentHooks
issub := (f, g) -> (
    if (R := ring f) =!= ring g then error "isSubset: expected objects of the same ring";
    if (c := runHooks(ContainmentHooks, (f, g))) =!= null then c
    else error "isSubset: no strategy implemented for this type of ring")

-- TODO: we can do better in the homogeneous case!
addHook(ContainmentHooks, Strategy => Inhomogeneous, (f, g) -> -1 === rawGBContains(raw gb g, raw f))

ZZ == Ideal := (n,I) -> I == n
Ideal == ZZ := (I,n) -> (
     if n === 0
     then I.generators == 0
     else if n === 1
     then issub(matrix {{1_(ring I)}}, generators I)
     else error "attempted to compare ideal to integer not 0 or 1"
     )

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

degree Ring := R -> degree R^1
degree Module := (
     () -> (
     	  -- constants:
	  local ZZ1;
	  local T;
	  local h;
	  local ev;
	  M -> (
	       if ZZ1 === null then (
		    ZZ1 = degreesRing 1;
		    T = ZZ1_0;
		    h = 1 - T;
		    );
	       hft := heft M;
	       if hft === null then error "degree: no heft vector defined";
	       hn := poincare M;
	       n := degreeLength M;
	       if n === 0 then return lift(hn,ZZ);		    -- assert( hd == 1 );
	       to1 := map(ZZ1,ring hn,apply(hft,i->T^i));	    -- this assigns a privileged role to the heft vector, which we need to investigate
	       hn = to1 hn;
	       if hn == 0 then return 0;
	       while hn % h == 0 do hn = hn // h;
	       if ev === null then ev = map(ZZ,ZZ1,{1}); -- ring maps are defined only later
	       ev hn)))()

multidegree Module := M -> (
     A := degreesRing M;
     onem := map(A,A,apply(generators A, t -> 1-t));
     c := codim M;
     if c === infinity then 0_A else part(c,numgens A:1,onem numerator poincare M))
multidegree Ring := R -> multidegree R^1
multidegree Ideal := I -> multidegree cokernel generators I

length Module := ZZ => (cacheValue symbol length) (M -> (
    c := runHooks((length, Module), M);
    if c =!= null then c else error "length: no method implemented for this type of module"))

addHook((length, Module), Strategy => Default, M -> (
     if not isHomogeneous M then notImplemented();
     if dim M > 0 then return infinity;
     degree M))

-----------------------------------------------------------------------------

presentation(Module) := Matrix => M -> (
     if M.cache.?presentation then M.cache.presentation else M.cache.presentation = (
	  if M.?generators then (
	       modulo( M.generators, if M.?relations then M.relations)
	       )
	  else relations M))
-----------------------------------------------------------------------------  

minimalPresentation(Module) := prune(Module) := Module => opts -> (cacheValue (symbol minimalPresentation => opts)) (M -> (
	  if isFreeModule M then (
	       M.cache.pruningMap = id_M;
	       return M);
	  homog := isHomogeneous M;
	  if debugLevel > 0 and homog then pushvar(symbol flagInhomogeneity,true);
	  C := runHooks((minimalPresentation, Module), (opts, M));
	  if debugLevel > 0 and homog then popvar symbol flagInhomogeneity;
	  if C =!= null then return C;
	  error "minimalPresentation: internal error: no method for this type of module"
	  ))

addHook((minimalPresentation, Module), Strategy => Default, (opts, M) -> (
	  -- we try to handle any module here, without any information about the ring
          g := mingens gb presentation M;
	  f := mutableMatrix g;
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
	  if isHomogeneous M then f = map(target g, source g, f);
	  rows := first \ piv;
	  cols := last \ piv;
	  f = submatrix'(f,rows,cols);
	  N := cokernel f;
	  N.cache.pruningMap = map(M,N,submatrix'(id_(cover M),rows));
	  break N))

addHook((minimalPresentation, Module), (opts, M) -> (
     	  R := ring M;
	  if (isAffineRing R and isHomogeneous M) or (R.?SkewCommutative and isAffineRing coefficientRing R and isHomogeneous M) then (
	       f := presentation M;
	       g := complement f;
	       N := cokernel modulo(g, f);
	       N.cache.pruningMap = map(M,N,g);
	       break N)))

addHook((minimalPresentation, Module), (opts, M) -> (
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

addHook((minimalPresentation, Module), (opts, M) -> (
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

minimalPresentation(Matrix) := prune(Matrix) := Matrix => opts -> (m) -> (
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

dual Module := Module => {} >> o -> F -> if F.cache.?dual then F.cache.dual else F.cache.dual = (
     if not isFreeModule F then kernel transpose presentation F
     else new Module from (ring F,rawDual raw F))

Module#id = (M) -> map(M,M,1)

reshape = method()
reshape(Module,Module,Matrix) := Matrix => (F, G, m) -> map(F,G,rawReshape(raw m, raw cover F, raw cover G))

Hom(Ideal, Ideal) := Module => (I,J) -> Hom(module I, module J)
Hom(Ideal, Module) := Module => (I,M) -> Hom(module I, M)
Hom(Module, Ideal) := Module => (M,I) -> Hom(M, module I)

Hom(Module, Ring) := Module => (M,R) -> Hom(M, R^1)
Hom(Ring, Module) := Module => (R,M) -> Hom(R^1, M)
Hom(Ideal, Ring) := Module => (I,R) -> Hom(module I, R^1)
Hom(Ring, Ideal) := Module => (R,I) -> Hom(R^1, module I)

Hom(Module, Module) := Module => (M,N) -> (
     Y := youngest(M.cache.cache,N.cache.cache);
     if Y#?(Hom,M,N) then return Y#(Hom,M,N);
     H := trim kernel (transpose presentation M ** N);
     H.cache.homomorphism = (f) -> map(N,M,adjoint'(f,M,N), Degree => first degrees source f);
     Y#(Hom,M,N) = H; -- a hack: we really want to type "Hom(M,N) = ..."
     H.cache.formation = FunctionApplication { Hom, (M,N) };
     H)

adjoint' = method()
adjoint'(Matrix,Module,Module) := Matrix => (m,G,H) -> (
     -- adjoint':  m : F --> Hom(G,H) ===> F ** G --> H
     -- warning: in versions 1.7.0.1 and older dual G was called for, instead of G, since G was assumed to be free
     F := source m;
     inducedMap(H, F ** G, reshape(super H, F ** G, super m),Verify=>false))

adjoint = method()
adjoint (Matrix,Module,Module) := Matrix => (m,F,G) -> (
     -- adjoint :  m : F ** G --> H ===> F --> Hom(G,H)
     H := target m;
     inducedMap(Hom(G,H), F, reshape(Hom(cover G,ambient H), F, super m),Verify=>false))

homomorphism = method()
homomorphism Matrix := Matrix => (f) -> (
     -- from a map R^1 -> Hom(M,N) produce a map M-->N
     H := target f;
     if not H.cache.?homomorphism then error "expected target of map to be of the form 'Hom(M,N)'";
     if not isFreeModule source f
     or not rank source f == 1 then error "expected source of map to be free of rank 1";
     H.cache.homomorphism f)

homomorphism' = method()
homomorphism' Matrix := Matrix => (f) -> (
     -- from a map M-->N produce a map R^1 -> Hom(M,N)
     R := ring f;
     M := source f;
     adjoint(f,R^1,M)
     )

compose = method()
compose(Module, Module, Module) := Matrix => (M,N,P) -> (
     R := ring M;
     if not ring N === R or not ring P === R then error "expected modules over the same ring";
     if isQuotientModule N then (
	  -- Now cover N === ambient N
	  inducedMap(Hom(M,P),,
	       map(dual cover M ** ambient P, Hom(M,N)**Hom(N,P), 
		    (dual cover M ** reshape(R^1, cover N ** dual cover N, id_(cover N)) ** ambient P)
		    *
		    (generators Hom(M,N) ** generators Hom(N,P))),
	       Verify=>false))
     else (
	  N' := cokernel presentation N;
	  compose(M,N',P) * (Hom(M,map(N',N,1))**Hom(map(N,N',1),P))))

flatten Matrix := Matrix => m -> (
     R := ring m;
     F := target m;
     G := source m;
     if not isFreeModule F or not isFreeModule G
     then error "expected source and target to be free modules";
     if numgens F === 1 
     then m
     else (
	  f := reshape(R^1, G ** dual F ** R^{ - degree m}, m);
	  f = map(target f, source f, f, Degree => toList(degreeLength R:0));
	  f))

flip = method()
flip(Module,Module) := Matrix => (F,G) -> map(ring F,rawFlip(raw F, raw G))

-----------------------------------------------------------------------------
pdim Module := M -> length resolution minimalPresentation M

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
	  oldw := w;
	  w = apply(w, i -> if ic#?i 
		    then ic#i 
		    else error "expected an index of a component of a direct sum"));
     -- if the components of M have 3,4,5 generators, then
     -- we want to construct { (0,1,2), (3,4,5,6), (7,8,9,10,11) } for quick access
     k := 0;
     v := apply(M.cache.components, N -> k .. (k = k + numgens N) - 1);
     newcomps := M.cache.components_w;
     if oldw =!= null then newcomps = apply(oldw,newcomps,(i,M) -> i => M); -- warning: duplicate entries in oldw will lead to inaccessible components
     map(directSum newcomps, M, (cover M)^(splice apply(w, i -> v#i))))

Module _ Array := Matrix => (M,w) -> if M.cache#?(symbol _,w) then M.cache#(symbol _,w) else M.cache#(symbol _,w) = (
     -- we don't splice any more because natural indices include pairs (i,j).
     w = toList w;
     if not M.cache.?components then error "expected a direct sum module";
     if M.cache.?indexComponents then (
	  ic := M.cache.indexComponents;
	  oldw := w;
	  w = apply(w, i -> if ic#?i 
		    then ic#i 
		    else error "expected an index of a component of a direct sum"));
     -- if the components of M have 3,4,5 generators, then
     -- we want to construct { (0,1,2), (3,4,5,6), (7,8,9,10,11) } for quick access
     k := 0;
     v := apply(M.cache.components, N -> k .. (k = k + numgens N) - 1);
     newcomps := M.cache.components_w;
     if oldw =!= null then newcomps = apply(oldw,newcomps,(i,M) -> i => M); -- warning: duplicate entries in oldw will lead to inaccessible components
     map(M, directSum newcomps, (cover M)_(splice apply(w, i -> v#i))))

-----------------------------------------------------------------------------
Module ^ List := Matrix => (M,rows) -> submatrix(id_M,rows,)
-----------------------------------------------------------------------------
Module _ List := Matrix => (M,v) -> (
     N := cover M;
     f := id_N_v;
     map(M, source f, f))

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
