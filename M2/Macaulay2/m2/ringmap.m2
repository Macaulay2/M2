--		Copyright 1995-2002 by Daniel R. Grayson

-- TODO: needs "newring.m2" for flattenRing
needs "galois.m2"
needs "matrix1.m2"
needs "modules.m2"
needs "modules2.m2"
needs "mutablemat.m2"

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- should do something about the degree map here
degmap0 := n -> ( d := toList ( n : 0 ); e -> d )

-----------------------------------------------------------------------------
-- RingMap type declarations and basic methods
-----------------------------------------------------------------------------

RingMap = new Type of HashTable
RingMap.synonym = "ring map"

matrix RingMap := opts -> f -> f.matrix
source RingMap := f -> f.source
target RingMap := f -> f.target
raw RingMap := f -> f.RawRingMap

ZZ == RingMap := (n, f) -> f == n
RingMap == ZZ := (f, n) -> (
    if n == 1 then (source f === target f and f === id_(source f)) else
    error "encountered integer other than 1 in comparison with a ring map")

-- printing helpers
describe   RingMap := f -> Describe expression f
expression RingMap := f -> (expression map) (expression (target f, source f, first entries matrix f))

toExternalString RingMap := toString @@ describe
toString RingMap := toString @@ expression
net      RingMap :=      net @@ expression
texMath  RingMap :=  texMath @@ expression

RingMap#AfterPrint =
RingMap#AfterNoPrint = f -> (
    -- class f, " ", target f, " <--- ", source f)
    class f, " ", new MapExpression from {target f, source f})

-----------------------------------------------------------------------------
-- RingMap constructors
-----------------------------------------------------------------------------

Ring#id = R -> map(R, R, vars R)

map(RingFamily, Thing, Thing) := RingMap => opts -> (R, S, m) -> map(default R, S, m, opts)
map(Thing, RingFamily, Thing) := RingMap => opts -> (R, S, m) -> map(R, default S, m, opts)

map(Ring, Ring)          := RingMap => opts -> (R, S   ) -> map(R, S, matrix(R, {{}}), opts)
map(Ring, Ring, RingMap) := RingMap => opts -> (R, S, f) -> map(R, S, matrix f,        opts)
map(Ring, Ring, List)    := RingMap => opts -> (R, S, m) -> (
    if m#?0 and instance(m#0, Option) then sub2(R, S, m) -- TODO: deprecate this?
    else map(R, S, matrix(R, {m}), opts))

map(Ring,       Matrix)  := RingMap => opts -> (   S, m) -> map(ring m, S, m,   opts)
map(Ring, Ring, Matrix)  := RingMap => opts -> (R, S, m) -> (
     if not isFreeModule target m or not isFreeModule source m
     then error "expected a homomorphism between free modules";
     if ring m === (try coefficientRing R) and ring m === (try coefficientRing S)
     then (
	  if numgens R != rank target m
	  then error ("expected a matrix with ", toString numgens R, " rows");
	  m = vars R * (m ** R);   -- handle a change of coordinates
	  )
     else (
	  try m = promote(m,R) else error "map: expected a matrix over the target ring, or promotable to it";
	  );
     if rank target m != 1
     then error "expected a matrix with 1 row";
     degmap := (
	  if opts.DegreeMap =!= null then (
	       if degreeLength R =!= # (opts.DegreeMap apply(degreeLength S, i -> 0))
	       then error ("expected DegreeMap function to transform a degree of length ", 
		    toString degreeLength S,
		    " into a degree of length ", toString degreeLength R);
	       opts.DegreeMap
	       )
	  else if (pr:=lookup(promote,List,S,R)) =!= null then (d -> first pr({d},S,R))
	  else if degreeLength R === degreeLength S then identity
	  else if degreeLength S === 0 or degreeLength R === 0 then degmap0 degreeLength R
	  else (
	       -- error "map(Ring,Ring,Matrix): nonzero degree monoids differ, promotion not available, DegreeMap needed";
	       degmap0 degreeLength R));
     dR := ZZ^(degreeLength R);
     dS := ZZ^(degreeLength S);
     degmapmatrix := map(dR,dS,transpose apply(entries id_dS,degmap));
     deglift := (
	  if opts.DegreeLift =!= null then opts.DegreeLift
	  else if degmap === identity then identity
	  else if degreeLength S === 0 and degreeLength R === 0 then identity
	  else (d -> (
		    (q,r) := quotientRemainder(transpose matrix {d}, degmapmatrix);
		    if r != 0 then error "degreeLift: degree not liftable";
		    flatten entries q)));
     mdegs := {};
     n := 0;
     A := S;
     record := new MutableHashTable; -- we should look for variables with the same symbol
     justonce := s -> (
	  if record#?s then error "map: multiple variables would map to the same variable, by name";
	  record#s = true;
	  R.indexSymbols#s);
     mE := m;
     while true do (
	  mdegs = join(mdegs, promote(degrees A,A,S) / degmap);
	  r := numgens source m;
	  if r > n then (
	       if instance(A,GaloisField) and A.rawGaloisField then (
		    -- the engine wants to know where the primitive element will go
		    p := map(R,ambient A,m_(toList(n .. r-1)));
		    m' := new MutableMatrix from m;
		    m'_(0,n) = p A.PrimitiveElement;
		    m = matrix m';
		    ))
	  else if r < n then error ("encountered values for ", toString r, " variables, but expected ", toString n)
	  else if r == n then (
	       if numgens A > 0 then (
		    if A === R or isPromotable(A, R) then (
			 -- we can promote
			 mE = mE | promote(vars A, R);
			 if instance(A,GaloisField) and A.rawGaloisField then (
		    	      -- the engine wants to know where the primitive element will go
		    	      m = m | promote(A.PrimitiveElement, R)
			      )
			 else m = m | promote(vars A, R);
			 )
		    else (
			 mm := matrix(R, {apply(A.generatorSymbols, s -> if R.?indexSymbols and R.indexSymbols#?s then justonce s else 0_R)});
			 mE = mE | mm;
			 if instance(A,GaloisField) and A.rawGaloisField then (
			      -- the engine wants to know where the primitive element will go
			      m = m | matrix {{(map(R,ambient A,mm)) A.PrimitiveElement}}
			      )
			 else m = m | mm;
			 )));
	  n = n + numgens A;
	  try A = coefficientRing A else break
	  );
     if n != numgens source m then error ("encountered values for ", toString numgens source m," variables");
     zdeg  := toList ( degreeLength R : 0 );
     mE = map(R^{zdeg}, R^-mdegs, mE, Degree => zdeg);
     new RingMap from {
	  symbol target => R,
	  symbol source => S,
	  symbol matrix => mE,
	  symbol RawRingMap => rawRingMap m.RawMatrix,
	  symbol Matrix => degmapmatrix,
	  symbol cache => new CacheTable from {
	       symbol DegreeMap => degmap,
	       symbol DegreeLift => deglift
	       }
	  }
     )

-----------------------------------------------------------------------------
-- evaluation of ring maps
-----------------------------------------------------------------------------

RingMap RingElement := RingElement => fff := (p,m) -> (
     R := source p;
     S := target p;
     if R =!= ring m then (
	  m = try promote(m,R) else error "ring element not in source of ring map, and not promotable to it";
	  );
     promote(rawRingMapEval(raw p, raw m),S))

RingMap Number := (p,m) -> fff(p, promote(m,source p))

RingMap Matrix := Matrix => (p,m) -> (
     R := source p;
     S := target p;
     if R =!= ring m then (
	  m = try promote(m,R) else error "ring of matrix not source of ring map, and not promotable to it";
	  );
     F := p target m;
     E := p source m;
     map(F,E,map(S,rawRingMapEval(raw p, raw cover F, raw m)), Degree => p.cache.DegreeMap degree m))

RingMap MutableMatrix := MutableMatrix => (p,m) -> (
     R := source p;
     S := target p;
     if R =!= ring m 
     then error "expected source of ring map to be the same as ring of matrix";
     map(S,rawRingMapEval(raw p, raw m)))

RingMap Vector := Vector => (p,m) -> (
     f := p new Matrix from m;
     new target f from f)

RingMap Ideal  := Ideal  => (f, I) -> ideal f module I
RingMap Module := Module => (f, M) -> (
    (S, R) := (target f, source f);
    if R =!= ring M then error "expected module over source ring";
    if M.?relations then error "ring map applied to module with relations: use '**' or 'tensor' instead";
    if M.?generators then image f M.generators
    else ( -- M is a free module
	d := degrees M;
	e := f.cache.DegreeMap \ d;
	-- use the same module if we can
	if R === S and d === e then M else S^-e)
    )

-- misc
tensor(RingMap, Module) := Module => {} >> opts -> (f, M) -> (
    if source f =!= ring M then error "expected module over source ring";
    subquotient(f ambient M,
	if M.?generators then f M.generators,
	if M.?relations  then f M.relations))
RingMap ** Module := Module => (f, M) -> tensor(f, M)

tensor(RingMap, Matrix) := Matrix => {} >> opts -> (f, m) -> (
    if source f =!= ring m then error "expected matrix over source ring";
    map(f ** target m, f ** source m, f cover m))
RingMap ** Matrix := Matrix => (f, m) -> tensor(f, m)

VisibleList / RingMap := VisibleList => (v,f) -> apply(v,x -> f x)
RingMap \ VisibleList := VisibleList => (f,v) -> apply(v,x -> f x)

-----------------------------------------------------------------------------
-- kernel
-----------------------------------------------------------------------------

kernel RingMap := Ideal => opts -> (cacheValue (symbol kernel => opts)) (f -> (
    (F, R) := (target f, source f);
    if 0_F == 1_F then return ideal 1_R;
    -- the actual computation occurs here
    I := runHooks((kernel, RingMap), (opts, f));
    if I =!= null then I else error "kernel: no method implemented for this type of ring map"))

-- This is a map from method keys to strategy hash tables
algorithms := new MutableHashTable from {}
algorithms#(kernel, RingMap) = new MutableHashTable from {
    FractionField => (opts, f) -> (
	(F, R) := (target f, source f);
	C := last F.baseRings;
	if not instance(F, FractionField)
	or not coefficientRing R === (k := coefficientRing C)
	or not(isPolynomialRing C or isQuotientOf(PolynomialRing, C))
	or not(isPolynomialRing R or isQuotientOf(PolynomialRing, R))
	then return null;
	       prs := presentation C;
	       B := ring prs;
	       images := apply(generators R, x -> (
			 w := f x;
			 new Divide from {numerator w, denominator w} ));
	       -- now make a common denominator for all images
	       images = new MutableList from images;
	       i := 1;
	       while i < #images do (
		    z := syz(
			 matrix{{denominator images#0,denominator images#i}},
			 SyzygyLimit => 1 );
		    a := -z_(0,0);
		    b := z_(1,0);
		    j := 0;
		    while j < i do (
			 images#j = apply(images#j, s -> s*a);
			 j += 1;
			 );
		    images#i = apply(images#i, s -> s*b);
		    i += 1;
		    );
	       images = toList images;
	       commonDenominator := images#0#1;
	       d := symbol d;
	       h := symbol h;
	       x := symbol x;
	       y := symbol y;
	(n1, n2) := (numgens F, numgens R);
	       S := k[x_1 .. x_n1, d, y_1 .. y_n2, h,
		    MonomialOrder => Eliminate (n1 + 1),
		    Degrees => join(degrees C, {{1}}, degrees R, {{1}})];
	       in1 := map(S,C,matrix {take (generators S, n1)});
	       in2 := map(S,B,matrix {take (generators S, n1)});
	       in3 := map(S,R,matrix {take (generators S, {n1 + 1, n1 + n2})});
	       back := map(R,S,map(R^1,R^(n1 + 1),0) | vars R | 1 );
	       ideal back selectInSubring( 1, 
		    generators gb(
			 in2 prs |
			 homogenize (
			      in3 vars source in3 - d * in1 matrix {apply(images, first)}
			      | d * in1 commonDenominator - 1,
			      h),
			 Strategy => LongPolynomial, opts))
	),

    "AffineRing" => (opts, f) -> (
	(F, R) := (target f, source f);
	if not isAffineRing R
	or not isAffineRing F
	or not instance(ambient R, PolynomialRing)
	or not instance(ambient F, PolynomialRing)
	or not isField coefficientRing R
	or not coefficientRing R === coefficientRing F
	then return null;
	       graph := generators graphIdeal f;
	       assert( not isHomogeneous f or isHomogeneous graph );
	       SS := ring graph;
	       chh := checkHilbertHint graph;
	       if chh then (
		   -- compare with pushNonLinear
		   hf := poincare module target f;
		   T := degreesRing SS;
		   hf = hf * product(degrees source graph, d -> 1 - T_d);
		   -- cache poincare
		   poincare cokernel graph = hf;
		   );
	n1 := numgens F;
	       mapback := map(R, ring graph, map(R^1, R^n1, 0) | vars R);
	       G := gb(graph,opts);
	       assert (not chh or G#?"rawGBSetHilbertFunction log"); -- ensure the Hilbert function hint was actually used in gb.m2
	       ideal mapback selectInSubring(1,generators G)
	),

    Default => (opts, f) -> (
	(F, R) := (target f, source f);
	       numsame := 0;
	       while (
		    R.baseRings#?numsame and
		    F.baseRings#?numsame and 
		    R.baseRings#numsame === F.baseRings#numsame
		    )
	       do numsame = numsame + 1;
	       while not (
		    isField F.baseRings#(numsame-1)
		    or
		    F.baseRings#(numsame-1).?isBasic
		    )
	       do numsame = numsame - 1;
	       k := F.baseRings#(numsame-1);
	       (R',p) := flattenRing(R, CoefficientRing => k);
	       (F',r) := flattenRing(F, CoefficientRing => k);
	if R' === R and F' === F then return null;
	p^-1 kernel (r * f * p^-1)
	),
    }

-- Installing hooks for kernel RingMap
scan({Default, "AffineRing", FractionField}, strategy ->
    addHook(key := (kernel, RingMap), algorithms#key#strategy, Strategy => strategy))

-----------------------------------------------------------------------------

preimage = method()
preimage(RingMap, Ideal) := Ideal => (f, J) -> (
    R := ring J;
    kernel( map(R/J, R) * f ))

preimage(Matrix, Module) := (f, M) -> (
    T := target f;
    g := inducedMap(T/M, T);
    kernel(g * f))

coimage RingMap := QuotientRing => f -> f.source / kernel f

isInjective RingMap := f -> kernel f == 0

-----------------------------------------------------------------------------
-- composition of ring maps
-----------------------------------------------------------------------------

RingMap * RingMap := RingMap => (g,f) -> (
     if source g =!= target f then error "ring maps not composable";
     m := g matrix f;
     new RingMap from {
	  symbol source => source f,
	  symbol target => target g,
	  symbol matrix => m,
	  symbol RawRingMap => rawRingMap raw m,
	  symbol Matrix => g.Matrix * f.Matrix,
	  symbol cache => new CacheTable from {
	       symbol DegreeMap => g.cache.DegreeMap @@ f.cache.DegreeMap,
	       symbol DegreeLift => f.cache.DegreeLift @@ g.cache.DegreeLift
	       }
	  }
     )

RingMap#1 = f -> (
    if source f =!= target f then error "expected source and target to agree"
    else id_(target f))
RingMap ^ ZZ := RingMap => BinaryPowerMethod

-----------------------------------------------------------------------------

-- TODO: should also check consistency with the degree groups
isHomogeneous RingMap := (f) -> (
     R := f.source;
     S := f.target;
     isHomogeneous R and isHomogeneous S and
     all(generators(R, CoefficientRing=>ZZ), r -> r == 0 or (
	       s := f r;
	       s == 0 or isHomogeneous s and degree s === f.cache.DegreeMap degree r
	       )))

-----------------------------------------------------------------------------
-- substitute
-----------------------------------------------------------------------------

substitute(Power,Thing) := (v,s) -> Power{substitute(v#0,s),v#1}
substitute(Divide,Thing) := (v,s) -> Divide{substitute(v#0,s),substitute(v#1,s)}
substitute(Sum,Thing) := substitute(Product,Thing) := (v,s) -> apply(v,t -> substitute(t,s))

substitute(RingElement,Matrix) := RingElement => (r,f) -> (map(ring f,ring r,f)) r
substitute(Vector,Matrix) := Vector => (v,f) -> (map(ring f,ring v,f)) v
substitute(Matrix,Matrix) := Matrix => (m,f) -> (map(ring f,ring m,f)) m
substitute(Module,Matrix) := Module => (M,f) -> (map(ring f,ring M,f)) M
substitute(Ideal,Matrix) := Ideal => (I,f) -> (map(ring f,ring I,f)) I

substitute(Matrix,Ring) := Matrix => (m,S) -> (map(S,ring m)) m
substitute(Module,Ring) := Module => (M,S) -> (map(S,ring M)) M
substitute(Ideal,Ring) := Ideal => (I,S) -> (map(S,ring I)) I
substitute(Vector,Ring) := Vector => (v,S) -> (map(S,ring v)) v
substitute(Number,Ring) := 
substitute(RingElement,Ring) := RingElement => (r,S) -> (map(S,ring r)) r

substitute(Matrix,RingFamily) := Matrix => (m,S) -> substitute(m, default S)
substitute(Module,RingFamily) := Module => (M,S) -> substitute(M, default S)
substitute(Ideal,RingFamily) := Ideal => (I,S) -> substitute(I, default S)
substitute(Vector,RingFamily) := Vector => (v,S) -> substitute(v, default S)
substitute(Number,RingFamily) := 
substitute(RingElement,RingFamily) := RingElement => (r,S) -> substitute(r, default S)

substitute(Matrix,ZZ) := Matrix => (m,i) -> (
     R := ring m;
     if i === 0 then (
     	  if isPolynomialRing R 
	  or isQuotientRing R and isPolynomialRing ultimate(ambient,R)
	  then substitute(m,map(R^1, R^(numgens R), 0))
     	  else m
	  )
     else error "expected integer to be zero"
     )

sub2 = (S,R,v) -> (				   -- S is the target ring or might be null, meaning target ring not known yet
     commonzero := if S === null then 0 else 0_S;  -- the 0 element of the target ring
     local dummy;
     A := R;
    -- a list, containing variables of R and its base rings
    (g, gs) := flatten \ toSequence transpose while A =!= ZZ list {generators A, if A.?generatorSymbols then A.generatorSymbols else {}} do try (
        A = if instance(A, FractionField) then frac coefficientRing A else coefficientRing A) else break;
    -- a hash table, consisting of pairs (generator symbol) => (indices)
    h := new MutableHashTable;
    -- a list, eventually containing the targets of each generator
    m := new MutableList from apply(pairs gs, (i, x) -> ( h#x = if h#?x then append(h#x, i) else 1:i; symbol dummy ));
     for opt in v do (
	  if class opt =!= Option or #opt =!= 2 then error "expected a list of options";
	  x := baseName opt#0;
	  y := opt#1;
	  if instance(y, Constant) then y = numeric y;
	  if not instance(y,RingElement) and not instance(y,Number) then error "expected substitution values to be ring elements or numbers";
	  if S === null
	  then try commonzero = commonzero + 0_(ring y) else error "expected substitution values to be in compatible rings"
	  else try y = promote(y,S) else error "expected to be able to promote value to target ring";
	  try x_R else error( "expected ", toString x, " to be a generator of ", toString R );
	  for i in h#x do (
	       if m#i =!= symbol dummy and m#i =!= y then error "multiple destinations specified for a generator";
	       m#i = y;
	       ));
     if S === null then (
	  if any(m,x -> x === symbol dummy) then (
	       for i from 0 to #m-1 do if m#i === symbol dummy then (
		    try commonzero = commonzero + 0_(ring g#i) else error "expected substitution values and omitted generators to be in compatible rings";
		    m#i = commonzero + g#i;
		    );
	       )
	  else (
	       -- the target ring should be at least as big as the bottom coefficient ring:
	       try commonzero = commonzero + 0_A
	       else error "expected substitution values and omitted generators to be in compatible rings";
	       );
	  S = ring commonzero;
	  if instance(R,FractionField) then S=frac S;
	  for i from 0 to #m-1 do m#i = promote(m#i, S);
	  )
     else if R === S and S === ring commonzero then (
     	  -- if source==target, then the default is to leave generators alone
	  for i from 0 to #m-1 do if m#i === symbol dummy then m#i = g#i;
	  )
     else (
	  if any(m,x -> x === symbol dummy) then error "destinations not specified for every generator";
	  );
     f := if S === null then matrix{toList m} else matrix(S,{toList m});
     map(ring f,R,f))

substitute(Matrix,List) := Matrix => (f,v) -> (sub2(,ring f,v)) f
substitute(Module,List) := Module => (M,v) -> (sub2(,ring M,v)) M
substitute(Ideal,List) := Ideal => (I,v) -> (sub2(,ring I,v)) I
substitute(Vector,List) := Vector => (f,v) -> (sub2(,ring f,v)) f
substitute(RingElement,List) := RingElement => (f,v) -> (sub2(,ring f,v)) f

substitute(Matrix,Option) := (f,v) -> (sub2(,ring f,{v})) f
substitute(Module,Option) := (M,v) -> (sub2(,ring M,{v})) M
substitute(Ideal,Option) := (I,v) -> (sub2(,ring I,{v})) I
substitute(Vector,Option) := (f,v) -> (sub2(,ring f,{v})) f
substitute(RingElement,Option) := (f,v) -> (sub2(,ring f,{v})) f

-----------------------------------------------------------------------------
-- Syntactic sugar for polynomial evaluation
-----------------------------------------------------------------------------

RingElement Array := -- TODO: eventually deprecate this
RingElement Sequence := (f, v) -> (
    R := ring f;
    n := if R.?numallvars then R.numallvars else numgens R;
    if #v > n
    then error("encountered values for ", #v, " variables, but expected at most ", n)
    else substitute(f, apply(#v, i -> R_i => v#i)))
-- this will make f(a) work as expected
RingElement Number := RingElement RingElement := (f, n) -> f(1:n)

-----------------------------------------------------------------------------
-- inverse
-----------------------------------------------------------------------------

inverse RingMap := RingMap.InverseMethod = (cacheValue symbol inverse) ( f -> (
	  R := target f;
	  S := source f;
	  I := graphIdeal f;
	  G := ring I;
	  mapto := map(G,R,take(generators G,numgens R));
	  mapback := map(S,G,{ numgens R : 0, toSequence generators S }); -- not a homomorphism
	  m := selectInSubring(1, mapto vars R % I);
	  if numColumns m === numColumns vars R 
	  then map(S,R,mapback m)
	  else error "ring map not invertible"))

-----------------------------------------------------------------------------
-- module maps over ring maps:
-----------------------------------------------------------------------------

map(Module,Module,Nothing,RawMatrix) := opts -> (M,N,p,f) -> map(M,N,f)
map(Module,Module,RingMap,RawMatrix) := opts -> (M,N,p,f) -> (
     (R,S) := (ring M,ring N);
     (M',N') := (cover M,p cover N);
     if R =!= target p or S =!= source p then error "expected rings to match";
     if target f =!= raw M' then error "expected target of matrix to match (cover of) target module";
     ---- we relax this check so "basis" can return something for basis(QQ[x]/x^2,SourceRing=>QQ):
     -- if source f =!= raw N' then error "expected source of matrix to match (cover of) source module";
     deg := if opts.Degree === null then rawMultiDegree f else degreeCheck(opts.Degree,R);
     new Matrix from {
	  symbol RingMap => p,
	  symbol target => M,
	  symbol source => N,
	  symbol RawMatrix => reduce(M,rawMatrixRemake2(raw M',raw N',deg,f,0)),
	  symbol cache => new CacheTable
	  })
map(Module,Nothing,RingMap,RawMatrix) := Matrix => o -> (M,N,p,f) -> (
    d := degreeLength ring M;
     degs := pack(d,degrees source f);
     deg := o.Degree;
     if deg =!= null then degs = apply(degs, dg -> dg - deg);
     srcdegs := apply(degs,p.cache.DegreeLift);
     map(M,(source p)^-srcdegs,p,f,o))

map(Module,Nothing,RingMap,Matrix) := 
map(Module,Module,RingMap,Matrix) := Matrix => o -> (M,N,p,f) -> map(M,N,p,raw f,o)
map(Module,Module,RingMap,List) := Matrix => o -> (M,N,p,f) -> map(M,N,p,map(M,ring M ** N,f),o)
map(Module,Nothing,RingMap,List) := Matrix => o -> (M,N,p,f) -> map(M,N,p,map(M,,f),o)
map(Module,RingMap) := Matrix => o -> (M,p) -> map(M,,p,map(M,cover M,1),o)

--
setupPromote (RingMap,Ring,Ring,Function) := lookup(setupPromote,Function,Ring,Ring,Function)
setupPromote (RingMap,Ring,Ring) := (f,R,S) -> setupPromote(f,R,S,f.cache.DegreeMap)
-- note that promote(Module,R,S) := (M,R,S) -> f ** M would make more sense, but promote only works with free modules anyway
setupPromote RingMap := f -> setupPromote(f,source f,target f)
setupPromote (Ring,Ring) := (R,S) -> setupPromote map(S,R)

setupLift (RingMap,Ring,Ring) := (f,R,S) -> -- f is a partial inverse to the promote map
    setupLift( a -> ( b := f a; if promote(b,R) == a then b else error "cannot lift" ), R,S,f.cache.DegreeMap);

setupLift RingMap := f -> setupLift(f,source f,target f)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
