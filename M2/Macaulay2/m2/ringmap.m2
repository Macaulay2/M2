--		Copyright 1995-2002 by Daniel R. Grayson

-- TODO: needs "newring.m2" for flattenRing
needs "galois.m2"
needs "matrix1.m2"
needs "modules.m2"
needs "modules2.m2"
needs "mutablemat.m2"

RingMap = new Type of HashTable

RingMap.synonym = "ring map"
matrix RingMap := opts -> f -> f.matrix
source RingMap := f -> f.source
target RingMap := f -> f.target
raw RingMap := f -> f.RawRingMap

expression RingMap := f -> (expression map) (expression (target f, source f, first entries matrix f))
toString RingMap := f -> toString expression f
net RingMap := f -> net expression f
texMath RingMap := x -> texMath expression x

describe RingMap := f -> Describe expression f
toExternalString RingMap := f -> toString describe f
-- should do something about the degree map here

degmap0 := n -> ( d := toList ( n : 0 ); e -> d )

map(RingFamily,Thing,Thing) := RingMap => opts -> (R,S,m) -> map(default R,S,m,opts)
map(Thing,RingFamily,Thing) := RingMap => opts -> (R,S,m) -> map(R,default S,m,opts)

workable = f -> try (f(); true) else false

map(Ring,Ring,Matrix) := RingMap => opts -> (R,S,m) -> (
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
	  else if workable (() -> promote({},S,R)) then (d -> first promote({d},S,R))
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
		    if A === R or member(A, R.baseRings) then (
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

map(Ring,Matrix) := RingMap => opts -> (S,m) -> map(ring m,S,m)

map(Ring,Ring) := RingMap => opts -> (S,R) -> map(S,R,{},opts)

Ring#id = (R) -> map(R,R,vars R)

RingMap#{Standard,AfterPrint} = RingMap#{Standard,AfterNoPrint} = f -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : " << class f;
     << " " << target f << " <--- " << source f << endl;
     )

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

kernel = method(Options => { SubringLimit => infinity })
kernel RingMap := Ideal => opts -> (cacheValue (symbol kernel => opts)) (
     (f) -> (
	  R := source f;
	  n2 := numgens R;
	  F := target f;
	  n1 := numgens F;
	  if 0_F == 1_F then return ideal(1_R);
	  if class F === FractionField then (
	       C := last F.baseRings;
	       if not (
		    (isPolynomialRing C or isQuotientOf(PolynomialRing,C))
		    and
		    (isPolynomialRing R or isQuotientOf(PolynomialRing,R))
		    and
		    coefficientRing R === coefficientRing C
		    ) then error "kernel: not implemented yet";
	       k := coefficientRing R;
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
			 j = j+1;
			 );
		    images#i = apply(images#i, s -> s*b);
		    i = i+1;
		    );
	       images = toList images;
	       commonDenominator := images#0#1;
	       d := symbol d;
	       h := symbol h;
	       x := symbol x;
	       y := symbol y;
	       S := k[x_1 .. x_n1, d, y_1 .. y_n2, h,
		    MonomialOrder => Eliminate (n1 + 1),
		    Degrees => join(
			 apply(generators C, degree), {{1}}, 
			 apply(generators R, degree), {{1}})];
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
			 Strategy => LongPolynomial, opts)))
	  else if (
	       isAffineRing R and instance(ambient R, PolynomialRing) and isField coefficientRing R
	       and isAffineRing F and instance(ambient F, PolynomialRing)
	       and coefficientRing R === coefficientRing F
	       ) 
	  then (
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
	       mapback := map(R, ring graph, map(R^1, R^n1, 0) | vars R);
	       G := gb(graph,opts);
	       assert (not chh or G#?"rawGBSetHilbertFunction log"); -- ensure the Hilbert function hint was actually used in gb.m2
	       ideal mapback selectInSubring(1,generators G)
	       )
	  else (
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
	       k = F.baseRings#(numsame-1);
	       (R',p) := flattenRing(R, CoefficientRing => k);
	       (F',r) := flattenRing(F, CoefficientRing => k);
	       if R' === R and F' === F then error "kernel RingMap: not implemented yet";
	       p^-1 kernel (r * f * p^-1))))

coimage RingMap := QuotientRing => f -> f.source / kernel f

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

isHomogeneous RingMap := (f) -> (
     R := f.source;
     S := f.target;
     isHomogeneous R and isHomogeneous S and
     all(generators(R, CoefficientRing=>ZZ), r -> r == 0 or (
	       s := f r;
	       s == 0 or isHomogeneous s and degree s === f.cache.DegreeMap degree r
	       )))

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
     g := generators R;
     A := R;
     while try (A = if instance(A,FractionField) then frac coefficientRing A else coefficientRing A; true) else false
     do g = join(g, generators A);
     h := new MutableHashTable;
     for i from 0 to #g-1 do h#(g#i) = if h#?(g#i) then (h#(g#i),i) else 1:i;
     h = new HashTable from apply(pairs h, (x,i) -> (x,deepSplice i));
     m := new MutableList from (#g:symbol dummy);
     for opt in v do (
	  if class opt =!= Option or #opt =!= 2 then error "expected a list of options";
	  x := opt#0;
	  y := opt#1;
	  if instance(y, Constant) then y = numeric y;
	  if not instance(y,RingElement) and not instance(y,Number) then error "expected substitution values to be ring elements or numbers";
	  if S === null
	  then try commonzero = commonzero + 0_(ring y) else error "expected substitution values to be in compatible rings"
	  else try y = promote(y,S) else error "expected to be able to promote value to target ring";
	  if not h#?x and ((try x=promote(x,R))===null or not h#?x) then error( "expected ", toString x, " to be a generator of ", toString R );
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

map(Ring,Ring,List) := RingMap => opts -> (S,R,m) -> (
     if #m>0 and all(m, o -> class o === Option) then sub2(S,R,m)
     else map(S,R,matrix(S,{m}),opts)
     )

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

RingElement Array := (r,v) -> substitute(r,matrix {toList v})

RingMap Ideal := Ideal => (f,I) -> ideal f module I

fixup := (f) -> if isHomogeneous f then f else map(target f,,f)

RingMap Module := Module => (f,M) -> (
     R := source f;
     S := target f;
     if R =!= ring M then error "expected module over source ring";
     if M.?relations then error "ring map applied to module with relations: use '**' or 'tensor' instead";
     if M.?generators then image f M.generators
     else (
	  d := degrees M;
	  e := f.cache.DegreeMap \ d;
	  if R === S and d === e
	  then M -- use the same module if we can
     	  else S^-e
	  )
     )

RingMap ** Module := Module => (f,M) -> (
     R := source f;
     S := target f;
     if R =!= ring M then error "expected module over source ring";
     cokernel f presentation M);

RingMap ** Matrix := Matrix => (f,m) -> (
     if source f =!= ring m then error "expected matrix over source ring";
     map(f ** target m, f ** source m, f cover m))

tensor(RingMap, Module) := Module => {} >> opts -> (f, M) -> f ** M
tensor(RingMap, Matrix) := Matrix => {} >> opts -> (f, m) -> f ** m

isInjective RingMap := (f) -> kernel f == 0

preimage(RingMap,Ideal) := (f,J) -> (
     R := ring J;
     kernel ( map(R/J,R) * f ))

List / RingMap := List => (v,f) -> apply(v,x -> f x)
RingMap \ List := List => (f,v) -> apply(v,x -> f x)
RingMap == ZZ := (f,n) -> (
     if n == 1 then (source f === target f and f === id_(source f))
     else error "encountered integer other than 1 in comparison with a ring map")
ZZ == RingMap := (n,f) -> f == n

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

RingMap ^ ZZ := BinaryPowerMethod

map(Ring,Ring,RingMap) := RingMap => opts -> (R,S,f) -> map(R,S,matrix f,opts)

-- module maps over ring maps:
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
     d := degreeLength M;
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
