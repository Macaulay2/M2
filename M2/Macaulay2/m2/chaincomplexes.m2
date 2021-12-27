--		Copyright 1993-2002 by Daniel R. Grayson
-- TODO:
-- 1. support QQ in rawResolutionGetMatrix, then enable FastNonminimal for QQ

needs "gradedmodules.m2"
needs "hilbert.m2"
needs "modules2.m2"

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

union := (x,y) -> keys(set x + set y)
intersection := (x,y) -> keys(set x * set y)

-----------------------------------------------------------------------------
-- ChainComplex, ChainComplexMap, and Resolution type declarations and basic constructors
-----------------------------------------------------------------------------

Resolution = new Type of MutableHashTable
Resolution.synonym = "resolution"

toString Resolution := C -> toString raw C

ChainComplex = new Type of GradedModule
ChainComplex.synonym = "chain complex"

new ChainComplex := ChainComplex => T -> (
    C := newClass(ChainComplex, new MutableHashTable); -- sigh
    C.cache = new CacheTable;
    C.dd = new ChainComplexMap;
    C.dd.degree = -1;
    C.dd.source =
    C.dd.target = C)

new ChainComplex from Resolution := ChainComplex => (T, R) -> (
    C := new ChainComplex;
    C.ring = R.ring;
    C.Resolution = C.dd.Resolution = R;
    C)

ChainComplexMap = new Type of GradedModuleMap
ChainComplexMap.synonym = "chain complex map"

raw Resolution   := C -> C.RawComputation
raw ChainComplex := C -> raw C.Resolution

ring Resolution      :=
ring ChainComplex    := C -> C.ring
ring ChainComplexMap := C -> ring source C

chainComplex = method(TypicalValue => ChainComplex, Options => true, Dispatch => Thing)
chainComplex Ring     := {} >> opts -> R -> ( C := new ChainComplex; C.ring = R; C )
chainComplex Matrix   := {} >> opts -> f -> chainComplex {f}
chainComplex Sequence :=
chainComplex List     := {} >> opts -> maps -> (
    if #maps === 0 then error "chainComplex: expected at least one differential map";
    C := new ChainComplex;
    R := C.ring = ring target maps#0;
    scan(#maps, i -> (
	    f := maps#i;
	    if R =!= ring f then error "chainComplex: expected differential maps over the same ring";
	    if i > 0 and C#i != target f then (
		deg := degrees C#i - degrees target f;
		if same deg then f = f ** R^(- deg#0)
		else error "chainComplex: expected composable differential maps");
	    C.dd#(i+1) = f;
	    if i === 0 then C#i = target f;
	    C#(i+1) = source f));
    C)
chainComplex GradedModule := {} >> opts -> M -> (
    C := new ChainComplex from M;
    C.cache = new CacheTable;
    C.dd = new ChainComplexMap;
    C.dd.degree = -1;
    C.dd.source =
    C.dd.target = C)

-----------------------------------------------------------------------------
-- ChainComplex, ChainComplexMap, and Resolution methods
-----------------------------------------------------------------------------

complete ChainComplex := ChainComplex => C -> (
     if C.?Resolution and not C.?complete then (
	  for i from 0 to C.Resolution.length do if 0 == (C#i = C_i) then break;
     	  C.complete = true;
	  );
     C)

ChainComplex _ ZZ := Module => (C,i) -> (
     if C#?i 
     then C#i
     else if C.?Resolution then (
	  gr := C.Resolution;
	  F := new Module from (ring C,rawResolutionGetFree(gr.RawComputation,i));
	  if F != 0 then C#i = F;
	  F)
     else (ring C)^0                           -- for chain complexes of sheaves we'll want something else!
     )

ChainComplex _ ZZ = (C,i,M) -> C#i = M

ChainComplex ^ ZZ := Module => (C,i) -> C_-i

length ChainComplex := (C) -> (
     s := select(spots complete C, i -> C_i != 0);
     if #s === 0 then 0 else max s - min s
     )

ChainComplex == ChainComplex := (C,D) -> (
     complete C;
     complete D;
     I := sort union(spots C, spots D);
     all(I, i -> C_i == D_i) and all(I, i -> C.dd_i == D.dd_i)
     )     

ChainComplex == ZZ := (C,i) -> (complete C; all(spots C, i -> C_i == 0))
ZZ == ChainComplex := (i,C) -> (complete C; all(spots C, i -> C_i == 0))

net ChainComplex := C -> (
     complete C;
     s := sort spots C;
     if # s === 0 then "0"
     else (
	  a := s#0;
	  b := s#-1;
	  horizontalJoin between(" <-- ", apply(a .. b,i -> stack (net C_i," ",net i)))))

texMath ChainComplex := C -> (
     complete C;
     s := sort spots C;
     if # s === 0 then "0" else
     concatenate apply(s,i->(
	     if i>s#0 then "\\,\\xleftarrow{" | texMath short C.dd_i | "}\\,",
	     texUnder(texMath C_i,i)
	     ))
      )

-----------------------------------------------------------------------------

complete ChainComplexMap := f -> (
     complete source f;
     if target f =!= source f then complete target f;
     if f.?Resolution then ( i := 1; while f_i != 0 do i = i+1; );
     f)

source ChainComplexMap := f -> f.source
target ChainComplexMap := f -> f.target

sum ChainComplex := Module => C -> (complete C; directSum apply(sort spots C, i -> C_i))
sum ChainComplexMap := Matrix => f -> (
     complete f;
     R := ring f;
     T := target f;
     t := sort spots T;
     S := source f;
     s := sort spots S;
     d := degree f;
     u := spots f;
     if #t === 0 and #s === 0 then map(R^0,0)
     else (
	  tar := if #t === 0 then R^0 else directSum apply(t,i->T_i);
	  src := if #s === 0 then R^0 else directSum apply(s,i->S_i);
	  if #u > 0 and same(apply(u, i -> degree f#i))
	  then (
	       deg := degree f#(u#0);
	       map(tar, src, matrix table(t,s,
			 (j,i) -> if j == i+d then f_i else map(T_j,S_i,0)), Degree=>deg)
	       )
	  else (
	       map(tar, src, matrix table(t,s,
		    	 (j,i) -> if j == i+d then f_i else map(T_j,S_i,0))))))

degree ChainComplexMap := f -> f.degree

net ChainComplexMap := f -> (
     complete f;
     v := between("",
	  apply(sort intersection(spots f.source, spots f.target / (i -> i - f.degree)),
	       i -> horizontalJoin (
		    net (i+f.degree), " : ", net MapExpression { target f_i, source f_i, f_i }, " : ", net i
		    )
	       )
	  );
     if # v === 0 then "0"
     else stack v)

ring ChainComplexMap := (f) -> ring source f

ChainComplexMap _ ZZ := Matrix => (f,i) -> if f#?i then f#i else (
     de := if f.?degree then f.degree else 0;
     so := (source f)_i;
     ta := (target f)_(i+de);
     if f.?Resolution then (
	  gr := f.Resolution;
	  p := map(ta,so,rawResolutionGetMatrix(gr.RawComputation,i));
	  if p != 0 then f#i = p;
	  p)
     else map(ta,so,0))

ChainComplexMap _ ZZ = (f,i,p) -> (
     f#i = p;
     (target f)_(i + degree f) = target p;
     (source f)_i = source p;
     p)

ChainComplex#id = (C) -> (
     complete C;
     f := new ChainComplexMap;
     f.cache = new CacheTable;
     f.source = f.target = C;
     f.degree = 0;
     scan(spots C, i -> f#i = id_(C_i));
     f)
- ChainComplexMap := ChainComplexMap => f -> (
     complete f;
     g := new ChainComplexMap;
     g.cache = new CacheTable;
     g.source = f.source;
     g.target = f.target;
     g.degree = f.degree;
     scan(spots f, i -> g#i = -f_i);
     g)
RingElement + ChainComplexMap := (r,f) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) + f
     else error "expected map to have same source and target and to have degree 0")
ChainComplexMap + RingElement := (f,r) -> r+f

ChainComplexMap + ZZ := (f,i) -> (
     if i === 0 then f
     else if source f != target f
     then error "expected same source and target"
     else f + i*id_(target f))
ZZ + ChainComplexMap := (i,f) -> f+i

RingElement - ChainComplexMap := (r,f) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) - f
     else error "expected map to have same source and target and to have degree 0")
ChainComplexMap - RingElement := (f,r) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) - f
     else error "expected map to have same source and target and to have degree 0")


RingElement == ChainComplexMap := (r,f) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) == f
     else error "expected map to have same source and target and to have degree 0")
ChainComplexMap == RingElement := (f,r) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) == f
     else error "expected map to have same source and target and to have degree 0")
RingElement * ChainComplexMap := (r,f) -> (
     complete f;
     g := new ChainComplexMap;
     g.cache = new CacheTable;
     g.source = f.source;
     g.target = f.target;
     g.degree = f.degree;
     scan(spots f, i -> g#i = r * f_i);
     g)
ZZ * ChainComplexMap := (n,f) -> (
     complete f;
     g := new ChainComplexMap;
     g.cache = new CacheTable;
     g.source = f.source;
     g.target = f.target;
     g.degree = f.degree;
     scan(spots f, i -> g#i = n * f_i);
     g)
ChainComplexMap ^ ZZ := ChainComplexMap => (f,n) -> (
     if source f != target f then error "expected source and target to be the same";
     if n < 0 then error "expected nonnegative integer";
     if n === 0 then id_(source f)
     else (
     	  complete f;
	  g := new ChainComplexMap;
	  g.cache = new CacheTable;
	  C := g.source = f.source;
	  g.target = f.target;
	  d := g.degree = n * f.degree;
	  scan(spots f, i ->
	       if C#?(i+d) and C#(i+d) != 0 then (
		    s := f_i;
		    j := 1;
		    while (
			 if j < n then s != 0
			 else (
			      g#i = s;
			      false)
			 ) do (
			 s = f_(i + j * f.degree) * s;
			 j = j+1;
			 )
		    ));
	  g))
ChainComplexMap + ChainComplexMap := ChainComplexMap => (f,g) -> (
     if source f != source g
     or target f != target g
     or f.degree != g.degree then (
	  error "expected maps of the same degree with the same source and target";
	  );
     h := new ChainComplexMap;
     h.cache = new CacheTable;
     h.source = f.source;
     h.target = f.target;
     h.degree = f.degree;
     complete f;
     complete g;
     scan(union(spots f, spots g), i -> h#i = f_i + g_i);
     h)
ChainComplexMap - ChainComplexMap := ChainComplexMap => (f,g) -> (
     if source f != source g
     or target f != target g
     or f.degree != g.degree then (
	  error "expected maps of the same degree with the same source and target";
	  );
     h := new ChainComplexMap;
     h.cache = new CacheTable;
     h.source = f.source;
     h.target = f.target;
     h.degree = f.degree;
     complete f;
     complete g;
     scan(union(spots f, spots g), i -> h#i = f_i - g_i);
     h)
ChainComplexMap == ChainComplexMap := (f,g) -> (
     if source f != source g
     or target f != target g
     or f.degree != g.degree then (
	  error "expected maps of the same degree with the same source and target";
	  );
     complete f;
     complete g;
     all(union(spots f, spots g), i -> f_i == g_i))
ChainComplexMap == ZZ := (f,i) -> (
     complete f;
     if i === 0 then all(spots f, j -> f_j == 0)
     else source f == target f and f == i id_(source f))
ZZ == ChainComplexMap := (i,f) -> f == i

formation ChainComplexMap := f -> if f.cache.?formation then f.cache.formation

ChainComplexMap ++ ChainComplexMap := ChainComplexMap => (f,g) -> (
     -- why don't we implement ChainComplexMap.directSum instead?
     if f.degree != g.degree then (
	  error "expected maps of the same degree";
	  );
     h := new ChainComplexMap;
     h.cache = new CacheTable;
     h.source = f.source ++ g.source;
     h.target = f.target ++ g.target;
     h.degree = f.degree;
     complete f;
     complete g;
     scan(union(spots f, spots g), i -> h#i = f_i ++ g_i);
     h.cache.components = {f,g};
     h.cache.formation = BinaryOperation { symbol ++, f, g };
     h)

isHomogeneous ChainComplexMap := f -> (complete f; all(spots f, i -> isHomogeneous f_i))
isHomogeneous ChainComplex := C -> isHomogeneous C.dd

isDirectSum ChainComplex := (C) -> C.cache.?components
components ChainComplexMap := f -> if f.cache.?components then f.cache.components else {f}
ChainComplexMap _ Array := ChainComplexMap => (f,v) -> f * (source f)_v
ChainComplexMap ^ Array := ChainComplexMap => (f,v) -> (target f)^v * f

RingMap ChainComplex := ChainComplex => (f,C) -> (
     D := new ChainComplex;
     D.ring = target f;
     complete C;
     scan(spots C, i -> D#i = f C#i);
     complete C.dd;
     scan(spots C.dd, i -> D.dd#i = map(D_(i-1),D_i, f C.dd#i));
     D)

RingMap ChainComplexMap := ChainComplexMap => (f,phi) -> map(f target phi, f source phi, i -> f phi_i)

ChainComplexMap * ChainComplexMap := ChainComplexMap => (g,f) -> (
     if target f != source g then error "expected composable maps of chain complexes";
     h := new ChainComplexMap;
     h.cache = new CacheTable;
     h.source = source f;
     h.target = target g;
     h.degree = f.degree + g.degree;
     complete f;
     complete g;
     scan(union(spots f, apply(spots g, i -> i - f.degree)),
	  i -> h#i = g_(i+f.degree) * f_i);
     h)

extend = method(Options => {Verify => true})

extend(ChainComplex,ChainComplex,Matrix) := ChainComplexMap => opts -> (D,C,fi)-> (
     i := 0;
     j := 0;
     f := new ChainComplexMap;
     f.cache = new CacheTable;
     f.source = C;
     f.target = D;
     complete C;
     s := f.degree = j-i;
     f#i = fi;
     n := i+1;
     while C#?n do (
	  p := f_(n-1) * C.dd_n;
	  q := D.dd_(n+s);
	  if opts.Verify then (
	       (quot,rem) := quotientRemainder(p,q);
	       if rem != 0 then error "map cannot be extended";
	       f#n = quot;
	       )
	  else (
	       f#n = p // q;
	       );
	  n = n+1;
	  );
     f)

cone ChainComplexMap := ChainComplex => f -> (
     complete f;
     if f.degree =!= 0 then error "expected a map of chain complexes of degree zero";
     C := source f;
     D := target f;
     E := new ChainComplex;
     E.ring = ring f;
     scan(union(spots C /( i -> i+1 ), spots D), i -> E#i = D_i ++ C_(i-1));
     complete C.dd;
     complete D.dd;
     bump := x -> apply(x, i -> i+1);
     scan(keys(set bump spots C.dd + set spots D.dd + set bump spots f),
	  i -> E.dd#i = 
          D.dd_i                  |      f_(i-1)    ||
          map(C_(i-2),D_i,0)      |   - C.dd_(i-1)
          );
     E)

nullhomotopy ChainComplexMap := ChainComplexMap => f -> (
     s := new ChainComplexMap;
     s.cache = new CacheTable;
     s.ring = ring f;
     s.source = C := source f;
     c := C.dd;
     s.target = D := target f;
     b := D.dd;
     deg := s.degree = f.degree + 1;
     complete f;
     scan(sort spots f, i -> 
	  (
	       if s#?(i-1) and c#?i
	       then if f#?i
	       then (
		    -- if    (f_i - s_(i-1) * c_i) %  b_(i+deg) != 0
		    -- then error "expected map to be null homotopic";
		    s#i = (f_i - s_(i-1) * c_i) // b_(i+deg)
		    )
	       else (
		    -- if    (    - s_(i-1) * c_i) %  b_(i+deg) != 0
		    -- then error "expected map to be null homotopic";
		    s#i = (    - s_(i-1) * c_i) // b_(i+deg)
		    )
	       else if f#?i 
	       then (
		    -- if    (f_i                ) %  b_(i+deg) != 0
		    -- then error "expected map to be null homotopic";
		    s#i = (f_i                ) // b_(i+deg)
		    )
	       )
	  );
     s)

-----------------------------------------------------------------------------
poincare ChainComplex := C -> (
     R := ring C;
     S := degreesRing R;
     use S;
     f := 0_S;
     complete C;
     scan(keys C, i -> if class i === ZZ then scanPairs(tally degrees C_i, (d,m) -> f = f + m * (-1)^i * product(# d, j -> S_j^(d_j))));
     f)

-- TODO: make poincareN return in variables of (degreesRing R)[S],
-- so that sub(poincareN C, S => -1) == poincare C holds
poincareN = method(TypicalValue => RingElement)
poincareN ChainComplex := (C) -> (
     s := getSymbol "S";
     t := getSymbol "T";
     -- this stuff has to be redone as in Poincare itself, DRG
     R := ZZ[s, t_0 .. t_(degreeLength ring C - 1), Inverses=>true, MonomialOrder => RevLex, Global => false];
     f := 0_R;
     complete C;
     scan(keys C, n -> if class n === ZZ then scanPairs(tally degrees C_n, (d,m) -> f = f + m * R_0^n * product(# d, j -> R_(j+1)^(d_j))));
     f )

ChainComplex ** Ring := ChainComplex => (C,S) -> (
     complete C;
     complete C.dd;
     D := new ChainComplex;
     D.cache = new CacheTable;
     D.complete = true;
     D.ring = S;
     D.dd = new ChainComplexMap;
     D.dd.cache = new CacheTable;
     D.dd.degree = deg := C.dd.degree;
     D.dd.source = D;
     D.dd.target = D;
     scan(spots C   , i -> D  #i = C#i ** S);
     scan(spots C.dd, i -> D.dd#i = map(D#(i+deg),D#i,(cover C.dd#i) ** S));
     D)

ChainComplex ** Module := ChainComplex => (C,M) -> (
     D := new ChainComplex;
     D.ring = ring C;
     complete C.dd;
     scan(keys C.dd,i -> if class i === ZZ then (
	       f := D.dd#i = C.dd#i ** M;
	       D#i = source f;
	       D#(i-1) = target f;
	       ));
     D)

Module ** ChainComplex := ChainComplex => (M,C) -> (
     D := new ChainComplex;
     D.ring = ring C;
     complete C.dd;
     scan(keys C.dd,i -> if class i === ZZ then (
	       f := D.dd#i = M ** C.dd#i;
	       D#i = source f;
	       D#(i-1) = target f;
	       ));
     D)

Module ** ChainComplexMap := ChainComplexMap => (M,f) -> (
     map(M ** target f, M ** source f, i -> M ** f_i, Degree => degree f)
     )

ChainComplexMap ** Module := ChainComplexMap => (f,M) -> (
     map(target f ** M, source f ** M, i -> f_i ** M, Degree => degree f)
     )

-----------------------------------------------------------------------------

homology(ZZ,ChainComplex) := Module => opts -> (i,C) -> homology(C.dd_i, C.dd_(i+1))
cohomology(ZZ,ChainComplex) := Module => opts -> (i,C) -> homology(-i, C)

homology(ZZ,ChainComplexMap) := Matrix => opts -> (i,f) -> (
     inducedMap(homology(i+degree f,target f), homology(i,source f),f_i)
     )
cohomology(ZZ,ChainComplexMap) := Matrix => opts -> (i,f) -> homology(-i,f)

homology(ChainComplex) := GradedModule => opts -> (C) -> (
     H := new GradedModule;
     H.ring = ring C;
     complete C;
     scan(spots C, i -> H#i = homology(i,C));
     H)

gradedModule(ChainComplex) := GradedModule => (C) -> (
     H := new GradedModule;
     H.ring = ring C;
     complete C;
     scan(spots C, i -> H#i = C#i);
     H)

homology(ChainComplexMap) := GradedModuleMap => opts -> (f) -> (
     g := new GradedModuleMap;
     g.degree = f.degree;
     g.source = HH f.source;
     g.target = HH f.target;
     complete f;
     scan(spots f, i -> g#i = homology(i,f));
     g)

formation ChainComplex := M -> if M.cache.?formation then M.cache.formation

directSum ChainComplex := C -> directSum(1 : C)
ChainComplex.directSum = args -> (
     C := new ChainComplex;
     C.ring = ring args#0;
     scan(args,D -> (complete D; complete D.dd;));
     scan(unique flatten (args/spots), n -> C#n = directSum apply(args, D -> D_n));
     scan(spots C, n -> if C#?(n-1) then C.dd#n = directSum apply(args, D -> D.dd_n));
     C.cache.components = toList args;
     C.cache.formation = FunctionApplication { directSum, args };
     C)
ChainComplex ++ ChainComplex := ChainComplex => (C,D) -> directSum(C,D)

components ChainComplex := C -> if C.cache.?components then C.cache.components else {C}

ChainComplex Array := ChainComplex => (C,A) -> (
     if # A =!= 1 then error "expected array of length 1";
     n := A#0;
     if not instance(n,ZZ) then error "expected an integer";
     D := new ChainComplex;
     b := D.dd;
     D.ring = ring C;
     complete C;
     scan(pairs C,(i,F) -> if class i === ZZ then D#(i-n) = F);
     complete C.dd;
     if even n
     then scan(pairs C.dd, (i,f) -> if class i === ZZ then b#(i-n) = f)
     else scan(pairs C.dd, (i,f) -> if class i === ZZ then b#(i-n) = -f);
     D)

ChainComplexMap Array := ChainComplexMap => (f,A) -> (
     if # A =!= 1 then error "expected array of length 1";
     n := A#0;
     if not instance(n,ZZ) then error "expected an integer";
     complete f;
     g := new ChainComplexMap;
     g.cache = new CacheTable;
     g.source = f.source A;
     g.target = f.target A;
     g.degree = f.degree;
     scan(spots f, i -> g#(i-n) = f_i);
     g)

Hom(ChainComplex, Module) := ChainComplex => (C,N) -> (
     c := C.dd;
     complete c;
     D := new ChainComplex;
     D.ring = ring C;
     b := D.dd;
     scan(spots C, i -> D#-i = Hom(C_i,N));
     scan(spots c, i -> (
	       j := - i + 1;
	       f := b#j = (-1)^j * Hom(c_i,N);
	       D#j = source f;
	       D#(j-1) = target f));
     D)

Hom(Module, ChainComplex) := ChainComplex => (M,C) -> (
     complete C.dd;
     D := new ChainComplex;
     D.ring = ring C;
     scan(spots C.dd, i -> (
	       f := D.dd#i = Hom(M,C.dd_i);
	       D#i = source f;
	       D#(i-1) = target f;
	       ));
     D)

dual ChainComplex := ChainComplex => {} >> o -> (C) -> (
	  R := ring C;
	  Hom(C,R^1))

Hom(ChainComplexMap, Module) := ChainComplexMap => (f,N) -> (
     complete f;
     g := new ChainComplexMap;
     g.cache = new CacheTable;
     d := g.degree = f.degree;
     g.source = Hom(target f, N);
     g.target = Hom(source f, N);
     scan(spots f, i -> (
	       j := -i-d;
	       g#j = (-1)^(j*d) * Hom(f#i,N);
	       ));
     g)

Hom(Module, ChainComplexMap) := ChainComplexMap => (N,f) -> (
     complete f;
     g := new ChainComplexMap;
     g.cache = new CacheTable;
     d := g.degree = f.degree;
     g.source = Hom(N, source f);
     g.target = Hom(N, target f);
     scan(spots f, i -> g#i = Hom(N,f#i));
     g)

transpose ChainComplexMap := f -> dual f
dual ChainComplexMap := ChainComplexMap => {} >> o -> f -> Hom(f, (ring f)^1)

-----------------------------------------------------------------------------
nonMinRes = m -> (
     F' := resolution image m;
     complete F';
     chainComplex apply(1+length F', j-> if j==0 then m else F'.dd_j))

syzygyScheme = (C,i,v) -> (
           g := extend(nonMinRes transpose (C.dd_i * v), dual C[-i], transpose v);
           minimalPresentation cokernel (C.dd_1  * transpose g_(i-1)))
-----------------------------------------------------------------------------

tens := (R,f,g) -> map(R, rawTensor( f.RawMatrix, g.RawMatrix ))

ChainComplex ** ChainComplex := ChainComplex => (C,D) -> (
     R := ring C;
     if ring D =!= R then error "expected chain complexes over the same ring";
     complete C;
     complete D;
     E := chainComplex (lookup(symbol **, GradedModule, GradedModule))(C,D);
     scan(spots E, i -> if E#?i and E#?(i-1) then E.dd#i = 
	  map( E#(i-1), E#i,
	       concatBlocks(table(
			 E#(i-1).cache.indices,
			 E#i.cache.indices,
			 (j,k) -> map(
				   E#(i-1).cache.components#(E#(i-1).cache.indexComponents#j),
				   E#i.cache.components#(E#i.cache.indexComponents#k),
				   if j#0 === k#0 and j#1 === k#1 - 1 then (-1)^(k#0) * tens(R, id_(cover C#(j#0)), matrix D.dd_(k#1))
				   else if j#0 === k#0 - 1 and j#1 === k#1 then tens(R, matrix C.dd_(k#0), id_(cover D#(k#1)))
				   else 0)))));
     E)

ChainComplex ** GradedModule := ChainComplex => (C,D) -> C ** chainComplex D

GradedModule ** ChainComplex := ChainComplex => (C,D) -> chainComplex C ** D

ChainComplexMap ** ChainComplexMap := ChainComplexMap => (f,g) -> (
     h := new ChainComplexMap;
     h.cache = new CacheTable;
     E := h.source = source f ** source g;
     F := h.target = target f ** target g;
     deg := h.degree = f.degree + g.degree;
     scan(spots E, n -> if F#?(n+deg) then (
	       E' := E#n;
	       E'i := E'.cache.indexComponents;
	       E'c := E'.cache.components;
	       F' := F#(n+deg);
	       F'i := F'.cache.indexComponents;
	       h#n = map(F',E', matrix {
			 apply(E'.cache.indices, (i,j) -> (
				   t := (i+f.degree, j+g.degree);
				   if F'i#?t then F'_[t] * ( ((-1)^(g.degree * i) * f_i ** g_j) )
				   else map(F',E'c#(E'i#(i,j)),0)))})));
     h)

ChainComplexMap ** ChainComplex := ChainComplexMap => (f,C) -> f ** id_C
ChainComplex ** ChainComplexMap := ChainComplexMap => (C,f) -> id_C ** f

-- min ChainComplex := C -> min spots C
-- max ChainComplex := C -> max spots C

tensorAssociativity(Module,Module,Module) := Matrix => (A,B,C) -> map((A**B)**C,A**(B**C),1)

tensorAssociativity(ChainComplex,ChainComplex,ChainComplex) := ChainComplexMap => (A,B,C) -> (
     R := ring A;
     map(
	  F := (AB := A ** B) ** C,
	  E :=  A ** (BC := B ** C),
	  k -> concatBlocks(apply(F_k.cache.indices, (ab,c) -> (
			 apply(E_k.cache.indices, (a,bc) -> (
				   b := bc-c;  -- ab+c=k=a+bc, so b=bc-c=ab-a
				   if A#?a and B#?b and C#?c
				   then (
					(AB#ab_[(a,b)] ** C#c)
					* tensorAssociativity(A#a,B#b,C#c)
					* (A#a ** BC#bc^[(b,c)])
					)
				   else map(F_k.cache.components#(F_k.cache.indexComponents#(ab,c)),
					     E_k.cache.components#(E_k.cache.indexComponents#(a,bc)),
					     0))))))
	       ))


     -- 	  k -> sum(E_k.cache.indices, (a,bc) -> (
     -- 		    sum(BC_bc.cache.indices, (b,c) -> (
     -- 			      F_k_[(a+b,c)]
     -- 			      * (AB_(a+b)_[(a,b)] ** C_c)
     -- 			      * tensorAssociativity(A_a,B_b,C_c)
     -- 			      * (A_a ** BC_bc^[(b,c)])
     -- 			      )) * E_k^[(a,bc)]))

Module Array := ChainComplex => (M,v) -> (
     if #v =!= 1 then error "expected array of length 1";
     n := v#0;
     if class n =!= ZZ then error "expected [n] with n an integer";
     C := new ChainComplex;
     C.ring = ring M;
     C#-n = M;
     C)

ChainComplexMap _ Array := ChainComplexMap => (f,v) -> f * (source f)_v
ChainComplexMap ^ Array := ChainComplexMap => (f,v) -> (target f)^v * f

trans := (C,v) -> (
     if C.cache.?indexComponents then (
	  Ci := C.cache.indexComponents;
	  apply(v, i -> if Ci#?i then Ci#i else error "expected an index of a component of the direct sum"))
     else (
     	  if not C.cache.?components then error "expected a direct sum of chain complexes";
	  Cc := C.cache.components;
	  apply(v, i -> if not Cc#?i then error "expected an index of a component of the direct sum");
	  v)
     )
ChainComplex _ Array := ChainComplexMap => (C,v) -> if C#?(symbol _,v) then C#(symbol _,v) else C#(symbol _,v) = (
     v = trans(C,v);
     D := directSum apply(toList v, i -> C.cache.components#i);
     map(C,D,k -> C_k_v))

ChainComplex ^ Array := ChainComplexMap => (C,v) -> if C#?(symbol ^,v) then C#(symbol ^,v) else C#(symbol ^,v) = (
     v = trans(C,v);
     D := directSum apply(toList v, i -> C.cache.components#i);
     map(D,C,k -> C_k^v))

map(ChainComplex,ChainComplex,Function) := ChainComplexMap => options -> (C,D,f) -> (
     h := new ChainComplexMap;
     h.cache = new CacheTable;
     h.source = D;
     h.target = C;
     deg := h.degree = if options.Degree === null then 0 else options.Degree;
     complete D;
     scan(spots D, k -> (
	       if C#?(k+deg) then (
		    g := f(k);
		    if g =!= null and g != 0 then h#k = map(C#(k+deg),D#k,g);
		    )));
     h
     )

map(ChainComplex,ChainComplex,ChainComplexMap) := ChainComplexMap => opts -> (C,D,f) -> map(C,D,k -> f_k,opts)

inducedMap(ChainComplex,ChainComplex) := ChainComplexMap => options -> (C,D) -> (
     h := new ChainComplexMap;
     h.cache = new CacheTable;
     h.source = D;
     h.target = C;
     deg := h.degree = if options.Degree === null then 0 else options.Degree;
     complete D;
     scan(spots D, k -> if C#?(k+deg) then h#k = inducedMap(C#(k+deg),D#k));
     h
     )

kernel ChainComplexMap := ChainComplex => options -> (f) -> (
     D := source f;
     C := new ChainComplex;
     C.ring = ring f;
     complete D;
     scan(spots D, k -> C#k = kernel f_k);
     scan(spots C, k -> if C#?(k-1) then C.dd#k = (D.dd_k * inducedMap(D_k,C_k)) // inducedMap(D_(k-1),C_(k-1)));
     C)

coimage ChainComplexMap := ChainComplex => (f) -> (
     D := source f;
     C := new ChainComplex;
     C.ring = ring f;
     complete D;
     scan(spots D, k -> C#k = coimage f_k);
     scan(spots C, k -> if C#?(k-1) then C.dd#k = map(C#(k-1),C#k,matrix D.dd_k));
     C)

cokernel ChainComplexMap := ChainComplex => (f) -> (
     D := target f;
     deg := f.degree;
     C := new ChainComplex;
     C.ring = ring f;
     complete D;
     scan(spots D, k -> C#k = cokernel f_(k-deg));
     scan(spots C, k -> if C#?(k-1) then C.dd#k = map(C#(k-1),C#k,matrix D.dd_k));
     C)

image ChainComplexMap := ChainComplex => (f) -> (
     D := target f;
     E := source f;
     deg := f.degree;
     C := new ChainComplex;
     C.ring = ring f;
     complete D;
     scan(spots D, k -> C#k = image f_(k-deg));
     scan(spots C, k -> if C#?(k-1) then C.dd#k = map(C#(k-1),C#k,matrix E.dd_(k-deg)));
     C)

minimalPresentation ChainComplex := prune ChainComplex := ChainComplex => opts -> (C) -> (
     D := new ChainComplex;
     complete C;
     complete C.dd;
     D.ring = ring C;
     scan(spots C, i -> D#i = minimalPresentation C#i);
     scan(spots C.dd, i -> D.dd#i = minimalPresentation C.dd#i);
     D)

minimalPresentation ChainComplexMap := prune ChainComplexMap := ChainComplexMap => opts -> (f) -> (
     complete f;
     map(minimalPresentation target f, minimalPresentation source f, k -> minimalPresentation f#k)
     )
----------------------------------------
compositions = method(TypicalValue => List);
compositions(ZZ,ZZ) := (n,k) -> (
     compositionn := (n,k) -> (
	  if n===0 or k < 0 then {}
	  else if k===0 then {toList(n:0)}
	  else join(
	       apply(compositionn(n-1,k), s -> s | {0}),
	       apply(compositionn(n,k-1), s -> s + (toList(n-1:0) | {1}))));
     compositionn = memoize compositionn;
     result := compositionn(n,k);
     compositionn = null;
     result);

eagonNorthcott = method(TypicalValue => ChainComplex)
eagonNorthcott Matrix := f -> (
     -- code is by GREG SMITH, but is experimental, and 
     -- should be replaced by engine code
     -- Modified by ELIANA DUARTE to fix the grading for matrices 
     -- with entries of arbitrary degrees.
     if not isHomogeneous f then error "Matrix not homogeneous.";
     R := ring f;
     m := rank source f;
     n := rank target f;
     B := hashTable apply(toList(1..m-n+2), 
     	  i -> {i, flatten table(subsets(m,n+i-1), compositions(n,i-1), 
	       	    (p,q) -> {p,q})});
     d1 := map(R^1,, {apply(B#1, r -> determinant f_(r#0))});
     nextDegrees := toSequence(-flatten degrees source d1);
     d := {d1};
     j:=2; while j<m-n+3 do(
	             d=d|{map(source d_(j-2),, table(B#(j-1), B#j, 
                          (p,q) -> if not isSubset(p#0,q#0) then 0_R
                          else (
                               vec := q#1 - p#1;
                               if any(vec, e -> e < 0 or e > 1) then 0_R 
                               else (
                                    s := first select(toList(0..#q#0-1), 
                                         l -> not member(q#0#l, p#0));
                                    t := first select(toList(0..n-1), l -> vec#l == 1);
                                    (-1)^(s+1)*f_(t,q#0#s)))))};
		    j=j+1) ;
      chainComplex d);

------ koszul

koszul(ZZ, Matrix) := Matrix => (i,m) -> map(ring m, rawKoszul(i, raw m))

koszul Matrix := ChainComplex => f ->(
     n := rank source f;
     chainComplex toList apply(1 .. n, i -> koszul(i,f)))

-- this seems not to be useful to the user (yet):
-- koszul(ZZ, Matrix, Matrix) := Matrix => (i,m,n) -> map(ring m, rawKoszulMonomials(i, raw m, raw n))


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
