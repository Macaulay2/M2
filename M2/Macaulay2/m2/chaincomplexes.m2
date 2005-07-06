--		Copyright 1993-2002 by Daniel R. Grayson

Resolution = new Type of MutableHashTable
Resolution.synonym = "resolution"
toString Resolution := g -> "<<resolution " | toString g.handle | ">>"
raw Resolution := X -> X.RawComputation

ChainComplex = new Type of GradedModule
ChainComplex.synonym = "chain complex"
new ChainComplex := ChainComplex => (cl) -> (
     C := newClass(ChainComplex,new MutableHashTable); -- sigh
     C.cache = new CacheTable;
     b := C.dd = new ChainComplexMap;
     b.degree = -1;
     b.source = b.target = C;
     C)

raw ChainComplex := C -> raw C.Resolution

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
	  F := newModule(ring C,rawResolutionGetFree(gr.RawComputation,i));
	  if F != 0 then C#i = F;
	  F)
     else (ring C)^0                           -- for chain complexes of sheaves we'll want something else!
     )

ChainComplex ^ ZZ := Module => (C,i) -> C_-i

spots := C -> select(keys C, i -> class i === ZZ)
union        := (x,y) -> keys(set x + set y)
intersection := (x,y) -> keys(set x * set y)

rank ChainComplex := C -> sum(spots C, i -> rank C_i)

length ChainComplex := (C) -> (
     s := select(spots complete C, i -> C_i != 0);
     if #s === 0 then 0 else max s - min s
     )

ChainComplex == ChainComplex := (C,D) -> (
     all(sort union(spots C, spots D), i -> C_i == D_i)
     )     

ChainComplex == ZZ := (C,i) -> all(spots C, i -> C_i == 0)
ZZ == ChainComplex := (i,C) -> all(spots C, i -> C_i == 0)

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
     if # s === 0 then "0"
     else (
	  a := s#0;
	  b := s#-1;
	  horizontalJoin between(" \\leftarrow ", apply(a .. b,i -> texMath C_i))))

tex ChainComplex := C -> "$" | texMath C | "$"

-----------------------------------------------------------------------------
ChainComplexMap = new Type of MutableHashTable
ChainComplexMap.synonym = "chain complex map"
ring ChainComplexMap := C -> ring source C
complete ChainComplexMap := f -> (
     complete source f;
     if target f =!= source f then complete target f;
     if f.?Resolution then ( i := 1; while f_i != 0 do i = i+1; );
     f)

source ChainComplexMap := f -> f.source
target ChainComplexMap := f -> f.target

lineOnTop := (s) -> concatenate(width s : "-") || s

sum ChainComplex := Module => C -> directSum apply(sort spots C, i -> C_i)
sum ChainComplexMap := Matrix => f -> (
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
		    net (i+f.degree), " : ", net target f_i, " <--",
		    lineOnTop net f_i,
		    "-- ", net source f_i, " : ", net i
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

ChainComplex#id = (C) -> (
     complete C;
     f := new ChainComplexMap;
     f.source = f.target = C;
     f.degree = 0;
     scan(spots C, i -> f#i = id_(C_i));
     f)
- ChainComplexMap := ChainComplexMap => f -> (
     complete f;
     g := new ChainComplexMap;
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
     g.source = f.source;
     g.target = f.target;
     g.degree = f.degree;
     scan(spots f, i -> g#i = r * f_i);
     g)
ZZ * ChainComplexMap := (n,f) -> (
     complete f;
     g := new ChainComplexMap;
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
ChainComplexMap ++ ChainComplexMap := ChainComplexMap => (f,g) -> (
     if f.degree != g.degree then (
	  error "expected maps of the same degree";
	  );
     h := new ChainComplexMap;
     h.source = f.source ++ g.source;
     h.target = f.target ++ g.target;
     h.degree = f.degree;
     complete f;
     complete g;
     scan(union(spots f, spots g), i -> h#i = f_i ++ g_i);
     h.cache.components = {f,g};
     h)

isHomogeneous ChainComplexMap := f -> all(spots f, i -> isHomogeneous f_i)
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

ChainComplexMap * ChainComplexMap := ChainComplexMap => (g,f) -> (
     if target f != source g then error "expected composable maps of chain complexes";
     h := new ChainComplexMap;
     h.source = source f;
     h.target = target g;
     h.degree = f.degree + g.degree;
     complete f;
     complete g;
     scan(union(spots f, apply(spots g, i -> i - f.degree)),
	  i -> h#i = g_(i+f.degree) * f_i);
     h)

extend = method()

extend(ChainComplex,ChainComplex,Matrix) := ChainComplexMap => (D,C,fi)-> (
     i := 0;
     j := 0;
     f := new ChainComplexMap;
     f.source = C;
     f.target = D;
     complete C;
     s := f.degree = j-i;
     f#i = fi;
     n := i+1;
     while C#?n do (
	  f#n = (f_(n-1) * C.dd_n) // D.dd_(n+s);
	  n = n+1;
	  );
     f)

cone ChainComplexMap := ChainComplex => f -> (
     if f.degree =!= 0 then error "expected a map of chain complexes of degree zero";
     C := source f;
     D := target f;
     E := new ChainComplex;
     E.ring = ring f;
     complete C;
     complete D;
     scan(union(spots C /( i -> i+1 ), spots D), i -> E#i = D_i ++ C_(i-1));
     complete C.dd;
     complete D.dd;
     scan(union(spots C.dd /( i -> i+1 ), spots D.dd), i -> E.dd#i = 
	       D.dd_i	      	       |      f_(i-1)    ||
	       map(C_(i-2),D_i,0)      |   - C.dd_(i-1)
	       );
     E)

nullhomotopy ChainComplexMap := ChainComplexMap => f -> (
     s := new ChainComplexMap;
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
     G := monoid S;
     use S;
     f := 0_S;
     complete C;
     scan(keys C, i -> (
	       if class i === ZZ
	       then scanPairs(tally degrees C_i, 
		    (d,m) -> f = f + m * (-1)^i * product(# d, j -> G_j^(d_j)))));
     f)

poincareN ChainComplex := (C) -> (
     s := global S;
     t := global T;
     G := monoid [s, t_0 .. t_(degreeLength ring C - 1), Inverses=>true, MonomialOrder => RevLex];
     -- this stuff has to be redone as in Poincare itself, DRG
     R := ZZ G;
     f := 0_R;
     complete C;
     scan(keys C, n -> (
	       if class n === ZZ
	       then scanPairs(tally degrees C_n, 
		    (d,m) -> f = f + m * G_0^n * product(# d, j -> G_(j+1)^(d_j)))));
     f )

ChainComplex ** Module := ChainComplex => (C,M) -> (
--      P := youngest(C,M);
--      key := (C,M,symbol **);
--      if P#?key then P#key
--      else C**M = 
     (
	  D := new ChainComplex;
	  D.ring = ring C;
	  complete C.dd;
	  scan(keys C.dd,i -> if class i === ZZ then (
		    f := D.dd#i = C.dd#i ** M;
		    D#i = source f;
		    D#(i-1) = target f;
		    ));
	  D))

Module ** ChainComplex := ChainComplex => (M,C) -> (
--      P := youngest(M,C);
--      key := (M,C,symbol **);
--      if P#?key then P#key
--      else M**C = 
     (
	  D := new ChainComplex;
	  D.ring = ring C;
	  complete C.dd;
	  scan(keys C.dd,i -> if class i === ZZ then (
		    f := D.dd#i = M ** C.dd#i;
		    D#i = source f;
		    D#(i-1) = target f;
		    ));
	  D))

Module ** ChainComplexMap := ChainComplexMap => (M,f) -> (
     map(M ** target f, M ** source f, i -> M ** f_i)
     )

ChainComplexMap ** Module := ChainComplexMap => (f,M) -> (
     map(target f ** M, source f ** M, i -> f_i ** M)
     )

-----------------------------------------------------------------------------

homology(ZZ,ChainComplex) := Module => opts -> (i,C) -> homology(C.dd_i, C.dd_(i+1))
cohomology(ZZ,ChainComplex) := Module => opts -> (i,C) -> homology(-i, C)

homology(ZZ,ChainComplexMap) := Matrix => opts -> (i,f) -> (
     inducedMap(homology(i+degree f,target f), homology(i,source f),f_i)
     )
cohomology(ZZ,ChainComplexMap) := Matrix => opts -> (i,f) -> homology(-i,f)

homology(Nothing,ChainComplex) := homology(ChainComplex) := GradedModule => opts -> (C) -> (
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

homology(Nothing,ChainComplexMap) := homology(ChainComplexMap) := GradedModuleMap => opts -> (f) -> (
     g := new GradedModuleMap;
     g.degree = f.degree;
     g.source = HH f.source;
     g.target = HH f.target;
     scan(spots f, i -> g#i = homology(i,f));
     g)

chainComplex = method(SingleArgumentDispatch=>true)

chainComplex Matrix := ChainComplexMap => f -> chainComplex {f}

chainComplex Sequence := chainComplex List := ChainComplex => maps -> (
     if #maps === 0 then error "expected at least one differential map";
     C := new ChainComplex;
     R := C.ring = ring target maps#0;
     scan(#maps, i -> (
	       f := maps#i;
	       if R =!= ring f
	       then error "expected differential maps over the same ring";
	       if i > 0 and C#i != target f then (
		    diff := degrees C#i - degrees target f;
		    if same diff
		    then f = f ** R^(- diff#0)
		    else error "expected composable differential maps";
		    );
	       C.dd#(i+1) = f;
	       if i === 0 then C#i = target f;
	       C#(i+1) = source f;
	       ));
     C)

betti = method(TypicalValue => Net)

betti Matrix := f -> betti chainComplex f
betti GroebnerBasis := G -> betti generators G
betti Ideal := I -> "generators: " | betti generators I
betti Module := M -> (
     if M.?relations then (
	  if M.?generators then (
	       "generators: " | betti generators M || "relations : " | betti relations M
	       )
	  else "relations : " | betti relations M
	  )
     else "generators: " | betti generators M
     )

directSum ChainComplex := C -> directSum(1 : C)
ChainComplex.directSum = args -> (
     C := new ChainComplex;
     C.cache.components = toList args;
     C.ring = ring args#0;
     scan(args,D -> (complete D; complete D.dd;));
     scan(unique flatten (args/spots), n -> C#n = directSum apply(args, D -> D_n));
     scan(spots C, n -> if C#?(n-1) then C.dd#n = directSum apply(args, D -> D.dd_n));
     C)
ChainComplex ++ ChainComplex := ChainComplex => (C,D) -> directSum(C,D)

components ChainComplex := C -> if C.cache.?components then C.cache.components else {C}

ChainComplex Array := ChainComplex => (C,A) -> (
     if # A =!= 1 then error "expected array of length 1";
     n := A#0;
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

Hom(ChainComplex, Module) := ChainComplex => (C,N) -> (
     c := C.dd;
     complete c;
     D := new ChainComplex;
     D.ring = ring C;
     b := D.dd;
     scan(spots c, i -> (
	       j := - i + 1;
	       f := b#j = (-1)^j * Hom(c_i,N);
	       D#j = source f;
	       D#(j-1) = target f;
	       ));
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

dual ChainComplex := ChainComplex => (C) -> (
	  R := ring C;
	  Hom(C,R^1))

Hom(ChainComplexMap, Module) := ChainComplexMap => (f,N) -> (
     g := new ChainComplexMap;
     d := g.degree = f.degree;
     g.source = Hom(target f, N);
     g.target = Hom(source f, N);
     scan(spots f, i -> (
	       j := -i-d;
	       g#j = (-1)^(j*d) * Hom(f#i,N);
	       ));
     g)

Hom(Module, ChainComplexMap) := ChainComplexMap => (N,f) -> (
     g := new ChainComplexMap;
     d := g.degree = f.degree;
     g.source = Hom(N, source f);
     g.target = Hom(N, target f);
     scan(spots f, i -> g#i = Hom(N,f#i));
     g)

transpose ChainComplexMap := dual ChainComplexMap := ChainComplexMap => f -> Hom(f, (ring f)^1)

regularity ChainComplex := C -> (
     max flatten apply(
	  select(pairs complete C, (n,F) -> class n === ZZ),
	  (n,F) -> apply(degrees F, d -> first d - n)))

regularity Module := (M) -> regularity resolution M

regularity Ideal := (I) -> 1 + regularity resolution cokernel generators I

rawbetti := method()
     -- returns a hash table with pairs of the form (d,i) => n
     -- where d is the multi-degree, i is the homological degree, and 
     -- n is the betti number.
rawbetti Resolution := X -> (
     bettiType := 0;
     w := rawGBBetti(X.RawComputation, bettiType);
     lo := w#0;
     hi := w#1;
     len := w#2;
     w = drop(w,3);
     w = pack(len+1,w);
     w = table(lo .. hi, 0 .. len, (i,j) -> ({i+j},j) => w#(i-lo)#j);
     w = hashTable toList splice w;
     w = select(w, n -> n != 0);
     w )
rawbetti ChainComplex := C -> (
     if C.?Resolution and degreeLength ring C === 1 then (
     	  repair := (ring C).Repair;
     	  applyKeys( rawbetti C.Resolution, (d,i) -> (first repair d,i) )
	  )
     else (
     	  betti := new MutableHashTable;
	  complete C;
	  p := select(pairs C, (i,F) -> class i === ZZ);
	  hashTable flatten apply(p, (i,F) -> apply(pairs tally apply(degrees F, first), (d,n) -> (d,i) => n))
	  )
     )

bettiDisplay := v -> (
     -- convert the hash table created by rawbetti to the standard display
     v = applyKeys( v, (d,i) -> (d-i,i) );		    -- skew the degrees
     k := keys v;
     fi := first \ k;
     la := last  \ k;
     mincol := min la;
     maxcol := max la;
     minrow := min fi;
     maxrow := max fi;
     v = table(toList (minrow .. maxrow), toList (mincol .. maxcol),
	  (i,j) -> if v#?(i,j) then v#(i,j) else 0);
     leftside := apply(
	  splice {"total:", apply(minrow .. maxrow, i -> toString i | ":")},
	  s -> (6-# s,s));
     totals := apply(transpose v, sum);
     v = prepend(totals,v);
     v = transpose v;
     v = applyTable(v, bt -> if bt === 0 then "." else toString bt);
     v = apply(v, col -> (
	       wid := 1 + max apply(col, i -> #i);
	       apply(col, s -> (wid-#s, s))));
     v = prepend(leftside,v);
     v = transpose v;
     stack apply(v, concatenate))

betti ChainComplex := C -> bettiDisplay rawbetti C

-----------------------------------------------------------------------------
syzygyScheme = (C,i,v) -> (
     -- this doesn't work any more because 'resolution' replaces the presentation of a cokernel
     -- by a minimal one.  The right way to fix it is to add an option to resolution.
     g := extend(resolution cokernel transpose (C.dd_i * v), dual C[i], transpose v);
     minimalPresentation cokernel (C.dd_1  * transpose g_(i-1)))
-----------------------------------------------------------------------------
chainComplex GradedModule := ChainComplex => (M) -> (
     C := new ChainComplex from M;
     b := C.dd = new ChainComplexMap;
     b.degree = -1;
     b.source = b.target = C;
     C)
-----------------------------------------------------------------------------

tens := (R,f,g) -> map(R, rawTensor( f.RawMatrix, g.RawMatrix ))

ChainComplex ** ChainComplex := ChainComplex => (C,D) -> (
--      P := youngest(C,D);
--      key := (C,D,symbol **);
--      if P#?key then P#key
--      else C**D = 
     (
	  R := ring C;
	  if ring D =!= R then error "expected chain complexes over the same ring";
	  E := chainComplex (lookup(symbol **, GradedModule, GradedModule))(C,D);
	  scan(spots E, i -> if E#?i and E#?(i-1) then E.dd#i = map(
		    E#(i-1),
		    E#i,
		    concatBlocks(table(
			      E#(i-1).cache.indices,
			      E#i.cache.indices,
			      (j,k) -> (
				   if j#0 === k#0 and j#1 === k#1 - 1 
				   then (-1)^(k#0) * tens(R, id_(cover C#(j#0)), matrix D.dd_(k#1))
				   else if j#0 === k#0 - 1 and j#1 === k#1 
				   then tens(R, matrix C.dd_(k#0), id_(cover D#(k#1)))
				   else map(
					E#(i-1).cache.components#(E#(i-1).cache.indexComponents#j),
					E#i.cache.components#(E#i.cache.indexComponents#k),
					0))))));
	  E))

ChainComplex ** GradedModule := ChainComplex => (C,D) -> (
--      P := youngest(C,D);
--      key := (C,D,symbol **);
--      if P#?key then P#key
--      else C**D = 
     (
     	  C ** chainComplex D
	  )
     )

GradedModule ** ChainComplex := ChainComplex => (C,D) -> (
--      P := youngest(C,D);
--      key := (C,D,symbol **);
--      if P#?key then P#key
--      else C**D = 
     (
     	  chainComplex C ** D
	  )
     )

ChainComplexMap ** ChainComplexMap := ChainComplexMap => (f,g) -> (
--      P := youngest(f,g);
--      key := (f,g,symbol **);
--      if P#?key then P#key
--      else f**g = 
     (
	  h := new ChainComplexMap;
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
	  h))

ChainComplexMap ** ChainComplex := ChainComplexMap => (f,C) -> (
--      P := youngest(f,C);
--      key := (f,C,symbol **);
--      if P#?key then P#key
--      else f**C = 
     (
     	  f ** id_C
	  )
     )
ChainComplex ** ChainComplexMap := ChainComplexMap => (C,f) -> (
--      P := youngest(C,f);
--      key := (C,f,symbol **);
--      if P#?key then P#key
--      else C**f = 
     (
     	  id_C ** f
	  )
     )

min ChainComplex := C -> min spots C
max ChainComplex := C -> max spots C

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
     h.source = D;
     h.target = C;
     deg := h.degree = if options.Degree === null then 0 else options.Degree;
     scan(spots D, k -> (
	       if C#?(k+deg) then (
		    g := f(k);
		    if g =!= null and g != 0 then h#k = map(C#(k+deg),D#k,g);
		    )));
     h
     )

map(ChainComplex,ChainComplex,ChainComplexMap) := ChainComplexMap => options -> (C,D,f) -> map(C,D,k -> f_k)

map(ChainComplex,ChainComplex) := ChainComplexMap => options -> (C,D) -> (
     h := new ChainComplexMap;
     h.source = D;
     h.target = C;
     deg := h.degree = if options.Degree === null then 0 else options.Degree;
     scan(spots D, k -> if C#?(k+deg) then h#k = map(C#(k+deg),D#k));
     h
     )

kernel ChainComplexMap := ChainComplex => options -> (f) -> (
     D := source f;
     C := new ChainComplex;
     C.ring = ring f;
     complete D;
     scan(spots D, k -> C#k = kernel f_k);
     scan(spots C, k -> if C#?(k-1) then C.dd#k = (D.dd_k * map(D_k,C_k)) // map(D_(k-1),C_(k-1)));
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

minimalPresentation ChainComplex := ChainComplex => opts -> (C) -> (
     D := new ChainComplex;
     complete C;
     complete C.dd;
     D.ring = ring C;
     scan(spots C, i -> D#i = minimalPresentation C#i);
     scan(spots C.dd, i -> D.dd#i = minimalPresentation C.dd#i);
     D)

minimalPresentation ChainComplexMap := ChainComplexMap => opts -> (f) -> (
     complete f;
     map(minimalPresentation target f, minimalPresentation source f, k -> minimalPresentation f#k)
     )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
