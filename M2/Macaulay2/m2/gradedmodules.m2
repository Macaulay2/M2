--		Copyright 1997-2002 by Daniel R. Grayson

needs "modules.m2"
needs "ringmap.m2"

GradedModule = new Type of MutableHashTable
GradedModule.synonym = "graded module"
ring GradedModule := (M) -> M.ring

spots  = C -> select(keys C, i -> class i === ZZ)
union := (x,y) -> keys(set x + set y)
intersection := (x,y) -> keys(set x * set y)

min GradedModule := C -> min spots complete C
max GradedModule := C -> max spots complete C

GradedModule == GradedModule := (C,D) -> (
     ring C === ring D
     and
     all( union(spots C, spots D), i -> (
	       C#?i and D#?i and C#i == D#i or
	       not D#?i and C#i == 0 or
	       not C#?i and D#i == 0
	       )))
GradedModule _ ZZ := Module => (M,i) -> if M#?i then M#i else (ring M)^0
net GradedModule := C -> (
     s := sort spots C;
     if # s === 0 then "0"
     else (
	  ind := apply(s,toString);
	  sep := " : ";
	  wid := max apply(ind,length) + length sep;
	  savePW := printWidth;
	  printWidth = printWidth - wid;
	  tr := M -> if printWidth > 6 then wrap net M else net M;
	  res := netList( 
	       apply(s, i -> {i, sep, tr C_i}),
	       Boxes =>false, 
	       Alignment => {Right,Center,Left}, 
	       VerticalSpace => 1);
	  printWidth = savePW;
	  res))
  
texUnder = (x,y) -> "\\underset{\\vphantom{\\Big|}"|y|"}{"|x|"}"

texMath GradedModule := C -> (
     s := sort spots C;
     if # s === 0 then "0"
     else demark("\\quad ",apply(s,i->texUnder(texMath C_i,i)))
      )

length GradedModule := (M) -> (
     s := spots M;
     if #s === 0 then 0 else max s - min s)
GradedModuleMap = new Type of MutableHashTable
GradedModuleMap.synonym = "graded module map"

source GradedModuleMap := GradedModule => f -> f.source
target GradedModuleMap := GradedModule => f -> f.target
ring GradedModuleMap := (f) -> ring source f
net GradedModuleMap := f -> (  -- net GradedModule & net ChainComplexMap are essentially identical...
     d := f.degree;
     v := between("",
	  apply( sort toList (
		    set spots f +
		    set spots source f +
		    set (apply(spots target f, i -> i-d))
		    ),
	       i -> horizontalJoin (
		    net (i+d), ": ", net target f_i, " <--",
		    net f_i, "-- ", net source f_i, " :", net i
		    )
	       )
	  );
     if # v === 0 then "0"
     else stack v)

texMath GradedModuleMap := f -> (
     d := f.degree;
     s := sort intersection(spots f.source, spots f.target / (i -> i - d));
     texMath if #s === 0 then ZERO else new VerticalList from apply(s,i-> expression(i+d) : MapExpression { target f_i, source f_i, f_i } : expression i)
)


GradedModuleMap _ ZZ := Matrix => (f,i) -> (
     if f#?i then f#i else map((target f)_(i+f.degree),(source f)_i,0)
     )
GradedModule#id = GradedModuleMap => (M) -> (
     f := new GradedModuleMap;
     f.ring = M.ring;
     f.source = f.target = M;
     f.degree = 0;
     scan(spots M, i -> f#i = id_(M_i));
     f
     )
- GradedModuleMap := GradedModuleMap => f -> (
     g := new GradedModuleMap;
     g.source = f.source;
     g.target = f.target;
     g.degree = f.degree;
     g.ring = ring f;
     scan(spots f, i -> g#i = -f_i);
     g)
RingElement + GradedModuleMap := GradedModuleMap => (r,f) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) + f
     else error "expected map to have same source and target and to have degree 0")
GradedModuleMap + RingElement := GradedModuleMap => (f,r) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) + f
     else error "expected map to have same source and target and to have degree 0")
RingElement - GradedModuleMap := GradedModuleMap => (r,f) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) - f
     else error "expected map to have same source and target and to have degree 0")
GradedModuleMap - RingElement := GradedModuleMap => (f,r) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) - f
     else error "expected map to have same source and target and to have degree 0")
RingElement == GradedModuleMap := (r,f) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) == f
     else error "expected map to have same source and target and to have degree 0")
GradedModuleMap == RingElement := (f,r) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) == f
     else error "expected map to have same source and target and to have degree 0")
RingElement * GradedModuleMap := GradedModuleMap => (r,f) -> (
     g := new GradedModuleMap;
     g.ring = ring f;
     g.source = f.source;
     g.target = f.target;
     g.degree = f.degree;
     scan(spots f, i -> g#i = r * f_i);
     g)
ZZ * GradedModuleMap := GradedModuleMap => (n,f) -> (
     g := new GradedModuleMap;
     g.ring = ring f;
     g.source = f.source;
     g.target = f.target;
     g.degree = f.degree;
     scan(spots f, i -> g#i = n * f_i);
     g)
GradedModuleMap ^ ZZ := GradedModuleMap => (f,n) -> (
     if n === -1 then (
	  h := new GradedModuleMap;
	  h.ring = ring f;
	  h.source = f.target;
	  h.target = f.source;
	  d := f.degree;
	  h.degree = - d;
	  scan(spots f, i -> h#(i+d) = f#i^-1);
	  h
	  )
     else if n < 0 then f^-1^-n
     else if n === 0 then id_(source f)
     else if n === 1 then f
     else (
     	  if source f != target f then error "expected source and target to be the same";
	  g := new GradedModuleMap;
	  g.ring = ring f;
	  C := g.source = f.source;
	  g.target = f.target;
	  d = g.degree = n * f.degree;
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
GradedModuleMap + GradedModuleMap := GradedModuleMap => (f,g) -> (
     if source f != source g
     or target f != target g
     or f.degree != g.degree then (
	  error "expected maps of the same degree with the same source and target";
	  );
     h := new GradedModuleMap;
     h.ring = ring f;
     h.source = f.source;
     h.target = f.target;
     h.degree = f.degree;
     scan(union(spots f, spots g), i -> h#i = f_i + g_i);
     h)
GradedModuleMap - GradedModuleMap := GradedModuleMap => (f,g) -> (
     if source f != source g
     or target f != target g
     or f.degree != g.degree then (
	  error "expected maps of the same degree with the same source and target";
	  );
     h := new GradedModuleMap;
     h.ring = ring f;
     h.source = f.source;
     h.target = f.target;
     h.degree = f.degree;
     scan(union(spots f, spots g), i -> h#i = f_i - g_i);
     h)
GradedModuleMap == GradedModuleMap := (f,g) -> (
     if source f != source g
     or target f != target g
     or f.degree != g.degree then (
	  error "expected maps of the same degree with the same source and target";
	  );
     all(union(spots f, spots g), i -> f_i == g_i))
GradedModuleMap == ZZ := (f,i) -> (
     if i === 0 then all(spots f, j -> f_j == 0)
     else source f == target f and f == i id_(source f))
ZZ == GradedModuleMap := (i,f) -> f == i

degree GradedModuleMap := G -> G.degree

formation GradedModule := M -> if M.cache.?formation then M.cache.formation

directSum GradedModule := GradedModule => M -> directSum(1 : M)
GradedModule.directSum = v -> (
     E := new GradedModule;
     rings := apply(v, ring);
     if not same rings
     then error "expected graded module maps in matrix to have the same ring";
     E.ring = rings#0;
     spts := new MutableHashTable;
     scan(v, M -> scan(spots M, i -> spts#i = 1));
     spts = keys spts;
     scan(spts, i -> E#i = directSum apply(v, M -> M_i));
     if not E.?cache then E.cache = new CacheTable;
     E.cache.components = v;
     E.cache.formation = FunctionApplication { directSum, v };
     E)

formation GradedModuleMap := M -> if M.cache.?formation then M.cache.formation

GradedModuleMap ++ GradedModuleMap := GradedModuleMap => directSum

GradedModuleMap.directSum = args -> (
     R := ring args#0;
     if not all(args, f -> ring f === R)
     then error "expected graded module maps all over the same ring";
     d := degree args#0;
     if not all(args, f -> degree f == d)
     then error "expected graded module maps all of the same degree";
     M := directSum apply(args, source);
     N := directSum apply(args, target);
     g := map(directSum apply(args, target), directSum apply(args, source), 
	  j -> directSum apply(args, f -> f_j), Degree => d);
     g.cache = new CacheTable;
     if not g.?cache then g.cache = new CacheTable;
     g.cache.components = toList args;
     g.cache.formation = FunctionApplication { directSum, args };
     g
     )

isDirectSum GradedModule := (M) -> M.cache.?components
components GradedModuleMap := f -> if f.cache.?components then f.cache.components else {f}
GradedModuleMap _ Array := GradedModuleMap => (f,v) -> f * (source f)_v
GradedModuleMap ^ Array := GradedModuleMap => (f,v) -> (target f)^v * f

GradedModuleMap | GradedModuleMap := (f,g) -> (
     if target f != target g then error "expected maps with the same target";
     if degree f != degree g then error "expected maps with the same degree";
     d := degree f;
     map(target f, source f ++ source g, j -> f_j | g_j, Degree => d))

GradedModuleMap || GradedModuleMap := (f,g) -> (
     if source f != source g then error "expected maps with the same source";
     if degree f != degree g then error "expected maps with the same degree";
     d := degree f;
     map(target f ++ target g, source f, j -> f_j || g_j, Degree => d))

GradedModuleMap * GradedModuleMap := GradedModuleMap => (g,f) -> (
     if target f != source g then error "expected composable maps of graded modules";
     h := new GradedModuleMap;
     h.ring = ring f;
     h.source = source f;
     h.target = target g;
     h.degree = f.degree + g.degree;
     scan(union(spots f, apply(spots g, i -> i - f.degree)),
	  i -> h#i = g_(i+f.degree) * f_i);
     h)
GradedModule ** Module := GradedModule => (C,M) -> (
     D := new GradedModule;
     D.ring = C.ring;
     scan(spots C, i -> D#i = C#i ** M);
     D)
Module ** GradedModule := GradedModule => (M,C) -> (
     D := new GradedModule;
     D.ring = C.ring;
     scan(spots C, i -> D#i = M ** C#i);
     D)

gradedModule = method(Dispatch => Thing)

gradedModule Sequence := gradedModule List := GradedModule => modules -> (
     C := new GradedModule;
     R := C.ring = ring modules#0;
     scan(#modules, i -> (
	       M := modules#i;
	       if R =!= ring M
	       then error "expected modules over the same ring";
	       C#i = M;
	       ));
     C)
gradedModule Module := GradedModule => M -> gradedModule (1:M)

GradedModule ++ GradedModule := GradedModule => directSum

GradedModule ++ Module := GradedModule => (C,M) -> C ++ gradedModule M
Module ++ GradedModule := GradedModule => (M,C) -> gradedModule M ++ C

components GradedModule := C -> if C.cache.?components then C.cache.components else {C}

GradedModule Array := GradedModule => (C,A) -> (
     if # A =!= 1 then error "expected array of length 1";
     n := A#0;
     D := new GradedModule;
     D.ring = C.ring;
     scan(spots C, i -> D#(i-n) = C#i);
     D)

GradedModule ** GradedModule := GradedModule => (C,D) -> (
     R := C.ring;
     if R =!= D.ring then error "expected graded modules over the same ring";
     c := spots C;
     d := spots D;
     pairs := new MutableHashTable;
     scan(c, i -> scan(d, j -> (
		    k := i+j;
		    p := if not pairs#?k then pairs#k = new MutableHashTable else pairs#k;
		    p#(i,j) = 1;
		    )));
     scan(keys pairs, k -> pairs#k = sort keys pairs#k);
     E := new GradedModule;
     E.ring = R;
     scan(keys pairs, k -> (
	       p := pairs#k;
	       E#k = directSum apply(p, v -> v => C#(v#0) ** D#(v#1));
	       ));
     E)

gradedModuleMap = method(Dispatch => Thing)

gradedModuleMap Sequence := gradedModuleMap List := GradedModuleMap => maps -> (
     if #maps === 0 then error "expected at least one argument";
     f := new GradedModuleMap;
     R := f.ring = ring maps#0;
     scan(#maps, i -> (
	       g := maps#i;
	       if R =!= ring g
	       then error "expected modules over the same ring";
	       f#i = g;
	       ));
     f.source = gradedModule(source \ maps);
     f.target = gradedModule(target \ maps);
     f.degree = 0;
     f)
gradedModuleMap Matrix := GradedModuleMap => M -> gradedModuleMap (1:M)

single := (v) -> (
     if not same v 
     then error "incompatible sources or targets in graded module maps in matrix";
     v#0)

GradedModuleMap.matrix = options -> (e) -> (
     nrows := #e;
     ncols := #(e#0);
     tars := apply(          e, row -> single apply(row,source));
     srcs := apply(transpose e, col -> single apply(col,source));
     R := single apply(join(tars,srcs), ring);
     f := new GradedModuleMap;
     f.ring = R;
     f.degree = 0;
     src := f.source = directSum srcs;
     tar := f.target = directSum tars;
     scan(toList(set spots src * set spots tar), k -> (
	       f#k = matrix apply(nrows, i -> apply(ncols, j -> (
			      if e#i#j#?k then e#i#j#k else map(tars#i_k,srcs#j_k,0)
			      )));
	       ));
     f
     )

kernel GradedModuleMap := GradedModule => options -> (f) -> (
     E := new GradedModule;
     E.ring = ring f;
     scan(spots source f, i -> E#i = kernel f_i);
     E
     )

image GradedModuleMap := GradedModule => (f) -> (
     E := new GradedModule;
     E.ring = ring f;
     d := f.degree;
     scan(spots f, i -> E#(i+d) = image f#i);
     E
     )

coimage GradedModuleMap := GradedModule => (f) -> (
     E := new GradedModule;
     E.ring = ring f;
     scan(spots f, i -> E#i = coimage f#i);
     E
     )

cokernel GradedModuleMap := GradedModule => (f) -> (
     E := new GradedModule;
     E.ring = ring f;
     d := f.degree;
     scan(spots f, i -> E#(i+d) = cokernel f#i);
     E
     )

cover GradedModule := GradedModule => (M) -> (
     E := new GradedModule;
     E.ring = M.ring;
     scan(spots M, i -> E#i = cover M#i);
     E)

ambient GradedModule := GradedModule => (M) -> (
     E := new GradedModule;
     E.ring = M.ring;
     scan(spots M, i -> E#i = ambient M#i);
     E)

super GradedModule := GradedModule => (M) -> (
     E := new GradedModule;
     E.ring = M.ring;
     scan(spots M, i -> E#i = super M#i);
     E)

minimalPresentation GradedModule := prune GradedModule := GradedModule => opts -> (M) -> (
     E := new GradedModule;
     E.ring = M.ring;
     scan(spots M, i -> E#i = minimalPresentation M#i);
     E)

minimalPresentation GradedModuleMap := prune GradedModuleMap := GradedModuleMap => opts -> f -> 
  map(minimalPresentation(f.target), 
      minimalPresentation(f.source), 
      k -> minimalPresentation f_k,
      Degree => f.degree)

complete GradedModule := (M) -> M
rank GradedModule := (M) -> sum(spots M, i -> (-1)^i * rank M#i)

map(GradedModule,GradedModule,Function) := GradedModuleMap => options -> (C,D,f) -> (
     h := new GradedModuleMap;
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

tensorAssociativity = method()
tensorAssociativity(GradedModule,GradedModule,GradedModule) := GradedModuleMap => (A,B,C) -> (
     R := ring A;
     map(
	  F := (AB := A ** B) ** C,
	  E :=  A ** (BC := B ** C),
	  k -> ggConcatBlocks(R, apply(F_k.indices, (ab,c) -> (
			 apply(E_k.indices, (a,bc) -> (
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
