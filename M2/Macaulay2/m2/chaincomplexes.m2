--		Copyright 1994 by Daniel R. Grayson

ChainComplex = new Type of GradedModule
new ChainComplex := (cl) -> (
     C := newClass(ChainComplex,new MutableHashTable); -- sigh
     b := C.dd = new ChainComplexMap;
     b.degree = -1;
     b.source = b.target = C;
     C)
document { quote ChainComplex,
     TT "ChainComplex", " -- the class of all chain complexes.",
     PARA,
     "A new chain complex can be made with 'C = new ChainComplex'.  This will
     automatically initialize C.dd, in which the differentials are stored.
     The modules can be installed with statements like 'C#i=M' and the 
     differentials can be installed with statements like 'C.dd#i=d'.",
     PARA,
     "See also ", TO "ChainComplexMap", " for a discussion of maps between
     chain complexes.  (The boundary map C.dd is regarded as one.)",
     PARA,
     "Here are some functions for producing or manipulating chain complexes.",
     MENU {
	  (TO "++", "           -- direct sum"),
	  (TO "ChainComplex ** Module", " -- tensor product"),
	  (TO "C_i", "          -- select a module"),
	  (TO "C.dd_i", "       -- select a differential"),
	  (TO "C[i]", "         -- shift a chain complex"),
	  (TO "HH_i C", "       -- homology of a chain complex"),
	  (TO "HH^i C", "       -- cohomology of a chain complex"),
	  (TO "Hom(C,M)", "     -- Hom complex"),
	  (TO "betti", "        -- display degrees in a free resolution"),
	  (TO "chainComplex", " -- make a chain complex"),
	  (TO "complete", "     -- complete the internal parts of a chain complex"),
	  (TO "dual", "         -- dual complex"),
	  (TO "length", "       -- length of a chain complex"),
	  (TO "poincare", "     -- assemble degrees into polynomial"),
	  (TO "poincareN", "    -- assemble degrees into polynomial"),
	  (TO "nullhomotopy", " -- produce a null homotopy"),
	  (TO "regularity", "   -- compute the regularity"),
	  (TO "resolution", "   -- make a projective resolution"),
	  (TO "status", "       -- display the status of a resolution computation"),
	  (TO "syzygyScheme", " -- construct the syzygy scheme from some syzygies")
	  },
     PARA,
     "The default display for a chain complex shows the modules and
     the stage at which they appear.",
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "C = resolution cokernel matrix {{x,y,z}}",
     "In order to see the matrices of the differentials, examine 'C.dd'.",
     EXAMPLE "C.dd",
     SEEALSO ("Resolution", "dd")
     }
document { "C_i",
     TT "C_i", " -- yields the i-th module from a chain complex C.",
     PARA,
     "Returns the zero module if no module has been stored in the
     i-th spot.",
     PARA,
     "See also ", TO "ChainComplex", " and ", TO "_", "."
     }
complete ChainComplex := C -> (
     if C.?Resolution then (i := 0; while C_i != 0 do i = i+1);
     C)
name ChainComplex := C -> (
     complete C;
     maxdeg := -10000;			  -- sigh
     scan(pairs C, (k,v) -> if class k === ZZ and k > maxdeg then maxdeg = k);
     concatenate between(" <- ",elements apply(0 .. maxdeg, i -> name C_i)))
ChainComplex _ ZZ := (C,i) -> (
     if C#?i 
     then C#i
     else if C.?Resolution then (
	  gr := C.Resolution;
	  sendgg(ggPush gr, ggPush i, ggresmodule);
	  F := new Module from ring C;
	  if F != 0 then C#i = F;
	  F)
     else (ring C)^0
     )
spots := C -> select(keys C, i -> class i === ZZ)
union := (x,y) -> keys(set x + set y)

length ChainComplex := (C) -> (
     complete C;
     s := spots C;
     if #s === 0 then 0 else max s - min s
     )

document { "C.dd_i",
     TT "C.dd_i", " -- yields the i-th differential from a chain complex C",
     PARA,
     "Returns a zero matrix if no map has been stored in the
     i-th spot.",
     PARA,
     "This differential is the one mapping C_i to C_(i-1).",
     PARA,
     "See also ", TO "ChainComplex", "."
     }
net ChainComplex := C -> if C.?name then net C.name else (
     complete C;
     s := sort spots C;
     if # s === 0 then "0"
     else (
	  a := s#0;
	  b := s#-1;
	  horizontalJoin 
	  between(" <-- ", apply(a .. b,i -> verticalJoin (net C_i,"",net i)))))
-----------------------------------------------------------------------------
ChainComplexMap = new Type of MutableHashTable
document { quote ChainComplexMap,
     TT "ChainComplexMap", " -- the class of all maps between chain complexes.",
     PARA,
     "The usual algebraic operations are available: addition, subtraction,
     scalar multiplication, and composition.  The identity map from a
     chain complex to itself can be produced with ", TO "id", ".  An
     attempt to add (subtract, or compare) a ring element to a chain complex
     will result in the ring element being multiplied by the appropriate
     identity map.",
     PARA,
     "Operations on maps of chain complexes:",
     MENU {
	  TO "!=",
	  TO "==",
	  TO "+",
	  TO "-",
	  (TO "*", "             -- composition"),
	  (TO "^", "             -- power (repeated composition)"),
	  (TO "++", "            -- direct sum"),
	  (TO "cone", "          -- mapping cone"),
	  (TO "extend", "          -- produce a map by lifting"),
	  (TO "ring", "          -- get the base ring"),
	  (TO "nullhomotopy", "  -- produce a null homotopy"),
	  (TO "p_i", "           -- get a component from a map of chain complexes")
	  }
     }
document { "p_i",
     TT "p_i", " -- for a map p : C -> D of chain complexes of degree d, provides
     the component p_i : C_i -> D_(i+d).",
     SEEALSO "ChainComplexMap"
     }
ring ChainComplexMap := C -> ring source C
complete ChainComplexMap := f -> (
     if f.?Resolution then ( i := 1; while f_i != 0 do i = i+1; );
     f)
net ChainComplexMap := f -> (
     complete f;
     v := between("",
	  apply( sort select(spots f, i -> f_i != 0),
	       i -> horizontalJoin (
		    net i, ": ", net target f_i, " <--",
		    net f_i, "-- ", net source f_i 
		    )
	       )
	  );
     if # v === 0 then "0"
     else verticalJoin v)
ring ChainComplexMap := (f) -> ring source f
ChainComplexMap _ ZZ := (f,i) -> (
     if f#?i 
     then f#i
     else if f.?Resolution then (
	  gr := f.Resolution;
	  sendgg(ggPush gr, ggPush i, ggresmap);
	  p := getMatrix ring gr;
	  if p != 0 then f#i = p;
	  p)
     else map((target f)_(i+f.degree),(source f)_i,0))
ChainComplex#id = (C) -> (
     complete C;
     f := new ChainComplexMap;
     f.source = f.target = C;
     f.degree = 0;
     scan(spots C, i -> f#i = id_(C_i));
     f)
- ChainComplexMap := f -> (
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
ChainComplexMap + RingElement := (f,r) -> (
     if source f == target f and f.degree === 0 
     then r*id_(source f) + f
     else error "expected map to have same source and target and to have degree 0")
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
ChainComplexMap ^ ZZ := (f,n) -> (
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
ChainComplexMap + ChainComplexMap := (f,g) -> (
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
ChainComplexMap - ChainComplexMap := (f,g) -> (
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
ChainComplexMap ++ ChainComplexMap := (f,g) -> (
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
     h.components = {f,g};
     h)

isDirectSum ChainComplex := (C) -> C.?components
components ChainComplexMap := f -> if f.?components then f.components else {f}
ChainComplexMap _ Array := (f,v) -> f * (source f)_v
ChainComplexMap ^ Array := (f,v) -> (target f)^v * f

ChainComplexMap * ChainComplexMap := (g,f) -> (
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

extend(ChainComplex,ChainComplex,Matrix) := (D,C,fi)-> (
     i := 0;
     j := 0;
     f := new ChainComplexMap;
     f.source = C;
     f.target = D;
     s := f.degree = j-i;
     f#i = fi;
     n := i+1;
     while C#?n do (
	  f#n = (f_(n-1) * C.dd_n) // D.dd_(n+s);
	  n = n+1;
	  );
     f)

document { quote extend,
     TT "extend(D,C,f0)", " -- produces a lifting of a map f0 : D_0 <--- C_0
     to a map f: D <--- C of chain complexes of degree 0."
     }

TEST "
R = ZZ/101[a..c]
I = image vars R
J = image symmetricPower (2,vars R)
g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))
E = cone g
"

cone ChainComplexMap := f -> (
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
document { quote cone,
     TT "cone f", " -- produce the mapping cone of a map f of chain complexes",
     PARA,
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "m = image vars R",
     EXAMPLE "m2 = image symmetricPower(2,vars R)",
     EXAMPLE "M = R^1/m2",
     EXAMPLE "N = R^1/m",
     EXAMPLE "C = cone extend(resolution N,resolution M,id_(R^1))",
     "Let's check that the homology is correct.  HH_0 should be 0.",
     EXAMPLE "prune HH_0 C",
     "HH_1 should be isomorphic to m/m2.",
     EXAMPLE "prune HH_1 C",
     EXAMPLE "prune (m/m2)"
     }
nullhomotopy ChainComplexMap := f -> (
     s := new ChainComplexMap;
     s.ring = ring f;
     s.source = C := source f;
     c := C.dd;
     s.target = D := target f;
     b := D.dd;
     deg := s.degree = f.degree + 1;
     complete f;
     scan(sort spots f, i -> (
	       si := (f_i - s_(i-1) * c_i) // b_(i+deg);
	       if si != 0 then s#i = si));
     s)
document { quote nullhomotopy,
     TT "nullhomotopy f", " -- produce a nullhomotopy for a map f of 
     chain complexes.",
     PARA,
     "Whether f is null homotopic is not checked.",
     PARA,
     "Here is part of an example provided by Luchezar Avramov.  We
     construct a random module over a complete intersection, resolve 
     it over the polynomial ring, and produce a null homotopy for the
     map which is multiplication by one of the defining equations
     for the complete intersection.",
     EXAMPLE "A = ZZ/101[x,y]",
     EXAMPLE "M = cokernel random(A^3, A^{-2,-2})",
     EXAMPLE "R = cokernel matrix {{x^3,y^4}}",
     EXAMPLE "N = prune (M**R)",
     EXAMPLE "C = resolution N",
     EXAMPLE "d = C.dd",
     EXAMPLE "s = nullhomotopy (x^3 * id_C)",
     EXAMPLE "s*d + d*s",
     EXAMPLE "s^2"
     }
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
document { quote poincare,
     TT "poincare C", " -- encodes information about the degrees of basis elements
     of a free chain complex in a polynomial.",
     BR,NOINDENT,
     TT "poincare M", " -- the same information about the free resolution
     of a module M.",
     PARA,
     "The polynomial has a term (-1)^i T_0^(d_0) ... T_(n-1)^(d_(n-1)) in it
     for each basis element of C_i with multi-degree {d_0,...,d_(n-1)}.
     When the multi-degree has a single component, the term is
     (-1)^i T^(d_0).",
     PARA,
     "The variable ", TT "T", " is defined in a hidden local scope, so will print out
     as ", TT "$T", " and not be directly accessible.",
     PARA,
     EXAMPLE "R = ZZ/101[x_0 .. x_3,y_0 .. y_3]",
     EXAMPLE "m = matrix table (2, 2, (i,j) -> x_(i+2*j))",
     EXAMPLE "n = matrix table (2, 2, (i,j) -> y_(i+2*j))",
     EXAMPLE "f = flatten (m*n - n*m)",
     EXAMPLE "poincare cokernel f",
     PARA,
     TT "(cokernel f).poincare = p", " -- inform the system that the Poincare 
     polynomial of the cokernel of f is p.  This can speed the computation 
     of a Groebner basis of f.",
     EXAMPLE "R = ZZ/101[t_0 .. t_17]",
     EXAMPLE "T = (degreesRing R)_0",
     EXAMPLE "f = genericMatrix(R,t_0,3,6)",
     EXAMPLE "(cokernel f).poincare = 3-6*T+15*T^2-20*T^3+15*T^4-6*T^5+T^6",
     EXAMPLE "gb f",
     "Keys used:",
     MENU {
	  TO "poincareComputation"
	  }
     }

poincareN = (C) -> (
     s := local S;
     t := local T;
     G := group [s, t_0 .. t_(degreeLength ring C - 1)];
     -- this stuff has to be redone as in Poincare itself, DRG
     R := ZZ G;
     use R;
     f := 0_R;
     complete C;
     scan(keys C, n -> (
	       if class n === ZZ
	       then scanPairs(tally degrees C_n, 
	       	    (d,m) -> f = f + m * G_0^n * product(# d, j -> G_(j+1)^(d_j)))));
     f )
document { quote poincareN,
     TT "poincareN C", " -- encodes information about the degrees of basis elements
     of a free chain complex in a polynomial.",
     PARA,
     "The polynomial has a term S^i T_0^(d_0) ... T_(n-1)^(d_(n-1)) in it
     for each basis element of C_i with multi-degree {d_0,...,d_(n-1)}.",
     PARA,
     EXAMPLE "R = ZZ/101[a..d]",
     EXAMPLE "poincareN resolution cokernel vars R"
     }

ChainComplex ** Module := (C,M) -> (
     D := new ChainComplex;
     D.ring = ring C;
     complete C.dd;
     scan(keys C.dd,i -> if class i === ZZ then (
	       f := D.dd#i = C.dd#i ** M;
	       D#i = source f;
	       D#(i-1) = target f;
	       ));
     D)
Module ** ChainComplex := (M,C) -> (
     D := new ChainComplex;
     D.ring = ring C;
     complete C.dd;
     scan(keys C.dd,i -> if class i === ZZ then (
	       f := D.dd#i = M ** C.dd#i;
	       D#i = source f;
	       D#(i-1) = target f;
	       ));
     D)
document { "ChainComplex ** Module",
     TT "C ** M", " -- produces the tensor product of a ", TO "ChainComplex", " C and
     a ", TO "Module", ".",
     PARA,
     TT "M ** C", " -- produces the tensor product of a ", TO "Module", " M and 
     a ", TO "ChainComplex", " C.",
     PARA,
     "See also ", TO "**", "."
     }
-----------------------------------------------------------------------------
homology(ZZ,ChainComplex) := (i,C) -> homology(C.dd_i, C.dd_(i+1))

TEST "
S = ZZ/101[x,y,z]
M = cokernel vars S
assert ( 0 == HH_-1 res M )
assert ( M == HH_0 res M )
assert ( 0 == HH_1 res M )
assert ( 0 == HH_2 res M )
assert ( 0 == HH_3 res M )
assert ( 0 == HH_4 res M )
"

document { "HH_i C",
     TT "HH_i C", " -- produce the i-th homology module of a chain complex C.",
     EXAMPLE "R = ZZ/101[x,y]",
     EXAMPLE "C = chainComplex(matrix{{x,y}},matrix{{x*y},{-x^2}})",
     EXAMPLE "M = HH_1 C",
     EXAMPLE "prune M",
     SEEALSO ( "HH", "ChainComplex", "ScriptedFunctor" )
     }

cohomology(ZZ,ChainComplex) := (i,C) -> HH_-i C

document { "HH^i C",
     TT "HH^i C", " -- produce the i-th cohomology module of a chain complex C",
     PARA,
     "This is the same as HH_(-i) C.",
     PARA,
     "See also ", TO "HH", " and ", TO "ChainComplex", "."
     }

chainComplex = method(SingleArgumentDispatch=>true)

chainComplex Sequence := chainComplex List := maps -> (
     C := new ChainComplex;
     R := C.ring = ring target maps#0;
     scan(#maps, i -> (
	       f := maps#i;
	       if R =!= ring f
	       then error "expected maps over the same ring";
	       if i > 0 and C#i != target f then (
		    diff := degrees C#i - degrees target f;
		    if same diff
		    then f = f ** R^(- diff#0)
		    else error "expected composable maps";
		    );
	       C.dd#(i+1) = f;
	       if i === 0 then C#i = target f;
	       C#(i+1) = source f;
	       ));
     C)
chainComplex Matrix := f -> chainComplex {f}
document { quote chainComplex,
     TT "chainComplex(f,g,h,...)", " -- create a chain complex whose differentials are
     the maps f, g, h, ... serving as d_0, d_1, d_2, ... respectively.",
     PARA,
     "See also ", TO "ChainComplex", "."
     }

betti Matrix := f -> (
     R := ring target f;
     f = matrix ( f ** R );
     f = map( f , Degree => 0 );
     betti chainComplex f
     )
betti GroebnerBasis := G -> betti generators G
betti Ideal := I -> (
     << "generators:" << endl;
     betti generators I;
     )
betti Module := M -> (
     if M.?relations then (
	  if M.?generators then (
	       << "generators:" << endl;
	       betti generators M;
	       << "relations:" << endl;
	       betti relations M;
	       )
	  else (
	       << "relations:" << endl;
	       betti relations M;
	       )
	  )
     else (
	  << "generators:" << endl;
	  betti generators M;
	  )
     )

ChainComplex ++ ChainComplex := (C,D) -> (
     Cd := C.dd;
     Dd := D.dd;
     E := new ChainComplex;
     Ed := E.dd;
     R := E.ring = ring C;
     if R =!= ring D then error "expected chain complexes over the same ring";
     complete C;
     complete D;
     scan(union(spots C, spots D), i -> E#i = C_i ++ D_i);
     complete Cd;
     complete Dd;
     scan(union(spots Cd, spots Dd), i -> Ed#i = Cd_i ++ Dd_i);
     E.components = {C,D};
     E)

components ChainComplex := C -> if C.?components then C.components else {C}

ChainComplex Array := (C,A) -> (
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
document { "C[i]",
     TT "C[i]", " -- shifts the chain complex C, producing a new chain complex
     D in which D_j is C_(i+j).  The signs of the differentials are reversed
     if i is odd.",
     PARA,
     "See also ", TO "ChainComplex", "."
     }

Hom(ChainComplex, Module) := (C,N) -> (
     c := C.dd;
     complete c;
     D := new ChainComplex;
     D.ring = ring C;
     b := D.dd;
     scan(keys c, i -> if class i === ZZ then (
	       j := - i + 1;
	       f := b#j = Hom(c_i,N);
	       D#j = source f;
	       D#(j-1) = target f;
	       ));
     D)

document { "Hom(C,M)",
     TT "Hom(C,M)", " -- produces the Hom complex from a chain complex C and
     a module M.",
     PARA,
     "See also ", TO "ChainComplex", "."
     }

dual ChainComplex := C -> (
     R := ring C;
     Hom(C,R^1) )

regularity ChainComplex := C -> (
     maxrow := null;
     complete C;
     scan(pairs C, (col,F) -> if class col === ZZ then (
	       degs := (transpose degrees F)#0;  -- ... fix ...
	       scanPairs(tally degs, (j,m) -> (
			 row := j-col;
			 maxrow = if maxrow === null then row else max(row,maxrow);
			 ))));
     if maxrow===null then 0 else maxrow)
regularity Module := (M) -> regularity resolution M
document { quote regularity,
     TT "regularity M", " -- computes the regularity of a module or chain complex C.",
     PARA,
     "For a free chain complex C, the regularity r is the smallest number so that 
     each basis element of C_i has degree at most i+r.  For a module M, the
     regularity is the regularity of a free minimal resolution of M."
     }

firstDegrees := method()
firstDegrees Module := M -> (
     R := ring M;
     if  M#?(quote firstDegrees) 
     then M#(quote firstDegrees)
     else M#(quote firstDegrees) = (
	  rk := numgens M;
	  nd := degreeLength R;
	  if nd == 0 then elements (rk : 0)
	  else (
	       sendgg ggPush M;
	       eePop ConvertList
	       if nd == 1 then ConvertInteger
	       else ConvertApply splice (first, nd : ConvertInteger))))

BettiNumbers := C -> (
     betti := new MutableHashTable;
     maxrow := null;
     minrow := null;
     maxcol := null;
     mincol := null;
     maxcols := new MutableHashTable;
     complete C;
     scan(pairs C, (col,F) -> if class col === ZZ then (
	       if not isFreeModule F
	       then error "expected a chain complex of free modules";
	       degs := firstDegrees F;
	       betti#("total", col) = # degs;
	       maxcol = if maxcol === null then col else max(col,maxcol);
	       mincol = if mincol === null then col else min(col,mincol);
	       scanPairs(tally degs, (j,m) -> (
			 row := j-col;
			 maxrow = if maxrow === null then row else max(row,maxrow);
			 minrow = if minrow === null then row else min(row,minrow);
			 betti#(row,col) = m + (
			      if not betti#?(row,col) then 0 else betti#(row,col)
			      );
			 maxcols#row = (
			      if not maxcols#?row then col 
			      else max(maxcols#row, col)
			      );
			 ))));
     betti#"mincol" = mincol;
     betti#"minrow" = minrow;
     betti#"maxcol" = maxcol;
     betti#"maxrow" = maxrow;
     betti)

resBetti := g -> (
    sendgg(ggPush g, ggbetti);
    eePopIntarray())

betti ChainComplex := C -> (
     if C.?Resolution then (
	  r := C.Resolution;
	  v := resBetti r;
	  minrow := v#0;
	  maxrow := v#1;
	  mincol := 0;
	  maxcol := v#2;
	  leftside := apply(
	       splice {"total:", apply(minrow .. maxrow, i -> string i | ":")},
	       s -> (10-# s,s));
	  v = drop(v,3);
	  v = pack(v,maxcol-mincol+1);
	  totals := apply(transpose v, sum);
	  v = prepend(totals,v);
	  v = transpose v;
	  -- v = applyTable(v, unlist); -- why did we have this?
	  t := 0;
	  while t < #v and sum v#(-t-1) === 0 do t = t + 1;
	  v = drop(v,-t);
	  v = applyTable(v, bt -> if bt === 0 then "." else string bt);
	  v = apply(v, col -> (
		    wid := 1 + max apply(col,i -> #i);
		    apply(col, s -> (
			      n := # s;
			      w := (wid - n + 1)//2; 
			      (w, s, wid-w-n)
			      ))));
	  v = prepend(leftside,v);
	  v = transpose v;
	  scan(v, row -> << concatenate row << endl);
	  )
     else (
	  betti := BettiNumbers C;
	  mincol = betti#"mincol";
	  minrow = betti#"minrow";
	  maxcol = betti#"maxcol";
	  maxrow = betti#"maxrow";
	  betti = hashTable apply(pairs betti,(k,v) -> (k,name v));
	  colwids := newClass(MutableList, apply(maxcol-mincol+1, i->1));
	  scan(pairs betti, (k,v) -> (
		    if class k === Sequence
		    then (
			 (row,col) -> colwids#(col-mincol) = max(colwids#(col-mincol), # v)
			 ) k 
		    ) 
	       );
	  apply(splice {"total", minrow .. maxrow},
	       row -> (
		    << pad(9,string row) << ":";
		    elements apply(mincol .. maxcol,
			 col -> << pad(
			      1+colwids#(col-mincol),
			      if not betti#?(row,col) then "." else betti#(row,col)
			      ));
		    << endl; 
		    ) 
	       ); 
	  )
     )
document { quote betti,
     TT "betti C", " -- display the graded Betti numbers for a ", TO "ChainComplex", " C.",
     PARA,
     "betti f -- display the graded Betti numbers for a ", TO "Matrix", " f,
     regarding it as a complex of length one.",
     PARA,
     "betti G -- display the graded Betti numbers for the matrix of generators
     of a ", TO "GroebnerBasis", " G.",
     PARA,
     "Here is a sample display:",
     EXAMPLE "R = ZZ/101[a..h]",
     EXAMPLE "p = genericMatrix(R,a,2,4)",
     EXAMPLE "q = generators gb p",
     EXAMPLE "C = resolution cokernel leadTerm q",
     EXAMPLE "betti C",
     "The top row of the display indicates the ranks of the free module C_j
     in column j.  The entry below in row i column j gives the number of
     basis elements of degree i+j.",
     PARA,
     "If these numbers are needed in a program, one way to get them is
     with ", TO "tally", ".",
     EXAMPLE "degrees C_2",
     EXAMPLE "t2 = tally degrees C_2",
     EXAMPLE "peek t2",
     EXAMPLE "t2_{2}",
     EXAMPLE "t2_{3}"
     }

TEST "
R = ZZ/101[a .. d,Degrees=>{1,2,3,5}]
f = vars R
C = resolution cokernel f
assert(regularity C === 7)
M = kernel f
assert( numgens source M.generators === 6 )
assert( kernel presentation kernel f === kernel presentation kernel f )

g = map(cokernel f, target f, id_(target f))
N = kernel g
assert( numgens source N.generators === 4 )
assert( kernel g == image f )
W = kernel f ++ cokernel f
P = poincare W
assert( P == poincare kernel f + poincare cokernel f )
assert( P == poincare prune W )
"

-----------------------------------------------------------------------------
pairs := g -> (
    sendgg(ggPush g, ggpairs);
    eePopIntarray())
remaining := g -> (
    sendgg(ggPush g, ggremaining);
    eePopIntarray())
nmonoms := g -> (
    sendgg(ggPush g, ggnmonoms);
    eePopIntarray())

ResolutionStatus := (r,options) -> (
     v := {};
     lab := {};
     if options#TotalPairs     === true then (
	  v = append(v,pairs r);
	  lab = append(lab,"total pairs");
	  );
     if options#PairsRemaining === true then (
	  v = append(v,remaining r);
	  lab = append(lab,"pairs remaining");
	  );
     if options#Monomials      === true then (
	  v = append(v,nmonoms r);
	  lab = append(lab,"monomials");
	  );
     numops := # v;
     if numops === 1 
     then lab = concatenate( "        : ", lab)
     else lab = concatenate( "        : (", between(",",lab), ")" );
     if numops === 0 then error "expected at least one option to be true";
     ss := v#0;
     minrow := ss_0;
     maxrow := ss_1;
     mincol := 0;
     maxcol := ss_2;
     leftside := apply(
	  splice {"totals:", apply(minrow .. maxrow, i -> string i | ":")},
	  s -> (9-# s,s));
     v = transpose v;
     v = drop(v,3);
     v = pack(v,maxcol-mincol+1);
     totals := apply(transpose v, sum);
     v = prepend(totals,v);
     v = transpose v;
     v = applyTable(v, unlist);
     if numops === 1
     then v = applyTable(v,(i) -> if i === 0 then "." else name i)
     else v = applyTable(v,args -> concatenate("(", between(",",apply(args,name)), ")" ));
     just := (
	  if numops === 1
	  then (wid,s) -> (wid - # s, s)  -- right justify
	  else (wid,s) -> (			  -- center
	       n := # s;
	       w := (wid - n + 1)//2; 
	       (w, s, wid-w-n)));
     v = apply(v, col -> apply(col, s -> just(1 + max apply(col, i -> #i), s)));
     v = prepend(leftside,v);
     v = transpose v;
     v = apply(v, row -> (row,"\n"));
     << lab << endl;
     printString(stdout,v);
     )
statusDefaults := new OptionTable from {
     TotalPairs => true,
     PairsRemaining => false,
     Monomials => false
     }
status = method (Options => statusDefaults)
status Resolution := (r,options) -> ResolutionStatus(r, options)
status ChainComplex := (C,options) -> ResolutionStatus(C.Resolution, options)
document { quote status,
     TT "status C", " -- displays the status of the computation of a
     chain complex C constructed by ", TO "resolution", ".  The display has
     the same shape as the display produced by ", TO "betti", ", but
     the number(s) displayed in each degree differ.",
     PARA,
     "Options:",
     MENU {
	  {TO "TotalPairs", "     -- display the total number of S-pairs"},
	  {TO "PairsRemaining", " -- display the number of S-pairs remaining"},
	  {TO "Monomials", "      -- display the number of monomials"}
	  },
     "Default values:",
          MENU {
	  {TO "TotalPairs", "     => ", statusDefaults.TotalPairs},
	  {TO "PairsRemaining", " => ", statusDefaults.PairsRemaining},
	  {TO "Monomials", "      => ", statusDefaults.Monomials}
	  }
     }
document { quote TotalPairs,
     TT "TotalPairs", " -- an option for ", TO "status", " which specifies
     whether to display the total number of S-pairs."
     }
document { quote PairsRemaining,
     TT "PairsRemaining", " -- an option for ", TO "status", " which specifies
     whether to display number of S-pairs remaining."
     }
document { quote Monomials,
     TT "Monomials", " -- an option for ", TO "status", " which specifies
     whether to display the number of monomials."
     }
-----------------------------------------------------------------------------
syzygyScheme = (C,i,v) -> (
     -- this doesn't work any more because 'resolution' replaces the presentation of a cokernel
     -- by a minimal one.  The right way to fix it is to add an option to resolution.
     g := extend(resolution cokernel transpose (C.dd_i * v), dual C[i], transpose v);
     prune cokernel (C.dd_1  * transpose g_(i-1)))
document { quote syzygyScheme,
     TT "syzygyScheme(C,i,v)", " -- produce the syzygy scheme from a map
     v : R^j ---> C_i which selects some syzygies from a resolution C."
     }
