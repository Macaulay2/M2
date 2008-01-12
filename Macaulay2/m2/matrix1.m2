--		Copyright 1993-2002 by Daniel R. Grayson

module Ring := Module => (stashValue symbol module) (R -> R^1)

matrix(Ring,List) := Matrix => options -> (R,m) -> (
     m = apply(splice m,splice);
     if not isTable m then error "expected a table";
     map(R^#m,,m,options))

map(Module,Module,Function) := Matrix => options -> (M,N,f) -> (
     map(M,N,table(numgens M, numgens N, f))
     )

map(Matrix) := Matrix => options -> (f) -> (
     if options.Degree === null then f
     else (
     	  R := ring source f;
	  d := options.Degree;
	  if class d === ZZ then d = {d};
     	  map(target f, source f ** R^{d - degree f}, f, options)))

map(Module,ZZ,Function) := Matrix => options -> (M,n,f) -> map(M,n,table(numgens M,n,f),options)

makeRawTable := (R,p) -> (					    -- this is messy
     if R === ZZ then (
	  applyTable(p,
	       x -> (
		    if class x === ZZ then rawFromNumber(ZZ.RawRing, x)
		    else error "expected an integer"
		    )))
     else if R === QQ then (
	  applyTable(p,x -> (
		    if class x === ZZ then rawFromNumber(QQ.RawRing, x)
		    else if class x === QQ then raw x
		    else error "expected an integer or a rational number")))
     else applyTable(p,x -> raw promote(x,R)))

map(Module,Nothing,Matrix) := Matrix => o -> (M,nothing,p) -> (
     if o.Degree =!= null then error "Degree option given with indeterminate source module";
     R := ring M;
     if ring p =!= R then error "expected matrix and new target module to have the same ring";
     if M.?generators then (
	  M' := source M.generators;
	  if numgens M' != numgens target p then error "expected matrix and new target module to agree";
     	  f := rawMatrixRemake1(raw M', raw p,0);
     	  map(M, new Module from (R, rawSource f), f)
	  )
     else (
	  if numgens M != numgens target p then error "expected matrix and new target module to agree";
     	  f = rawMatrixRemake1(raw M, raw p,0);
     	  map(M, new Module from (R, rawSource f), f)
	  )
     )

degreeCheck := (d,R) -> (
     if class d === ZZ then d = {d};
     if class d === List or class d === Sequence
     and all(d,i -> class i === ZZ) 
     and #d === degreeLength R
     then (
	  )
     else (
	  if degreeLength R === 1
	  then error "expected degree to be an integer or list of integers of length 1"
	  else error (
	       "expected degree to be a list of integers of length ",
	       toString degreeLength R
	       )
	  );
     toSequence d)

map(Module,Module,Matrix) := Matrix => o -> (M,N,f) -> (
     if M === f.target and N === f.source
     and (o.Degree === null or o.Degree === degree f)
     then f
     else (
	  R := ring M;
	  N' := cover N ** R;
	  deg := if o.Degree === null then (degreeLength R : 0) else o.Degree;
	  deg = degreeCheck(deg,R);
	  map(M,N,reduce(M,rawMatrixRemake2(raw cover M, raw N', deg, raw f,0)))))

-- combine the one above with the one below
map(Module,ZZ,List) := 
map(Module,Nothing,List) := 
map(Module,Module,List) := Matrix => options -> (M,N,p) -> (
     R := ring M;
     local rankN;
     local k;
     if N === null then (
	  k = R;
	  if #p === 0 then error "expected non-empty list of entries for matrix";
	  rankN = #p#0;
	  )
     else if class N === ZZ then (
	  k = R;
	  rankN = N;
	  )
     else (
	  assert( class N === Module );
     	  k = ring N;
     	  try promote(1_k,R) else error "modules over incompatible rings";
	  -- later, allow a ring homomorphism
	  rankN = numgens N;
	  );
     if not instance(N,Module) and options.Degree =!= null then error "Degree option given with indeterminate source module";
     deg := if options.Degree === null then toList ((degreeLength R):0) else options.Degree;
     deg = degreeCheck(deg,R);
     p = splice p;
     if all(p, o -> (
	       instance(o,Option)
	       and #o == 2
	       and class o#1 === R
	       and class o#0 === Sequence
	       and #o#0 == 2
	       and class o#0#0 === ZZ
	       and class o#0#1 === ZZ
	       )
	  ) then (		    -- sparse list of entries
     	  rows := apply(p, o -> o#0#0);
	  cols := apply(p, o -> o#0#1);
	  ents := toSequence apply(p, o -> raw o#1);
	  pref := 0;					    -- ???
	  m := (
	       if class N === Module
	       then rawSparseMatrix2(raw cover M, raw cover N, deg, rows, cols, ents, pref)
	       else rawSparseMatrix1(raw cover M, rankN, rows, cols, ents, pref)
	       );
	  new Matrix from {
	       symbol target => M,
	       symbol RawMatrix => m,
	       symbol source => if class N === Module then N else new Module from (R, rawSource m),
	       symbol ring => R,
	       symbol cache => new CacheTable
	       })
     else if all(p, o -> instance(o,List)) then (		    -- dense list of entries or blocks
	  p = apply(splice p,splice);
	  if #p != numgens M or #p > 0 and ( not isTable p or # p#0 != rankN )
	  then error( "expected ", toString numgens M, " by ", toString rankN, " table");
	  p = toSequence makeRawTable(R,p);
	  h := (
	       if class N === Module
	       then rawMatrix2(raw cover M, raw cover N, deg, flatten p,0)
	       else rawMatrix1(raw cover M, rankN, flatten p, 0)
	       );
	  new Matrix from {
	       symbol target => M,
	       symbol RawMatrix => h,
	       symbol source => if class N === Module then N else new Module from (R, rawSource h),
	       symbol ring => R,
	       symbol cache => new CacheTable
	       })
     else error "expected a list of lists or a list of options (i,j)=>r")

fixDegree := (m,d) -> (
     M := target m;
     N := source m;
     map(M,N,rawMatrixRemake2(raw cover M, raw cover N, degreeCheck(d,ring m), raw m)))

concatBlocks = mats -> (
     if not isTable mats then error "expected a table of matrices";
     if #mats === 1
     then concatCols mats#0
     else if #(mats#0) === 1
     then concatRows (mats/first)
     else (
     	  samering flatten mats;
	  sources := applyTable(mats,source);
	  targets := transpose applyTable(mats,target);
	  -- if not same sources then error "expected matrices in the same column to have equal sources";
	  -- if not same targets then error "expected matrices in the same row to have equal targets";
     	  ggConcatBlocks( Module.directSum first targets, Module.directSum first sources, mats)))

Matrix.matrix = options -> (f) -> concatBlocks f

matrixTable := options -> (f) -> (
     types := unique apply(flatten f, class);
     if # types === 1 then (
	  type := types#0;
	  if instance(type,Ring) then (
	       R := type;
	       map(R^#f,, f, options))
	  else if instance(type,InexactFieldFamily) then (
	       rings := unique apply(flatten f, ring);
	       if # rings === 1 then (
		    R = rings#0;
		    map(R^#f,, f, options))
	       else (
		    prec := min apply(flatten f, precision);
		    f = applyTable(f, numeric_prec);
		    R = ring f#0#0;
		    map(R^#f,, f, options)))
	  else if type.?matrix then (type.matrix options)(f)
	  else error "no method for forming a matrix from elements of this type")
     else if all(types, T -> instance(T,Ring) or instance(T,InexactFieldFamily)) then (
	  R = ring (
	       try sum apply(types, R -> 0_R)
	       else error "couldn't put matrix elements into the same ring"
	       );
	  map(R^#f,,f,options))
     else if all(types, T -> instance(T,Ring) or T === Matrix) then (
	  rings = unique apply(select(flatten f,m -> class m === Matrix), ring);
	  if #rings > 1 then error "matrices over different rings";
	  R = rings#0;
	  f = apply(f, row -> new MutableList from row);
	  m := #f;
	  n := #f#0;
	  tars := new MutableHashTable;
	  srcs := new MutableHashTable;
	  scan(m, row -> scan(n, col-> (
			 r := f#row#col;
			 if instance(r, Matrix) then (
			      if tars#?row then (
				   if tars#row != target r then error "expected matrices in the same row to have equal targets";
				   )
			      else tars#row = target r;
			      if srcs#?col then (
				   if srcs#col != source r then error "expected matrices in the same column to have equal sources";
				   )
			      else srcs#col = source r;
			      ))));
	  scan(m, i->scan(n, j-> (
			 r := f#i#j;
			 if instance(class r,Ring) and r != 0 then (
			      r = R#0 + r;
			      d := degree r;
			      if tars#?i then (
				   M := tars#i;
				   if srcs#?j then (
					N := srcs#j;
					if apply(degrees M, e -> e + d) =!= degrees N 
					then error ("matrices not compatible");
					f#i#j = map(M,N,r))
				   else (
					srcs#j = N = M ** R^{-d};
					f#i#j = map(M,N,r)))
			      else (
				   if srcs#?j then (
					N = srcs#j;
					tars#i = M = N ** R^{d};
					f#i#j = map(M,N,r))
				   else (
					tars#i = M = R^1;
					srcs#j = N = R^{-d};
					f#i#j = map(M,N,r)))))));
	  scan(m, i->scan(n, j-> (
			 r := f#i#j;
			 if r == 0 then (
			      if tars#?i then (
				   M := tars#i;
				   if srcs#?j then (
					N := srcs#j;
					f#i#j = map(M,N,0);)
				   else (
					srcs#j = M;
					f#i#j = map(M,M,0); ) )
			      else (
				   if srcs#?j then (
					N = srcs#j;
					tars#i = N;
					f#i#j = map(N,N,0);
					)
				   else (
					M = tars#i = srcs#j = R^1;
					f#i#j = map(M,M,0);
					))))));
	  f = toList \ f;
	  mm := concatBlocks f;
	  if options.Degree === null
	  then mm
	  else fixDegree(mm,options.Degree)
	  )
     else (error "expected ring elements or matrices";))

matrix(Matrix) := Matrix => options -> (m) -> (
     if isFreeModule target m and isFreeModule source m
     and ring source m === ring target m
     then m
     else map(cover target m, cover source m ** ring target m, m, Degree => degree m)
     )

matrix(List) := Matrix => options -> (m) -> (
     if #m === 0 then error "expected nonempty list";
     m = apply(splice m,splice);
     types := unique apply(m,class);
     if #types === 1 then (
	  type := types#0;
	  if instance(type,Module) then matrix { apply(m, v -> new Matrix from v) }
	  else if type === List then (
	       if isTable m then (matrixTable options)(m)
	       else error "expected rows all to be the same length"
	       )
	  else error "expected a table of ring elements or matrices, or a list of elements of the same module")
     else error "expected a table of ring elements or matrices, or a list of elements of the same module")

--------------------------------------------------------------------------

Module#id = (M) -> map(M,M,1)

reshape = method()
reshape(Module,Module,Matrix) := Matrix => (F, G, m) -> (
     if not isFreeModule F or not isFreeModule G
     then error "expected source and target to be free modules";
     map(F,G,rawReshape(raw m, raw F, raw G)))

-- adjoint1:  m : F --> G ** H ===> F ** dual G --> H
-- adjoint:   m : F ** G --> H ===> F --> dual G ** H
adjoint1 = method()
adjoint1(Matrix,Module,Module) := Matrix => (m,G,H) -> reshape(H, (source m) ** (dual G), m)
adjoint  = method()
adjoint (Matrix,Module,Module) := Matrix => (m,F,G) -> reshape((dual G) ** (target m), F, m)

flatten Matrix := Matrix => m -> (
     R := ring m;
     F := target m;
     G := source m;
     if not isFreeModule F or not isFreeModule G
     then error "expected source and target to be free modules";
     if numgens F === 1 
     then m
     else reshape(R^1, G ** dual F ** R^{- degree m}, m))

flip = method()
flip(Module,Module) := Matrix => (F,G) -> map(ring F,rawFlip(raw F, raw G))

align := f -> (
     if isHomogeneous f and any(degree f, i -> i =!= 0) then map(target f,,f) else f
     )

subquotient = method(TypicalValue => Module)
subquotient(Nothing,Matrix) := (null,relns) -> (
     R := ring relns;
     E := target relns;
     rE := E.RawFreeModule;
     Mparts := {
	  symbol cache => new CacheTable,
	  symbol RawFreeModule => rE,
	  symbol ring => R,
	  symbol numgens => rawRank rE
	  };
     relns = align matrix relns;
     if E.?generators then (
	  Mparts = append(Mparts, symbol generators => E.generators);
	  relns = E.generators * relns;
	  );
     if E.?relations then relns = relns | E.relations;
     if relns != 0 then (
	  Mparts = append(Mparts, symbol relations => relns);
	  );
     new Module of Vector from hashTable Mparts)
subquotient(Matrix,Nothing) := (subgens,null) -> (
     R := ring subgens;
     E := target subgens;
     rE := E.RawFreeModule;
     subgens = align matrix subgens;
     if E.?generators then subgens = E.generators * subgens;
     Mparts := {
	  symbol cache => new CacheTable,
	  symbol RawFreeModule => rE,
	  symbol ring => R,
	  symbol numgens => rawRank rE,
     	  symbol generators => subgens
	  };
     if E.?relations then (
	  Mparts = append(Mparts, symbol relations => E.relations);
	  );
     new Module of Vector from hashTable Mparts)
subquotient(Matrix,Matrix) := (subgens,relns) -> (
     R := ring relns;
     E := target subgens;
     if E != target relns then error "expected maps with the same target"; -- we used to have =!=, but Schreyer orderings of free modules are discarded by "syz"
     rE := E.RawFreeModule;
     n := rawRank rE;
     if n == 0 then new Module from (R,rE)
     else (
	  relns = align matrix relns;
	  subgens = align matrix subgens;
	  if E.?generators then (
	       relns = E.generators * relns;
	       subgens = E.generators * subgens;
	       );
	  if E.?relations then relns = relns | E.relations;
	  Mparts := {
	       symbol cache => new CacheTable,
	       symbol RawFreeModule => rE,
	       symbol ring => R,
	       symbol numgens => rawRank rE,
	       symbol generators => subgens
	       };
	  if relns != 0 then (
	       Mparts = append(Mparts, symbol relations => relns);
	       );
	  new Module of Vector from hashTable Mparts))

subquotient(Module,Matrix,Matrix) := (F,g,r) -> (
     if F =!= target g or F =!= target r then error "expected module to be target of maps";
     subquotient(g,r))
subquotient(Module,Nothing,Matrix) := (F,g,r) -> (
     if F =!= target r then error "expected module to be target of maps";
     subquotient(g,r))
subquotient(Module,Matrix,Nothing) := (F,g,r) -> (
     if F =!= target g then error "expected module to be target of maps";
     subquotient(g,r))
subquotient(Module,Nothing,Nothing) := (F,g,r) -> F

Matrix ** Matrix := Matrix => (f,g) -> (
     R := ring target f;
     if ring target g =!= R or ring source g =!= ring source f
     then error "expected matrices over the same ring";
     map(target f ** target g, 
	  source f ** source g, 
	  map(R, f.RawMatrix ** g.RawMatrix),
	  Degree => degree f + degree g))

Matrix ** Number := (f,r) -> r * f
Number ** Matrix := (r,f) -> r * f
Matrix ** RingElement := (f,r) -> f ** matrix {{r}}
RingElement ** Matrix := (r,f) -> matrix {{r}} ** f

Number ** RingElement := 
RingElement ** Number := 
RingElement ** RingElement := (r,s) -> matrix {{r}} ** matrix {{s}}

Matrix#{Standard,AfterPrint} = 
Matrix#{Standard,AfterNoPrint} = f -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : Matrix";
     if isFreeModule target f and isFreeModule source f
     then << " " << target f << " <--- " << source f;
     << endl;
     )

-- precedence Matrix := x -> precedence symbol x

compactMatrixForm = true

net Matrix := f -> (
     if f == 0 
     then "0"
     else (
	  m := (
	       if compactMatrixForm then (
	       	    stack toSequence apply(lines toString f.RawMatrix, x -> concatenate("| ",x,"|"))
	       	    )
     	       else net expression f
	       );
	  if compactMatrixForm and  degreeLength ring f > 0 -- and isHomogeneous f
	  then (
	       d := degrees cover target f;
	       if not all(d, i -> all(i, j -> j == 0)) then m = horizontalJoin(stack( d / toString ), " ", m);
	       );
	  m)
     )

image Matrix := Module => f -> (
     if f.cache.?image then f.cache.image else f.cache.image = subquotient(f,)
     )
coimage Matrix := Module => f -> (
     if f.cache.?coimage then f.cache.coimage else f.cache.coimage = cokernel inducedMap(source f, kernel f)
     )
cokernel Matrix := Module => m -> (
     if m.cache.?cokernel then m.cache.cokernel else m.cache.cokernel = subquotient(,m)
     )

cokernel RingElement := Module => f -> cokernel matrix {{f}}
image RingElement := Module => f -> image matrix {{f}}

Ideal = new Type of HashTable
Ideal.synonym = "ideal"

ideal = method(Dispatch => Thing, TypicalValue => Ideal)

expression Ideal := (I) -> new FunctionApplication from { ideal, expression unsequence toSequence first entries generators I }

net Ideal := (I) -> (
     if numgens I === 0 then "0"
     else net expression I
     )
toString Ideal := toExternalString Ideal := (I) -> toString expression I

isIdeal Ideal := I -> true
isHomogeneous Ideal := (I) -> isHomogeneous generators I

degrees Ideal := I -> degrees source generators I
degreeLength Ideal := I -> degreeLength ring I

promote(Ideal,Number) := 
promote(Ideal,RingElement) := (I,R) -> ideal promote(generators I, R)

comodule Module := Module => M -> cokernel super map(M,M,1)
quotient Module := Module => opts -> M -> comodule M
comodule Ideal := Module => I -> cokernel generators I
quotient Ideal := Module => opts -> I -> (ring I) / I

genera Ideal := (I) -> genera ((ring I)^1/I)
genus Ideal := (I) -> genus ((ring I)^1/I)

eulers(Ideal) := (I) -> eulers((ring I)^1/I)
euler(Ideal) := (I) -> euler((ring I)^1/I)

RingElement * Ideal := Ideal => (r,I) -> ideal (r ** generators I)
ZZ * Ideal := (r,I) -> ideal (r * generators I)

generators Ideal := Matrix => opts -> (I) -> I.generators
Ideal_* := I -> first entries generators I
Ideal / Function := List => (I,f) -> apply(flatten entries generators I, f)
Function \ Ideal := List => (f,I) -> apply(flatten entries generators I, f)

generator = method()
generator Ideal := RingElement => (I) -> (
     if I.cache.?trim then I = I.cache.trim;
     R := ring I;
     n := numgens I;
     if n == 0 then return 0_R;
     if n == 1 then return I_0;
     I = trim I;
     n = numgens I;
     if n == 0 then return 0_R;
     if n == 1 then return I_0;
     error "expected ideal to have a single generator")
generator Module := RingElement => (M) -> (
     if M.cache.?trim then M = M.cache.trim;
     n := rank source generators M;
     if n == 0 then return 0_M;
     if n == 1 then return M_0;
     M = trim M;
     n = rank source generators M;
     if n == 0 then return 0_M;
     if n == 1 then return M_0;
     error "expected ideal to have a single generator")

mingens Ideal := Matrix => options -> (I) -> mingens(module I,options)
Ideal / Ideal := Module => (I,J) -> module I / module J
Module / Ideal := Module => (M,J) -> M / (J * M)

Ideal#{Standard,AfterPrint} = Ideal#{Standard,AfterNoPrint} = (I) -> (
     << endl;				  -- double space
     << concatenate(interpreterDepth:"o") << lineNumber << " : Ideal of " << ring I << endl;
     )

Ideal ^ ZZ := Ideal => (I,n) -> ideal symmetricPower(n,generators I)
Ideal * Ideal := Ideal => (I,J) -> ideal flatten (generators I ** generators J)
Ideal * Module := Module => (I,M) -> subquotient (generators I ** generators M, relations M)
dim Ideal := I -> dim cokernel generators I
Ideal + Ideal := Ideal => (I,J) -> ideal (generators I | generators J)
Ideal + RingElement := (I,r) -> I + ideal r
degree Ideal := I -> degree cokernel generators I
trim Ideal := Ideal => options -> (cacheValue symbol trim) ((I) -> ideal trim(module I, options))
Ideal _ ZZ := RingElement => (I,n) -> (generators I)_(0,n)
Matrix % Ideal := Matrix => (f,I) -> f % gb I
numgens Ideal := (I) -> numgens source generators I
leadTerm Ideal := Matrix => (I) -> leadTerm generators gb I
leadTerm(ZZ,Ideal) := Matrix => (n,I) -> leadTerm(n,generators gb I)
jacobian Ideal := Matrix => (I) -> jacobian generators I
poincare Ideal := (I) -> poincare comodule I
Ideal _ List := (I,w) -> (module I)_w

protect symbol Order
assert( class infinity === InfiniteNumber )
hilbertSeries = method(Options => {
     	  Order => infinity
	  }
     )

hilbertSeries Ideal := options -> (I) -> hilbertSeries((ring I)^1/I,options)

ring Ideal := (I) -> I.ring

Ideal == Ring := (I,R) -> (
     if ring I =!= R
     then error "expected ideal in the given ring";
     1_R % I == 0)

Ring == Ideal := (R,I) -> I == R

Ideal == Ideal := (I,J) -> (
     if ring I =!= ring J
     then error "expected ideals in the same ring";
     ( generators I == generators J or 
	  -- if isHomogeneous I and isHomogeneous J  -- can be removed later
	  -- then gb I == gb J 
	  -- else
	  isSubset(I,J) and isSubset(J,I)	  -- can be removed later
	  ))

Ideal == Module := (I,M) -> module I == M
Module == Ideal := (M,I) -> M == module I

module Ideal := Module => (cacheValue symbol module) (
     I -> (
	  M := image generators I;
	  if I.cache.?poincare then M.cache.poincare = I.cache.poincare;
	  M))

ideal Matrix := Ideal => (f) -> (
     R := ring f;
     if not isFreeModule target f or not isFreeModule source f 
     then error "expected map between free modules";
     f = flatten f;			  -- in case there is more than one row
     if target f != R^1 then (
     	  f = map(R^1,,f);
	  )
     else if not isHomogeneous f and isHomogeneous R then (
     	  g := map(R^1,,f);			  -- in case the degrees are wrong
     	  if isHomogeneous g then f = g;
	  );
     new Ideal from { symbol generators => f, symbol ring => R, symbol cache => new CacheTable } )

ideal Module := Ideal => (M) -> (
     F := ambient M;
     if isSubmodule M and rank F === 1 then ideal generators M
     else error "expected a submodule of a free module of rank 1"
     )
ideal List := ideal Sequence := Ideal => v -> ideal matrix {toList v}
ideal RingElement := ideal Number := Ideal => v -> ideal {v}
ideal Ring := R -> ideal map(R^1,R^0,0)

kernel = method(Options => { SubringLimit => infinity })
kernel Matrix := Module => opts -> (cacheValue symbol kernel) ((g) -> (
	  N := source g;
	  P := target g;
	  g = matrix g;
	  if P.?generators then g = P.generators * g;
	  h := modulo(g, if P.?relations then P.relations);
	  if N.?generators then h = N.generators * h;
	  subquotient( h, if N.?relations then N.relations)))
kernel RingElement := Module => options -> (g) -> kernel (matrix {{g}},options)

homology(Matrix,Matrix) := Module => opts -> (g,f) -> (
     if g == 0 then cokernel f
     else if f == 0 then kernel g
     else (
	  R := ring f;
	  M := source f;
	  N := target f;
	  P := target g;
	  if source g != N then error "expected maps to be composable";
	  f = matrix f;
	  if not all(degree f, i -> i === 0) then f = map(target f, source f ** R^{-degree f}, f);
	  g = matrix g;
	  if P.?generators then g = P.generators * g;
	  h := modulo(g, if P.?relations then P.relations);
	  if N.?generators then (
	       f = N.generators * f;
	       h = N.generators * h;
	       );
	  subquotient(h, if N.?relations then f | N.relations else f)))

Hom(Matrix, Module) := Matrix => (f,N) -> (
     if isFreeModule source f and isFreeModule target f
     then transpose f ** N
     else notImplemented())

Hom(Module, Matrix) := Matrix => (N,f) -> (
     if isFreeModule N 
     then dual N ** f
     else notImplemented())

dual(Matrix) := Matrix => f -> (
     R := ring f;
     Hom(f,R^1)
     )

     -- i%m gives an error message if the module is not free, but i//m doesn't, so we can't use this code in inverse Matrix to check invertibility:
     -- if i % m != 0 then error "matrix not invertible";
Matrix.InverseMethod =
inverse Matrix := (cacheValue symbol inverse) ( m -> id_(target m) // m )

Matrix _ Array := Matrix => (f,v) -> f * (source f)_v
Matrix ^ Array := Matrix => (f,v) -> (target f)^v * f

entries Matrix := (m) -> (
     R := ring m;
     m = m.RawMatrix;
     applyTable ( entries m, r -> promote(r,R) )
     )

getshift := (f) -> rawMultiDegree f.RawMatrix

degree Matrix := List => (f) -> (
     M := source f;
     N := target f;
     d := getshift f;
     if M.?generators then d = d - getshift M.generators;
     if N.?generators then d = d + getshift N.generators;
     d)

super(Matrix) := Matrix => (f) -> (
     M := target f;
     if M.?generators then map(super M, M, M.generators) * f
     else f
     )

isInjective Matrix := (f) -> kernel f == 0
isSurjective Matrix := (f) -> cokernel f == 0

scan({ZZ,QQ}, S -> (
	  lift(Ideal,S) := opts -> (I,S) -> (
	       -- this will be pretty slow
	       if ring I === S then I
	       else (ideal lift(generators I,S,opts)) + ideal (presentation ring I ** S))));

content(RingElement) := Ideal => (f) -> ideal \\ last \ listForm f

cover(Matrix) := Matrix => (f) -> matrix f

rank Matrix := (f) -> rank image f

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
