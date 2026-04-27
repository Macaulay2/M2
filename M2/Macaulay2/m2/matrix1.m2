--		Copyright 1993-2002 by Daniel R. Grayson

needs "matrix.m2"
needs "modules.m2"
needs "quotient.m2"

-----------------------------------------------------------------------------

notsamering := (X,Y) -> (
     if X === Y then error("expected ",pluralsynonym X, " for the same ring")
     else error("expected ",X.synonym," and ",Y.synonym," for the same ring"))
nottosamering := (X,Y) -> (
     if X === Y then error("expected ",pluralsynonym X, " for compatible rings")
     else error("expected ",X.synonym," and ",Y.synonym," for compatible rings"))
samering = (M,N) -> if ring M === ring N then (M,N) else notsamering(class M,class N)
tosamering := (M,N) -> if ring M === ring N then (M,N) else (
     z := try 0_(ring M) + 0_(ring N) else nottosamering(class M,class N);
     (promote(M,ring z),promote(N,ring z)))

module Ring := Module => (cacheValue symbol module)(R -> R^1)

matrix(RingFamily,List) := Matrix => opts -> (R,m) -> matrix(default R, m, opts)

matrix(Ring,List) := Matrix => opts -> (R,m) -> (
     m = apply(splice m,splice);
     if not isTable m then error "expected a table";
     map(R^#m,,m,opts))

map(Module,Module,Function) := Matrix => opts -> (M,N,f) -> (
     map(M,N,table(numgens M, numgens N, f))
     )

map(Matrix) := Matrix => opts -> (f) -> (
     if opts.Degree === null then f
     else (
     	  R := ring source f;
	  d := opts.Degree;
	  if class d === ZZ then d = {d};
     	  map(target f, source f ** R^{d - degree f}, f, opts)))

map(Module,ZZ,Function) := Matrix => opts -> (M,n,f) -> map(M,n,table(numgens M,n,f),opts)

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
    -- TODO: why give an error? Compare with map(Module,Nothing,RawMatrix)
     if o.Degree =!= null then error "Degree option given with indeterminate source module";
     samering(M,p);
     R := ring M;
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

map(Module,Module,Matrix) := Matrix => o -> (M,N,f) -> (
     if M === f.target and N === f.source
     and (o.Degree === null or o.Degree === degree f)
     then (
	  -- let's redo it quickly, just to capture everything in M.cache and N.cache that === ignores
	  merge(f,new Matrix from { symbol target => M, symbol source => N }, last)
	  )
     else (
	  R := ring M;
	  deg := if o.Degree === null then (degreeLength R : 0) else o.Degree;
	  deg = degreeCheck(deg,R);
	  g := rawMatrixRemake2(raw cover M, raw cover N, deg, raw f, 0);
	  map(M, N, if M =!= f.target then reduce(M, g) else g)))

-- combine the one above with the one below
map(Module,ZZ,List) := 
map(Module,Nothing,List) := 
map(Module,Module,List) := Matrix => options -> (M,N,p) -> (
     R := ring M;
     local rankN;
     local k;
     if N === null then (
	  k = R;
	  rankN = if #p === 0 then 0 else #p#0;
	  )
     else if class N === ZZ then (
	  k = R;
	  rankN = N;
	  )
     else (
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
	       and class o#0 === Sequence
	       and #o#0 == 2
	       and class o#0#0 === ZZ
	       and class o#0#1 === ZZ
	       )
	  ) then (		    -- sparse list of entries
     	  rows := apply(p, o -> o#0#0);
	  cols := apply(p, o -> o#0#1);
	  ents := toSequence apply(p, o -> raw promote(o#1,R));
	  pref := 0;					    -- ???
	  m := (
	       if class N === Module
	       then rawSparseMatrix2(raw cover M, raw cover N, deg, rows, cols, ents, pref)
	       else rawSparseMatrix1(raw cover M, rankN, rows, cols, ents, pref)
	       );
	  new Matrix from {
	       symbol target => M,
	       symbol RawMatrix => reduce(M,m),
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
	       if instance(N,Module)
	       then rawMatrix2(raw cover M, raw cover N, deg, flatten p,0)
	       else rawMatrix1(raw cover M, rankN, flatten p, 0)
	       );
	  new Matrix from {
	       symbol target => M,
	       symbol RawMatrix => reduce(M,h),
	       symbol source => if class N === Module then N else new Module from (R, rawSource h),
	       symbol ring => R,
	       symbol cache => new CacheTable
	       })
     else error "expected a list of lists or a list of options (i,j)=>r")

fixDegree := (m,d) -> (
     M := target m;
     N := source m;
     map(M,N,rawMatrixRemake2(raw cover M, raw cover N, degreeCheck(d,ring m), raw m, 0)))

concatBlocks = mats -> (
     if not isTable mats then error "expected a table of matrices";
     if #mats === 1
     then concatCols mats#0
     else if #(mats#0) === 1
     then concatRows (mats/first)
     else (
     	  sameringMatrices flatten mats;
	  sources := applyTable(mats,source);
	  targets := transpose applyTable(mats,target);
	  -- if not same sources then error "expected matrices in the same column to have equal sources";
	  -- if not same targets then error "expected matrices in the same row to have equal targets";
     	  ggConcatBlocks( Module.directSum first targets, Module.directSum first sources, mats)))

Matrix.matrix = options -> (f) -> concatBlocks f

commonRing = method()
commonRing List := L -> (
    rings := unique apply(flatten L, ring);
    if #rings === 0 then ZZ      else
    if #rings === 1 then rings#0 else
    ring try sum apply(rings, R -> 0_R) else error "common ring not found")

matrixTable = opts -> (f) -> (
     R := commonRing f;
     havemat := false;
     f = applyTable(f,
	 x -> if isMorphism x then (havemat = true; promote(x, R))
	 else if instance(x, RingElement) or instance(x, Number) then promote(x, R)
	 else error "expected numbers, ring elements, and matrices");
     if not havemat then return map(R^#f, , f, opts);
     types := unique apply(flatten f, class);
     -- Note: we use Matrix.matrix here, which is different from Matrix#matrix
     if # types === 1 and types#0 .?matrix then return ( types#0 .matrix opts)(f);
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
		    if not instance(r,Matrix) and r != 0 then (
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
     if opts.Degree === null
     then mm
     else fixDegree(mm,opts.Degree)
     )

matrix(Matrix) := Matrix => opts -> (m) -> (
     if isFreeModule target m and isFreeModule source m and (not opts.Degree =!= null or degree m === opts.Degree or degree m === {opts.Degree} )
     then m
     else map(cover target m, cover source m, raw m, Degree => if opts.Degree =!= null then opts.Degree else degree m)
     )

matrix RingElement := matrix Number := opts -> r -> matrix({{r}}, opts)

matrix List := Matrix => opts -> L -> (
    if #L === 0 then return matrix(ZZ, {});
    L = apply(splice L, splice); -- TODO: is this deepSplice?
    if not uniform L and not isTable L
    then error "expected a list of vectors or a table of entries or maps";
    -- construct a matrix by concatenating column vectors
    if instance(L#0, Vector) then matrix { apply(L, matrix) } else
    -- construct a matrix from a table of entries or maps
    if isTable L then (matrixTable opts)(L)
    else error "expected a rows all to be the same length")

align := g -> (
     -- generator and relation maps can just as well have a nonzero degree
     -- this function zeroes the degree, preserving homogeneity
     deg := degree g;
     if all (deg, zero) then g
     else map(target g, source g ** (ring g)^{ -deg }, g, Degree => apply(#deg, x->0)))

subquotient = method(TypicalValue => Module)
subquotient(Nothing,Matrix) := (null,relns) -> (
     R := ring relns;
     E := target relns;
     rE := E.RawFreeModule;
     Mparts := {
	  symbol cache => new CacheTable from { 
	       cache => new MutableHashTable	    -- this hash table is mutable, hence has a hash number that can serve as its age
	       },
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
subquotient(Matrix, Nothing) := (subgens0, null) -> (
    R := ring subgens0;
    E := target subgens0;
    rE := E.RawFreeModule;
    subgens := align matrix subgens0;
     if E.?generators then subgens = E.generators * subgens;
     Mparts := {
	  symbol cache => new CacheTable from { 
	    symbol Monomials => subgens0,
	       cache => new MutableHashTable	    -- this hash table is mutable, hence has a hash number that can serve as its age
	       },
	  symbol RawFreeModule => rE,
	  symbol ring => R,
	  symbol numgens => rawRank rE,
     	  symbol generators => subgens
	  };
     if E.?relations then (
	  Mparts = append(Mparts, symbol relations => E.relations);
	  );
     new Module of Vector from hashTable Mparts)
subquotient(Matrix, Matrix) := (subgens0, relns) -> (
    R := ring relns;
    E := target subgens0;
     if E =!= target relns then error "expected maps with the same target";
     rE := E.RawFreeModule;
     n := rawRank rE;
     if n == 0 then new Module from (R,rE)
     else (
	  relns = align matrix relns;
	subgens := align matrix subgens0;
	  if E.?generators then (
	       relns = E.generators * relns;
	       subgens = E.generators * subgens;
	       );
	  if E.?relations then relns = relns | E.relations;
	  Mparts := {
	       symbol cache => new CacheTable from { 
		symbol Monomials => subgens0,
		    cache => new MutableHashTable	    -- this hash table is mutable, hence has a hash number that can serve as its age
		    },
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

Matrix ** Matrix := Matrix => (A, B) -> tensor(A, B)
tensor(Matrix, Matrix) := Matrix => {} >> opts -> ((f, g) -> (
     samering(target f,target g);
     samering(source f,source g);
     R := ring target f;
     if f === id_(R^1) then return g;
     if g === id_(R^1) then return f;
     map(target f ** target g, 
	  source f ** source g, 
	  map(R, f.RawMatrix ** g.RawMatrix),
	  Degree => degree f + degree g))) @@ toSameRing

Matrix ** Module := Matrix => (f, M) -> tensor(f, id_M)
Module ** Matrix := Matrix => (M, f) -> tensor(id_M, f)
tensor(Matrix, Module) := Matrix => {} >> o -> (f, M) -> tensor(f, id_M)
tensor(Module, Matrix) := Matrix => {} >> o -> (M, f) -> tensor(id_M, f)

Matrix ** Number := (f,r) -> r * f
Number ** Matrix := (r,f) -> r * f
Matrix ** RingElement := (f,r) -> f ** matrix {{r}}
RingElement ** Matrix := (r,f) -> matrix {{r}} ** f

Number ** RingElement := 
RingElement ** Number := 
RingElement ** RingElement := (r,s) -> matrix {{r}} ** matrix {{s}}

Matrix.AfterPrint = Matrix.AfterNoPrint = f -> (
    class f,
    (
	(tar, src) := apply((target f, source f), moduleAbbrv);
	if tar =!= null and src =!= null
	then (" ", MapExpression(tar, src))
    ))

-- precedence Matrix := x -> precedence symbol x

-- source and target are defined in modules.m2
-- image caches f in M.cache.Monomials
image    Matrix := Module => f -> f.cache.image    ??= subquotient(f, null)
cokernel Matrix := Module => f -> f.cache.cokernel ??= subquotient(null, f)
-- kernel is defined in pushforward.m2
coimage  Matrix := Module => f -> f.cache.coimage  ??= cokernel inducedMap(source f, kernel f)
-- homology is defined further down

cokernel RingElement := Module => f -> cokernel matrix {{f}}
image RingElement := Module => f -> image matrix {{f}}

Ideal = new Type of HashTable
Ideal.synonym = "ideal"

ideal = method(Dispatch => Thing, TypicalValue => Ideal)
ideal Ideal := identity

expression Ideal := (I) -> (expression ideal) unsequence apply(toSequence first entries generators I, expression)
net Ideal := (I) -> net expression I
toString Ideal := (I) -> toString expression I
toExternalString Ideal := (I) -> "ideal " | toExternalString generators I
texMath Ideal := (I) -> texMath expression I

isIdeal Ideal := I -> true
isHomogeneous Ideal := (I) -> isHomogeneous generators I

degrees Ideal := I -> degrees source generators I

promote(Ideal,Number) := 
promote(Ideal,RingElement) := (I,R) -> ideal promote(generators I, R)

comodule Module := Module => M -> cokernel super map(M,M,1)
quotient Module := Module => opts -> M -> comodule M
comodule Ideal := Module => I -> cokernel generators I
quotient Ideal := Module => opts -> I -> (ring I) / I
module   Ideal := Module => (cacheValue symbol module) (I -> image generators I)

genera Ideal := (I) -> genera ((ring I)^1/I)
genus Ideal := (I) -> genus ((ring I)^1/I)

eulers(Ideal) := (I) -> eulers((ring I)^1/I)
euler(Ideal) := (I) -> euler((ring I)^1/I)

RingElement * Ideal := Ideal => (r,I) -> ideal (r ** generators I)
Ideal * RingElement := Ideal => (I,r) -> ideal ((generators I)**r)
ZZ * Ideal := (r,I) -> ideal (r * generators I)
Ideal * ZZ := (I,r) -> ideal (r * generators I)

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

Ideal / Ideal := Module => (I,J) -> module I / module J
Module / Ideal := Module => (M,J) -> M / (J * M)

Ideal#AfterPrint = Ideal#AfterNoPrint = (I) ->  (Ideal," of ",ring I)

Ideal ^ ZZ := Ideal => (I,n) -> ideal symmetricPower(n,generators I)
Ideal * Ideal := Ideal => ((I,J) -> ideal flatten (generators I ** generators J)) @@ samering
Ideal * Module := Module => ((I,M) -> subquotient (generators I ** generators M, relations M)) @@ samering
Ideal + Ideal := Ideal => ((I,J) -> ideal (generators I | generators J)) @@ tosamering
Ideal + RingElement := Ideal + Number := ((I,r) -> I + ideal r) @@ tosamering
RingElement + Ideal := Number + Ideal := ((r,I) -> ideal r + I) @@ tosamering
Ideal _ ZZ := RingElement => (I,n) -> (generators I)_(0,n)
Matrix % Ideal := Matrix => ((f,I) -> 
     if numRows f === 1
     then f % gb I
     else (
	  -- we should have an engine routine to reduce each entry of a matrix modulo an ideal, to make this faster
	  R := ring I;
	  S := R/I;
	  lift(promote(f,S),R))
     ) @@ samering
Vector % Ideal := (v,I) -> new class v from {v#0%I}
numgens Ideal := (I) -> numgens source generators I
leadTerm Ideal := Ideal => (I) -> ideal leadTerm gb I
leadTerm(ZZ,Ideal) := Matrix => (n,I) -> ideal leadTerm(n,gb I)
jacobian Ideal := Matrix => (I) -> jacobian generators I
Ideal _ List := (I,w) -> (module I)_w

ring Ideal := (I) -> I.ring

Ideal == Ring := (I,R) -> (
     if ring I =!= R
     then error "expected ideal in the given ring";
     1_R % I == 0)

Ring == Ideal := (R,I) -> I == R

Ideal == Ideal := (I,J) -> (
     samering(I,J);
     ( generators I == generators J or 
	  -- if isHomogeneous I and isHomogeneous J  -- can be removed later
	  -- then gb I == gb J 
	  -- else
	  isSubset(I,J) and isSubset(J,I)	  -- can be removed later
	  ))

Ideal == Module := (I,M) -> module I == M
Module == Ideal := (M,I) -> M == module I

isSubset(Module, Ideal) :=
isSubset(Ideal, Module) :=
isSubset(Ideal, Ideal)  := (I, J) -> isSubset(module I, module J)

ideal Matrix := Ideal => (f) -> (
     R := ring f;
     if not isFreeModule target f or not isFreeModule source f 
     then error "expected map between free modules";
     f = flatten f;			  -- in case there is more than one row
     f = align f;			  -- in case degree f is nonzero
     if target f != R^1 then (
     	  f = map(R^1,,f);
	  )
     else if not isHomogeneous f and isHomogeneous R then (
     	  g := map(R^1,,f);			  -- in case the degrees are wrong
     	  if isHomogeneous g then f = g;
	  );
     new Ideal from { symbol generators => f, symbol ring => R, symbol cache => new CacheTable } )

ideal Module := Ideal => M -> if isIdeal M then ideal generators M else (
    error "expected a submodule of a free module of rank 1")

idealPrepare = method()
idealPrepare RingElement := 
idealPrepare Number := identity
idealPrepare Matrix := m -> flatten entries m
idealPrepare Ideal := I -> I_*
idealPrepare Thing := x -> error "expected a list of numbers, matrices, ring elements or ideals"
ideal List := ideal Sequence := Ideal => v -> ideal matrix {flatten apply(toList splice v,idealPrepare)}
ideal RingElement := ideal Number := Ideal => v -> ideal {v}
ideal Ring := R -> ideal map(R^1,R^0,0)

Ideal ^ Array := (I, e) -> (
   R := ring I;
   n := numgens R;
   -- Error if input is not correct.
   if any(e, i -> i < 0) then error "Expected nonnegative exponents.";
   if #e != 1 and n != #e then error "Expected single integer array, or array with length equal to the number of variables.";
   -- if only one element, then make vector the same as the length of
   -- the number of variables with the same number in each entry
   if #e == 1 then e = new Array from n:(e_0);
   -- build a ring homomorphism that will perform this substitution for us
   phi := map(R,R,matrix {apply(numgens R, i -> (R_i)^(e_i))});
   -- apply the ring homomorphism and create the new ideal
   ideal phi generators I
)

-----------------------------------------------------------------------------
-- kernel and homology
-----------------------------------------------------------------------------

kernel = method(Options => {
	DegreeLimit  => {},
	Strategy     => {},
	SubringLimit => infinity, -- stop after finding enough elements of a subring
    })
kernel Matrix := Module => opts -> g -> g.cache.kernel ??= if g == 0 then source g else tryHooks(
    (kernel, Matrix), (opts, g), (opts, g) -> (
	-- this is the default algorithm
	-- compare with homology below
	N := source g;
	P := target g;
	g = matrix g;
	if P.?generators then g = P.generators * g;
	h := modulo(g, if P.?relations then P.relations);
	if N.?generators then h = N.generators * h;
	subquotient(h, if N.?relations then N.relations)))
kernel RingElement := Module => opts -> f -> kernel(matrix {{f}}, opts)

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

-----------------------------------------------------------------------------

     -- i%m gives an error message if the module is not free, but i//m doesn't, so we can't use this code in inverse Matrix to check invertibility:
     -- if i % m != 0 then error "matrix not invertible";
Matrix.InverseMethod =
inverse Matrix := (cacheValue symbol inverse) (
     m -> (
      if hasEngineLinearAlgebra ring m and isBasicMatrix m then
          basicInverse m
      else (
	      (quo,rem) := quotientRemainder(id_(target m), m);
	      if rem != 0 then error "matrix not invertible";
	      quo))
     )

Matrix _ Array := Matrix => (f,v) -> f * (source f)_v
Matrix ^ Array := Matrix => (f,v) -> (target f)^v * f

entries Matrix := (m) -> (
     R := ring target m;
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
content(RingElement, RingElement) := Ideal => (f,x) -> ideal last coefficients(f, Variables => {x})

cover(Matrix) := Matrix => (f) -> matrix f

rank Matrix := (f) -> (
    if hasEngineLinearAlgebra ring f and isBasicMatrix f 
    then basicRank f 
    else rank image f
    )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
