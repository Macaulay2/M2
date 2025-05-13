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
     if ambient M =!= ambient N
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
Module^** ZZ     := Module => (F, n) -> BinaryPowerMethod(F, n, tensor, module @@ ring, dual)
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

tensorAssociativity = method()
tensorAssociativity(Module, Module, Module) := Matrix => (A, B, C) -> map((A ** B) ** C, A ** (B ** C), 1)

-- TODO: this is undocumented and only works correctly in a specific case.
-- can its goal be accomplished differently?
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
-- TODO: make documentation page for base change
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

Ideal * Ring := Ideal ** Ring := Ideal => (I, R) -> R ** I
Ring * Ideal := Ring ** Ideal := Ideal => (R, I) -> if ring I === R then I else ideal(I.generators ** R)

-----------------------------------------------------------------------------

ZZ == Ideal := (n,I) -> I == n
Ideal == ZZ := (I,n) -> (
     if n === 0
     then I.generators == 0
     else if n === 1
     then issub(matrix {{1_(ring I)}}, generators I)
     else error "attempted to compare ideal to integer not 0 or 1"
     )

-----------------------------------------------------------------------------

presentation Module := Matrix => M -> M.cache.presentation ??= (
	  if M.?generators then (
	       modulo( M.generators, if M.?relations then M.relations)
	       )
    else relations M)

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

addHook((minimalPresentation, Module), Strategy => "PID", (opts, M) -> (
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
    	       rows = rows | toList(rank target f..<rank target g); -- temporary fix for #3017
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

Module / Module := Module => (M,N) -> (
     L := ambient M;
     if L =!= ambient N then error "expected modules with the same ambient module";
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
-- TODO: is caching here wise? There are 2^(#comps) many possibilities
Module ^ Array := Matrix => (M, w) -> M.cache#(symbol ^, w) ??= (
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

Module _ Array := Matrix => (M, w) -> M.cache#(symbol _, w) ??= (
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
Module ^ List := Matrix => (M, rows) -> submatrix(map(cover M, M, id_M), rows,)
Module _ List := Matrix => (M, cols) -> submatrix(map(M, cover M, id_M), cols)
-----------------------------------------------------------------------------

pullback = method(Options => true)
pullback List := Module => {} >> o -> applyUniformMethod(symbol pullback, "pullback")
pullback(Matrix, Matrix) := Module => {} >> o -> (f, g) -> pullback {f, g}

Matrix.pullback = args -> (
    if not same apply(args, target) then error "expected morphisms with the same target";
    h := concatCols args;
    P := kernel h;
    S := source h;
    P.cache.formation = FunctionApplication (pullback, args);
    P.cache.pullbackMaps = apply(#args,
	i -> map(source args#i, S, S^[i], Degree => - degree args#i) * inducedMap(S, P));
    P)

pushout = method()
pushout List := Module => applyUniformMethod(symbol pushout, "pushout")
pushout(Matrix, Matrix) := Module => (f, g) -> pushout {f, g}

Matrix.pushout = args -> (
    if not same apply(args, source) then error "expected morphisms with the same source";
    h := concatRows args;
    P := cokernel h;
    T := target h;
    P.cache.formation = FunctionApplication (pushout, args);
    P.cache.pushoutMaps = apply(#args,
	i -> inducedMap(P, T) * map(T, target args#i, T_[i], Degree => - degree args#i));
    P)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
