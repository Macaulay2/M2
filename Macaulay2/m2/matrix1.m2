--		Copyright 1993-1998 by Daniel R. Grayson

module = method()

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
map(Module,ZZ,List) := Matrix => options -> (M,rankN,p) -> (
     if options.Degree =!= null
     then error "Degree option given with indeterminate source module";
     R := ring M;
     p = apply(splice p,splice);
     if #p != numgens M
     or #p > 0 and ( not isTable p or # p#0 != rankN )
     then error( "expected ", toString numgens M, " by ", toString rankN, " table");
     p = applyTable(p,x -> promote(x,R));
     coverM := cover M;
     h := newHandle(
	  apply(
	       if # p === 0 then splice {rankN:{}}
	       else transpose p, 
	       col -> {apply(col, r -> ggPush r), ggPush coverM, ggvector}
	       ),
	  ggPush coverM,
	  ggPush rankN,
	  ggmatrix);
     N := ( sendgg(ggPush h,gggetcols); new Module from R );
     new Matrix from {
     	  symbol handle => h,
	  symbol source => N,
	  symbol target => M,
	  symbol ring => R,
	  symbol cache => new CacheTable
	  })

map(Module,Nothing,Matrix) := Matrix => options -> (M,nothing,p) -> (
     R := ring M;
     coverM := cover M;
     n := numgens cover source p;
     colvectors := apply(n, i -> p_i);
     if options.Degree =!= null
     then error "Degree option given with indeterminate source module";
     h := newHandle( colvectors / ggPush, ggPush coverM, ggPush n, ggmatrix);
     N := (sendgg(ggPush h,gggetcols); new Module from R);
     new Matrix from {
	  symbol target => M,
	  symbol handle => h,
	  symbol source => N,
	  symbol ring => R,
	  symbol cache => new CacheTable
	  }
     )

degreeCheck := (d,R) -> (
     if class d === ZZ then d = {d};
     if class d === List
     and all(d,i -> class i === ZZ) 
     and #d === degreeLength R
     then (
	  if R.?Adjust then d = R.Adjust d;
	  )
     else (
	  if degreeLength R === 1
	  then error "expected degree to be an integer or list of integers of length 1"
	  else error (
	       "expected degree to be a list of integers of length ",
	       toString degreeLength R
	       )
	  );
     d)

map(Module,Module,Matrix) := Matrix => options -> (M,N,f) -> (
     if M === f.target and N === f.source
     and (options.Degree === null or options.Degree === degree f)
     then f
     else (
	  R := ring M;
	  N' := cover N ** R;
	  sendgg (ggPush cover M, ggPush N', ggPush f,
	       ggPush (
		    if options.Degree === null
		    then toList (degreeLength R : 0)
		    else degreeCheck(options.Degree, R)),
	       ggmatrix);
	  reduce M;
	  newMatrix(M,N)))

map(Module,Nothing,List) := 
map(Module,Module,List) := Matrix => 
options -> (M,N,p) -> (
     R := ring M;
     if N === null
     then (
	  k := R;
	  if #p === 0 then error "expected non-empty list of entries for matrix";
	  rankN := #p#0;
	  )
     else (
     	  k = ring N;
     	  try promote(1_k,R) else error "modules over incompatible rings";
	  -- later, allow a ring homomorphism
	  rankN = numgens N;
	  );
     p = apply(splice p,splice);
     if #p != numgens M
     or #p > 0 and ( not isTable p or # p#0 != rankN )
     then error( "expected ", toString numgens M, " by ", toString rankN, " table");
     p = applyTable(p,x -> promote(x,R));
     coverM := cover M;
     h := newHandle(
	  apply(
	       if # p === 0 then splice {rankN:{}}
	       else transpose p, 
	       col -> {apply(col, r -> ggPush r), ggPush coverM, ggvector}
	       ),
	  ggPush coverM,
	  if N === null
	  then (
	       if options.Degree =!= null
	       then error "Degree option given with indeterminate source module";
	       ggPush rankN,
	       if R.?newEngine then (
		    ggPush 3,				    -- 1=left 2=right 3=both
		    ggPush toList (degreeLength R:0)
		    )
	       )
	  else (
	       ggPush cover N,
	       if R.?newEngine then (
		    ggPush 3				    -- 1=left 2=right 3=both
		    ),
	       ggPush (
		    if options.Degree === null
		    then toList (degreeLength R:0)
		    else degreeCheck(options.Degree,R)
		    )
	       ),
	  ggmatrix);
     N' := (
	  if N === null then (sendgg(ggPush h,gggetcols); new Module from R)
	  else N
	  );
     new Matrix from {
	  symbol target => M,
	  symbol handle => h,
	  symbol source => N',
	  symbol ring => R,
	  symbol cache => new CacheTable
	  })

fixDegree := (m,d) -> (
     M := target m;
     N := source m;
     R := ring M;
     sendgg (
	  ggPush cover M,
	  ggPush cover N,
	  ggPush m, 
	  ggPush degreeCheck(d,R),
	  ggmatrix);
     newMatrix(M,N)
     )

concatBlocks := mats -> (
     if not isTable mats then error "expected a table of matrices";
     if #mats === 1
     then concatCols mats#0
     else if #(mats#0) === 1
     then concatRows (mats/first)
     else (
     	  samering flatten mats;
	  sources := unique applyTable(mats,source);
	  N := sources#0;
	  if not all(sources, F -> F == N) and not all(sources, F -> all(F,isFreeModule))
	  then error "unequal sources";
	  targets := unique transpose applyTable(mats,target);
	  M := targets#0;
	  if not all(targets, F -> F == M) and not all(targets, F -> all(F,isFreeModule))
	  then error "unequal targets";
     	  ggConcatBlocks(
	       Module.directSum (mats/first/target),
	       Module.directSum (mats#0/source),
	       mats)))

Matrix.matrix = options -> (f) -> concatBlocks f

matrixTable := options -> (f) -> (
     types := unique apply(flatten f, class);
     if # types === 1 then (
	  type := types#0;
	  if instance(type,Ring) then (
	       R := type;
	       map(R^#f,, f, options))
	  else if type.?matrix then (type.matrix options)(f)
	  else error "no method for forming a matrix from elements of this type")
     else if all(types, T -> instance(T,Ring)) then (
	  R = ring (
	       try sum apply(types, R -> R#0)
	       else error "couldn't put matrix elements into the same ring"
	       );
	  map(R^#f,,f,options))
     else if all(types, T -> instance(T,Ring) or T === Matrix) then (
	  rings := unique apply(select(flatten f,m -> class m === Matrix), ring);
	  if #rings > 1 then error "matrices over different rings";
	  R = rings#0;
	  f = apply(f, row -> new MutableList from row);
	  m := #f;
	  n := #f#0;
	  tars := new MutableHashTable;
	  srcs := new MutableHashTable;
	  scan(m, i->scan(n, j-> (
			 r := f#i#j;
			 if class r === Matrix then (
			      if tars#?i and tars#i != target r
			      then error "matrices not compatible";
			      tars#i = target r;
			      if srcs#?i and srcs#i != source r
			      then error "matrices not compatible";
			      srcs#j = source r;
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
	  mm := concatBlocks f;
	  if options.Degree === null
	  then mm
	  else fixDegree(mm,options.Degree)
	  )
     else error "expected ring elements or matrices")

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
	  if instance(type,Module) 
	  then (
	       M := type;
	       if M.?generators then error "not implemented yet";
	       R := ring M;
	       coverM := cover M;
	       h := newHandle(ggPush\m, ggPush coverM, ggPush (#m), ggmatrix);
     	       N := (sendgg(ggPush h,gggetcols); new Module from R);
	       new Matrix from {
		    symbol target => M,
		    symbol handle => h,
		    symbol source => N,
		    symbol ring => R,
		    symbol cache => new CacheTable
		    }
	       )
	  else if type === List then (
	       if isTable m then (matrixTable options)(m)
	       else error "expected rows all to be the same length"
	       )
	  else error "expected a table of ring elements or matrices, or a list of elements of the same module")
     else error "expected a table of ring elements or matrices, or a list of elements of the same module")

--------------------------------------------------------------------------

Module#id = (M) -> map(M,1)

reshape = method()
reshape(Module,Module,Matrix) := Matrix => (F, G, m) -> (
     if not isFreeModule F or not isFreeModule G
     then error "expected source and target to be free modules";
     sendgg(ggPush m, ggPush F, ggPush G, ggreshape);
     getMatrix ring m)

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
flip(Module,Module) := Matrix => (F,G) -> (
     sendgg(ggPush F, ggPush G, ggflip);
     getMatrix ring F)

align := f -> (
     if isHomogeneous f and any(degree f, i -> i =!= 0) then map(target f,,f) else f
     )

subquotient = method(TypicalValue => Module)
subquotient(Nothing,Matrix) := Module => (null,relns) -> (
     M := new Module of Vector;
     M.ring = ring relns;
     E := target relns;
     M.handle = handle E;
     relns = align matrix relns;
     if E.?generators then (
	  M.generators = E.generators;
	  relns = E.generators * relns;
	  );
     if E.?relations then relns = relns | E.relations;
     if relns != 0 then M.relations = relns;
     M.numgens = (sendgg (ggPush M.handle, gglength); eePopInt());
     M#0 = (sendgg(ggPush M, ggzero); new M);
     M)
subquotient(Matrix,Nothing) := Module => (subgens,null) -> (
     M := new Module of Vector;
     E := target subgens;
     subgens = align matrix subgens;
     if E.?generators then subgens = E.generators * subgens;
     M.handle = E.handle;
     M.generators = subgens;
     if E.?relations then M.relations = E.relations;
     M.ring = ring subgens;
     M.numgens = (sendgg (ggPush M.handle, gglength); eePopInt());
     M#0 = (sendgg(ggPush M, ggzero); new M);
     M)
subquotient(Matrix,Matrix) := Module => (subgens,relns) -> (
     E := target subgens;
     if E != target relns then error "expected maps with the same target";
     M := new Module of Vector;
     M.ring = ring subgens;
     M.handle = handle E;
     M.numgens = (sendgg (ggPush M.handle, gglength); eePopInt());
     M#0 = ( sendgg(ggPush M, ggzero); new M);
     if M == 0 then M
     else (
	  relns = align matrix relns;
	  subgens = align matrix subgens;
	  if E.?generators then (
	       relns = E.generators * relns;
	       subgens = E.generators * subgens;
	       );
	  if E.?relations then relns = relns | E.relations;
	  M.generators = subgens;
	  if relns != 0 then M.relations = relns;
	  M))

Matrix ** Matrix := Matrix => (f,g) -> (
     R := ring target f;
     if ring target g =!= R 
     or ring source g =!= ring source f
     then error "expected matrices over the same ring";
     sendgg (ggPush f, ggPush g, ggtensor);
     h := getMatrix R;
     map(target f ** target g, source f ** source g, h, Degree => degree f + degree g))

Matrix ** ZZ := 
Matrix ** QQ := (f,r) -> r * f
ZZ ** Matrix := 
QQ ** Matrix := (r,f) -> r * f

Matrix ** RingElement := (f,r) -> f ** matrix {{r}}
RingElement ** Matrix := (r,f) -> matrix {{r}} ** f

QQ ** RingElement := 
ZZ ** RingElement := 
RingElement ** QQ := 
RingElement ** ZZ := 
RingElement ** RingElement := (r,s) -> matrix {{r}} ** matrix {{s}}

Matrix.AfterPrint = 
Matrix.AfterNoPrint = f -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : Matrix";
     if isFreeModule target f and isFreeModule source f
     then << " " << target f << " <--- " << source f;
     << endl;
     )

precedence Matrix := x -> precedence symbol x

compactMatrixForm = true

net Matrix := f -> (
     if f == 0 
     then "0"
     else (
	  m := (
	       if compactMatrixForm then (
	       	    stack toSequence apply(lines sendgg(ggPush f,ggsee,ggpop), x -> concatenate("| ",x,"|"))
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
     if f.cache.?coimage then f.cache.coimage else f.cache.coimage = cokernel map(source f, kernel f)
     )
cokernel Matrix := Module => m -> (
     if m.cache.?cokernel then m.cache.cokernel else m.cache.cokernel = subquotient(,m)
     )

cokernel RingElement := Module => f -> cokernel matrix {{f}}
image RingElement := Module => f -> image matrix {{f}}

Ideal = new Type of HashTable
Ideal.synonym = "ideal"

ideal = method(SingleArgumentDispatch=>true, TypicalValue => Ideal)

expression Ideal := (I) -> new FunctionApplication from { 
     ideal,
     (
	  v := expression toSequence first entries generators I;
     	  if #v === 1 then v#0 else v
	  )
     }
net Ideal := (I) -> (
     if numgens I === 0 then "0"
     else net expression I
     )
toString Ideal := (I) -> if I.cache.?name then I.cache.name else toString expression I

isHomogeneous Ideal := (I) -> isHomogeneous I.generators

degrees Ideal := I -> degrees source gens I

genera(Ideal) := (I) -> genera module I
euler(Ideal) := (I) -> euler module I

RingElement * Ideal := Ideal => (r,I) -> ideal (r ** generators I)
ZZ * Ideal := (r,I) -> ideal (r * generators I)

generators Ideal := Matrix => (I) -> I.generators
mingens Ideal := Matrix => options -> (I) -> mingens(module I,options)
Ideal / Ideal := Module => (I,J) -> module I / module J
Module / Ideal := Module => (M,J) -> M / (J * M)

Ideal.AfterPrint = Ideal.AfterNoPrint = (I) -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : Ideal of " << ring I << endl;
     )

Ideal ^ ZZ := Ideal => (I,n) -> ideal symmetricPower(n,generators I)
Ideal * Ideal := Ideal => (I,J) -> ideal flatten (generators I ** generators J)
Ideal * Module := Module => (I,M) -> subquotient (generators I ** generators M, relations M)
dim Ideal := I -> dim cokernel generators I
codim Ideal := I -> codim cokernel generators I
Ideal + Ideal := Ideal => (I,J) -> ideal (generators I | generators J)
degree Ideal := I -> degree cokernel generators I
trim Ideal := Ideal => options -> (I) -> ideal trim(module I, options)
map(Ideal) := Matrix => options -> (I) -> map(module I,options)
map(Ideal,Ideal) := Matrix => options -> (I,J) -> map(module I,module J,options)
Ideal _ ZZ := RingElement => (I,n) -> (generators I)_(0,n)
Matrix % Ideal := Matrix => (f,I) -> f % gb I
numgens Ideal := (I) -> numgens source generators I
leadTerm Ideal := Matrix => (I) -> leadTerm generators gb I
leadTerm(ZZ,Ideal) := Matrix => (n,I) -> leadTerm(n,generators gb I)
jacobian Ideal := Matrix => (I) -> jacobian generators I
poincare Ideal := (I) -> poincare module I
Ideal _ List := (I,w) -> (module I)_w

protect symbol Order
assert( class infinity === InfiniteNumber )
hilbertSeries = method(Options => {
     	  Order => infinity
	  }
     )

hilbertSeries Ideal := options -> (I) -> hilbertSeries(module I,options)

ring Ideal := (I) -> I.ring

Ideal == Ring := (I,R) -> (
     if ring I =!= R
     then error "expected ideals in the same ring";
     1_R % I == 0)

Ring == Ideal := (R,I) -> I == R

Ideal == Ideal := (I,J) -> (
     if ring I =!= ring J
     then error "expected ideals in the same ring";
     ( I.generators == J.generators or 
	  -- if isHomogeneous I and isHomogeneous J  -- can be removed later
	  -- then gb I == gb J 
	  -- else
	  isSubset(I,J) and isSubset(J,I)	  -- can be removed later
	  ))

Ideal == Module := (I,M) -> module I == M
Module == Ideal := (M,I) -> M == module I

module Ideal := Module => I -> (
     M := image I.generators;
     if I.cache.?poincare then M.poincare = I.cache.poincare;
     M
     )

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
ideal RingElement := Ideal => v -> ideal {v}
ideal ZZ := v -> ideal {v}
ideal QQ := v -> ideal {v}

ideal Ring := R -> ideal 0_R

kernel = method(Options => {
	  SubringLimit => infinity
	  -- DegreeLimit => {}
	  })

ker = kernel

kernel Matrix := Module => options -> (g) -> if g.cache.?kernel then g.cache.kernel else g.cache.kernel = (
     N := source g;
     P := target g;
     g = matrix g;
     if P.?generators then g = P.generators * g;
     h := modulo(g, if P.?relations then P.relations
	  -- DegreeLimit => options.DegreeLimit
	  );
     if N.?generators then h = N.generators * h;
     subquotient( h, if N.?relations then N.relations))

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

Matrix.InverseMethod = m -> if m.cache#?-1 then m.cache#-1 else m.cache#-1 = (
     id_(target m) // m
     )

Matrix _ Array := Matrix => (f,v) -> f * (source f)_v
Matrix ^ Array := Matrix => (f,v) -> (target f)^v * f

entries = method()
entries Matrix := (m) -> (
     M := target m;
     R := ring M;
     N := source m;
     sendgg (ggPush m,
      	  apply(numgens M, i -> apply(numgens N, j -> (
		    	 ggdup, ggINT, gg i, ggINT, gg j, ggelem, ggINT, gg 1, ggpick
		    	 ))));
     RPop := R.pop;
     sendgg ggpop;
     r := reverse apply(numgens M, i -> reverse apply(numgens N, j -> RPop()));
     r)

getshift := (f) -> (
     sendgg(ggPush f, gggetshift);
     d := eePopIntarray();
     R := ring f;
     if R.?Repair then R.Repair d else d)

degree Matrix := List => (f) -> (
     M := source f;
     N := target f;
     d := getshift f;
     if M.?generators then d = d - getshift M.generators;
     if N.?generators then d = d + getshift N.generators;
     d)

promote(Matrix,ZZ) := (f,ZZ) -> (
     if ring f === ZZ then f
     else error "can't promote");
promote(Matrix,QQ) := (f,QQ) -> (
     if ring f === QQ then f
     else matrix applyTable(entries f, r -> promote(r,QQ)));

super(Matrix) := Matrix => (f) -> (
     M := target f;
     if M.?generators then map(super M, M, M.generators) * f
     else f
     )

isInjective Matrix := (f) -> kernel f == 0
isSurjective Matrix := (f) -> cokernel f == 0

scan({ZZ}, S -> (
	  lift(Matrix,S) := (f,S) -> (
	       -- this will be pretty slow
	       if ring target f === S then f
	       else if isQuotientOf(ring f,S) and
		       isFreeModule source f and
		       isFreeModule target f then
		   map(S^(-degrees target f), S^(-degrees source f), 
		       applyTable(entries f, r -> lift(r,S)))
	       else matrix(S, applyTable(entries f, r -> lift(r,S)))
	       );
	  lift(Ideal,S) := (I,S) -> (
	       -- this will be pretty slow
	       if ring I === S then I
	       else
		   (ideal lift(I.generators,S)) +
		   ideal (presentation ring I ** S));
	  ));

content(RingElement) := content(Matrix) := Ideal => (f) -> (
     R := ring f;
     n := numgens R;
     k := coefficientRing R;
     trim ideal lift((coefficients(splice {0..n-1},f))#1, k))

cover(Matrix) := Matrix => (f) -> matrix f

rank Matrix := (f) -> rank image f

erase symbol reduce
erase symbol newMatrix
erase symbol concatRows
erase symbol concatCols
erase symbol samering
erase symbol ggConcatBlocks
