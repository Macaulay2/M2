--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman

ModuleMap = new Type of HashTable
ModuleMap.synonym = "module map"

Matrix = new Type of ModuleMap
Matrix.synonym = "matrix"
raw Matrix := f -> f.RawMatrix
ring Matrix := f -> (
     S := ring target f;
     R := ring source f;
     if R === S then R
     else error "expected module map with source and target over the same ring"
     )
source Matrix := f -> f.source
target Matrix := f -> f.target

newMatrix = method(TypicalValue => Matrix)
newMatrix(Module,Module,RawMatrix) := (tar,src,f) -> (     -- we erase this later
     R := ring tar;
     new Matrix from {
	  symbol ring => R,
	  symbol source => src,
	  symbol target => tar,
	  symbol RawMatrix => f,
	  symbol cache => new CacheTable
	  })
newMatrix(Ring,RawMatrix) := (R,f) -> (			    -- replaces getMatrix
     newMatrix(newModule(R,rawTarget f),newModule(R,rawSource f),f)
     )

reduce = (tar,f) -> (					    -- we erase this later
     if not isFreeModule tar then (
	  g := gb presentation tar;
     	  error "gb reduction not re-implemented yet";
	  sendgg(ggPush g, ggPush 1, ggpick, ggreduce, ggpop);
	  )
     else f)

QQ * Matrix := (r,m) -> (r * 1_(ring m)) * m
Matrix * QQ := (m,r) -> (r * 1_(ring m)) * m

ZZ * Matrix := (i,m) -> newMatrix(target m, source m, reduce(target m, i * m.RawMatrix))

Matrix * ZZ := (m,i) -> i * m

RingElement * Matrix := (r,m) -> (
     R := ring r;
     if R =!= ring m then error "scalar not in ring of matrix";
     newMatrix(target m, source m, reduce(target m, r.RawRingElement * m.RawMatrix)))

sameRing := (m,n) -> (
     if ring m =!= ring n then (
	  try (promote(m,ring n) , n) else
	  try (m , promote(n,ring m))
	  else error "expected matrices over compatible rings"
	  )
     else (m,n))

BinaryMatrixOperation := (operation) -> (m,n) -> (
     if ring m =!= ring n then (
	  try m = promote(m,ring n)
	  else try n = promote(n,ring m)
	  else error "expected matrices over compatible rings");
     sendgg (ggPush m, ggPush n, operation);
     getMatrix ring m)

BinaryMatrixOperationSame := (operation) -> (m,n) -> (
     -- same source and target
     if ring m =!= ring n then (
	  try m = promote(m,ring n)
	  else try n = promote(n,ring m)
	  else error "expected matrices over compatible rings");
     sendgg (ggPush m, ggPush n, operation);
     T := target m;
     reduce T;
     newMatrix(T, source m))

Matrix _ Sequence := RingElement => (m,ind) -> (
     if # ind === 2
     then (
     	  R := ring m;
	  rows := numgens target m;
	  cols := numgens source m;
	  i := ind#0;
	  j := ind#1;
	  if i < 0 or i >= rows then error (
	       "encountered row index ", toString i,
	       " out of range 0 .. ", toString (rows-1));
	  if j < 0 or j >= cols then error (
	       "encountered column index ", toString j,
	       " out of range 0 .. ", toString (cols-1));
	  new R from rawMatrixEntry(m.RawMatrix, ind#0, ind#1))
     else error "expected a sequence of length two"
     )

Matrix _ ZZ := Vector => (m,i) -> new m.target from rawMatrixColumn(raw m, i)

Matrix == Matrix := (m,n) -> target m == target n and source m == source n and raw m === raw n
Matrix == RingElement := (m,f) -> m - f == 0
RingElement == Matrix := (f,m) -> m - f == 0
Matrix == ZZ := (m,i) -> (
     if i === 0
     then rawIsZero m.RawMatrix
     else (
	  R := ring m;
	  m == i_R
	  )
     )
ZZ == Matrix := (i,m) -> m == i

Matrix + Matrix := Matrix => (
     (f,g) -> newMatrix(target f, source f, f.RawMatrix + g.RawMatrix)
     ) @@ sameRing
Matrix + RingElement := (f,r) -> if r == 0 then f else f + r*id_(target f)
RingElement + Matrix := (r,f) -> if r == 0 then f else r*id_(target f) + f
ZZ + Matrix := (i,f) -> if i === 0 then f else i*id_(target f) + f
Matrix + ZZ := (f,i) -> if i === 0 then f else f + i*id_(target f)

Matrix - Matrix := Matrix => (
     (f,g) -> newMatrix(target f, source f, f.RawMatrix - g.RawMatrix)
     ) @@ sameRing
Matrix - RingElement := (f,r) -> if r == 0 then f else f - r*id_(target f)
RingElement - Matrix := (r,f) -> if r == 0 then -f else r*id_(target f) - f
ZZ - Matrix := (i,f) -> if i === 0 then -f else i*id_(target f) - f
Matrix - ZZ := (f,i) -> if i === 0 then f else f - i*id_(target f)

- Matrix := Matrix => f -> new Matrix from {
     symbol ring => ring f,
     symbol source => source f,
     symbol target => target f,
     symbol handle => newHandle (ggPush f, ggnegate),
     symbol cache => new CacheTable
     }

setdegree := (R,deg,f) -> (
     
     
     )

Matrix * Matrix := Matrix => (m,n) -> (
     if source m == target n then (
	  if ring target m =!= ring target n then (
	       n = matrix n ** ring target m;
	       );
	  M := target m;
	  N := source n;
	  newMatrix(M,N,reduce(M,m.RawMatrix * n.RawMatrix)))
     else (
     	  R := ring m;
	  S := ring n;
	  if R =!= S then (
	       try m = m ** S else
	       try n = n ** R else
	       error "maps over incompatible rings";
	       );
	  M = target m;
	  P := source m;
	  N = source n;
	  Q := target n;
	  if not isFreeModule P or not isFreeModule Q or rank P =!= rank Q
	  then error "maps not composable";
	  dif := degrees P - degrees Q;
	  deg := (
	       if same dif
	       then (degree m + degree n + dif#0)
 	       else toList (degreeLength R:0)
	       );
	  if R.?Adjust then deg = R.Adjust deg;
	  f := m.RawMatrix * n.RawMatrix;
	  f = rawMatrix(rawTarget f, rawSource f, deg, f);
	  newMatrix(M,N,reduce(M,f))))

Matrix ^ ZZ := Matrix => (f,n) -> (
     if n === 0 then id_(target f)
     else SimplePowerMethod (f,n))

transpose Matrix := Matrix => (m) -> if m.cache.?transpose then m.cache.transpose else m.cache.transpose = (
     if not (isFreeModule source m and isFreeModule target m) 
     then error "expected a map between free modules";
     newMatrix(ring m, rawDual m.RawMatrix))

ring(Matrix) := m -> m.target.ring

Matrix * Vector := Vector => (m,v) -> (
     if class v =!= source m then error "map not applicable to vector";
     if not isFreeModule source m then notImplemented();
     sendgg(ggPush m, ggPush v, ggmult);
     new m.target)

expression Matrix := m -> MatrixExpression applyTable(entries m, expression)

toExternalString Matrix := m -> concatenate (
     "map(", toExternalString target m, ", ", toExternalString source m, ", ", toString entries m,
     if not all(degree m, i -> i === 0) then (", Degree => ", toString degree m),
     ")"
     )

toString Matrix := m -> concatenate ( "matrix ", toString entries m )

isIsomorphism Matrix := f -> coker f == 0 and ker f == 0

isHomogeneous Matrix := m -> (
     if m.cache.?isHomogeneous then m.cache.isHomogeneous 
     else m.cache.isHomogeneous = (
	  isHomogeneous ring target m
	  and isHomogeneous ring source m
	  and (
	       M := source m;
	       N := target m;
	       rawIsHomogeneous m.RawMatrix
	       and
	       ( not M.?generators or isHomogeneous M.generators )
	       and
	       ( not N.?generators or isHomogeneous N.generators )
	       )))

isWellDefined Matrix := f -> matrix f * presentation source f % presentation target f == 0

ggConcatCols := (tar,src,mats) -> (
     sendgg(apply(mats,ggPush), ggPush (#mats), ggconcat);
     f := newMatrix(tar,src);
     if same(degree \ mats) and degree f != degree mats#0
     then f = map(target f, source f, f, Degree => degree mats#0);
     f)

ggConcatRows := (tar,src,mats) -> (
     sendgg(
	  apply(mats,m -> (ggPush m, ggtranspose)), 
	  ggPush (# mats), ggconcat, ggtranspose
	  );
     f := newMatrix(tar,src);
     if same(degree \ mats)
     and degree f != degree mats#0
     then f = map(target f, source f, f, Degree => degree mats#0);
     f)

ggConcatBlocks = (tar,src,mats) -> (			    -- we erase this later
     sendgg (
	  apply(mats, row -> ( 
		    apply(row, m -> ggPush m), 
		    ggPush(#row), ggconcat, ggtranspose )),
	  ggPush(#mats), ggconcat, ggtranspose );
     f := newMatrix(tar,src);
     if same(degree \ flatten mats)
     and degree f != degree mats#0#0
     then f = map(target f, source f, f, Degree => degree mats#0#0);
     f)

samering = mats -> (					    -- we erase this later
     R := ring mats#0;
     if not all ( mats, m -> ring m === R )
     then error "expected matrices over the same ring";
     R)

Matrix.directSum = args -> (
     R := ring args#0;
     if not all(args, f -> ring f === R) 
     then error "expected matrices all over the same ring";
     sendgg(apply(args, ggPush), ggPush (#args), ggdirectsum);
     new Matrix from {
	  symbol ring => R,
	  symbol source => directSum apply(args,source),
	  symbol target => directSum apply(args,target),
	  symbol handle => newHandle "",
	  symbol cache => new CacheTable,
	  symbol components => toList args
	  })

isDirectSum = method()
isDirectSum Module := (M) -> M.?components

components Module := M -> if M.?components then M.components else {M}
components Matrix := f -> if f.?components then f.components else {f}

Module.directSum = args -> (
	  R := ring args#0;
	  if not all(args, f -> ring f === R) 
	  then error "expected modules all over the same ring";
	  N := if all(args, M -> not M.?generators) 
	  then (
	       if all(args, M -> not M.?relations) 
	       then R ^ (- join toSequence apply(args, degrees))
	       else subquotient( null, directSum apply(args,relations) )
	       )
	  else (
	       if all(args, M -> not M.?relations) then (
		    subquotient( directSum apply(args,generators), null )
		    )
	       else subquotient(
		    directSum apply(args,generators), 
		    directSum apply(args,relations)));
	  N.components = toList args;
	  N)

single := v -> (
     if not same v 
     then error "incompatible objects in direct sum";
     v#0)

indices = method()
indices MutableHashTable := X -> (
     if not X.?components then error "expected an object with components";
     if X.?indices then X.indices else toList ( 0 .. #X.components - 1 ) )

directSum List := args -> directSum toSequence args
directSum Sequence := args -> (
     if #args === 0 then error "expected more than 0 arguments";
     y := youngest args;
     key := (directSum, args);
     if y =!= null and y#?key then y#key else (
	  type := single apply(args, class);
	  if not type.?directSum then error "no method for direct sum";
	  S := type.directSum args;
	  if y =!= null then y#key = S;
	  S))
Option ++ Option := directSum
Option.directSum = args -> (
     if #args === 0 then error "expected more than 0 arguments";
     modules := apply(args,last);
     y := youngest modules;
     key := (directSum, args);
     if y =!= null and y#?key then y#key else (
	  type := single apply(modules, class);
	  if not type.?directSum then error "no method for direct sum";
	  S := type.directSum modules;
	  if y =!= null then y#key = S;
     	  keys := S.indices = toList args/first;
     	  S.indexComponents = new HashTable from toList apply(#keys, i -> keys#i => i);
	  S))
Matrix ++ Matrix := Matrix => directSum
Module ++ Module := Module => directSum

Matrix ++ RingElement := Matrix => 
Matrix ++ ZZ := (f,r) -> f ++ matrix {{r}}

RingElement ++ Matrix := Matrix => 
ZZ ++ Matrix := (r,f) -> matrix {{r}} ++ f

RingElement ++ RingElement := Matrix => 
ZZ ++ RingElement :=
RingElement ++ ZZ := (r,s) -> matrix {{r}} ++ matrix {{s}}

RingElement  | RingElement := Matrix => (r,s) -> matrix {{r}}  | matrix {{s}}
RingElement || RingElement := Matrix => (r,s) -> matrix {{r}} || matrix {{s}}

concatCols = mats -> (					    -- we erase this later
     mats = select(toList mats,m -> m =!= null);
     if # mats === 1 
     then mats#0
     else (
	  samering mats;
	  targets := unique apply(mats,target);
	  M := targets#0;
	  if not all(targets, F -> F == M) 
	  and not all(targets, F -> isFreeModule F)
	  then error "unequal targets";
	  ggConcatCols(targets#0, Module.directSum apply(mats,source), mats)))

concatRows = mats -> (					    -- we erase this later
     mats = select(toList mats,m -> m =!= null);
     if # mats === 1 
     then mats#0
     else (
	  samering mats;
	  sources := unique apply(mats,source);
	  N := sources#0;
	  if not all(sources, F -> F == N) 
	  and not all(sources, F -> isFreeModule F)
	  then error "unequal sources";
	  ggConcatRows(Module.directSum apply(mats,target), sources#0, mats)))

Matrix | Matrix := Matrix => (f,g) -> concatCols(f,g)
RingElement | Matrix := (f,g) -> concatCols(f**id_(target g),g)
Matrix | RingElement := (f,g) -> concatCols(f,g**id_(target f))
ZZ | Matrix := (f,g) -> concatCols(f*id_(target g),g)
Matrix | ZZ := (f,g) -> concatCols(f,g*id_(target f))

Matrix || Matrix := Matrix => (f,g) -> concatRows(f,g)
RingElement || Matrix := (f,g) -> concatRows(f**id_(source g),g)
     -- we would prefer for f**id_(source g) to have the exact same source as g does
Matrix || RingElement := (f,g) -> concatRows(f,g**id_(source f))
ZZ || Matrix := (f,g) -> concatRows(f*id_(source g),g)
Matrix || ZZ := (f,g) -> concatRows(f,g*id_(source f))

listZ := v -> (
     if not all(v,i -> class i === ZZ) then error "expected list of integers";
     )

Matrix _ List := Matrix => (f,v) -> (
     v = splice v;
     listZ v;
     submatrix(f,v)
     )

Matrix ^ List := Matrix => (f,v) -> (
     v = splice v;
     listZ v;
     submatrix(f,v,)
     )

submatrix(Matrix,List,Nothing) := (m,rows,cols) -> (
     submatrix(m, rows, 0 .. numgens source m - 1)
     )

submatrix(Matrix,Nothing,List) := (m,rows,cols) -> (
     submatrix(m, 0 .. numgens target m - 1, cols)
     )

submatrix(Matrix,Sequence,Sequence) := 
submatrix(Matrix,Sequence,List) := 
submatrix(Matrix,List,Sequence) := 
submatrix(Matrix,List,List) := Matrix => (m,rows,cols) -> (
     if not isFreeModule source m or not isFreeModule target m
     then error "expected a homomorphism between free modules";
     rows = toList splice rows;
     listZ rows;
     cols = toList splice cols;
     listZ cols;
     error "IM2_Matrix_submatrix not re-implemented yet";
     sendgg(ggPush m, 
	  ggINTARRAY, gg rows, ggINTARRAY, gg cols, ggsubmatrix);
     getMatrix ring m)

submatrix(Matrix,List) := Matrix => (m,cols) -> (
     cols = toList splice cols;
     listZ cols;
     error "IM2_Matrix_submatrix not re-implemented yet";
     sendgg(ggPush m, 
	  ggINTARRAY, gg cols, 
	  ggsubmatrix);
     getMatrix ring m)

diff(Matrix, Matrix) := Matrix => (
     (f,g) -> newMatrix(target f, source f, rawMatrixDiff(f.RawMatrix, g.RawMatrix))
     ) @@ sameRing
diff(RingElement, RingElement) := RingElement => (f,g) -> (
     (diff(matrix{{f}},matrix{{g}}))_(0,0)
     )
diff(Matrix, RingElement) := (m,f) -> diff(m,matrix{{f}})
diff(RingElement, Matrix) := (f,m) -> diff(matrix{{f}},m)
diff(Vector, RingElement) := (v,f) -> (diff(matrix{v},matrix{{f}}))_0
diff(RingElement, Vector) := (f,v) -> diff(matrix{{f}},transpose matrix{v})
diff(Vector, Vector) := (v,w) -> diff(matrix{v}, transpose matrix{w})
diff(Matrix, Vector) := (m,w) -> diff(m,transpose matrix {w})
diff(Vector, Matrix) := (v,m) -> diff(matrix {v}, m)
diff(RingElement)    := f -> diff(vars ring f, f)
diff(Matrix)         := m -> diff(vars ring m, m)

contract (Matrix, Matrix) := Matrix => diff(Matrix, Matrix) := Matrix => (
     (f,g) -> newMatrix(target f, source f, rawMatrixContract(f.RawMatrix, g.RawMatrix))
     ) @@ sameRing
contract(RingElement, RingElement) := RingElement => (f,g) -> (
     (contract(matrix{{f}},matrix{{g}}))_(0,0)
     )
contract(Matrix, RingElement) := (m,f) -> contract(m,matrix{{f}})
contract(RingElement, Matrix) := (f,m) -> contract(matrix{{f}},m)
contract(Vector, RingElement) := (v,f) -> (contract(matrix{v},matrix{{f}}))_0
contract(RingElement, Vector) := (f,v) -> contract(matrix{{f}},transpose matrix{v})
contract(Vector, Vector) := (v,w) -> contract(matrix{v}, transpose matrix{w})
contract(Matrix, Vector) := (m,w) -> contract(m,transpose matrix {w})
contract(Vector, Matrix) := (v,m) -> contract(matrix {v}, m)

diff'(Matrix, Matrix) := Matrix => (m,n) -> (
     flip(dual target n, target m) * diff(n,m) * flip(source m, dual source n)
     )

contract'(Matrix, Matrix) := Matrix => (m,n) -> (
     flip(dual target n, target m) * contract(n,m) * flip(source m, dual source n)
     )

jacobian = method()
jacobian Matrix := Matrix => (m) -> diff(transpose vars ring m, m)

jacobian Ring := Matrix => (R) -> jacobian presentation R ** R

leadTerm(ZZ, Matrix) := Matrix => (i,m) -> (
     sendgg(ggPush m, ggINT, gg i, gginitial);
     getMatrix ring m)
leadTerm(Matrix) := Matrix => m -> (
     sendgg(ggPush m, gginitial);
     getMatrix ring m)

borel Matrix := Matrix => m -> (
     sendgg (
	  ggPush m, ggINT, gg 0, ggmonideal,  -- get monomial lead ideal
	  ggborel,                            -- monomial borel now on stack
	  ggmatrix);
     getMatrix ring m)

--------------------------------------------------------------------------
------------------------ matrix and map for modules ----------------------
--------------------------------------------------------------------------

mopts := Options => {
     Degree => null,					    -- for use with matrices
     DegreeMap => null					    -- for use in ring maps
     }

matrix = method mopts
map = method mopts

map(Module,Module) := Matrix => options -> (M,N) -> (
     F := ambient N;
     if F == ambient M
     then map(M,N,
	  if M.?generators 
	  then map(M,N,generators N // generators M) -- sigh, should we check for zero remainder?
	  else generators N,
	  options)
     else error "expected modules to have the same ambient free module"
     )

map(Module,Module,RingElement) := Matrix => options -> (M,N,r) -> (
     error "IM2_Matrix_zero not re-implemented yet";
     R := ring M;
     if r == 0 then new Matrix from {
	  symbol handle => newHandle(ggPush cover M, ggPush cover N, ggzeromat),
	  symbol source => N,
	  symbol target => M,
	  symbol ring => ring M,
	  symbol cache => new CacheTable
	  }
     else if numgens cover M == numgens cover N then map(M,N,r * id_(cover M)) 
     else error "expected 0, or source and target with same number of generators")

map(Module,Module,ZZ) := Matrix => options -> (M,N,i) -> (
     error "IM2_Matrix_zero not re-implemented yet";
     if i === 0 then new Matrix from {
	  symbol handle => newHandle(ggPush cover M, ggPush cover N, ggzeromat),
	  symbol source => N,
	  symbol target => M,
	  symbol ring => ring M,
	  symbol cache => new CacheTable
	  }
     else if M == N then map(M,i)
     else if numgens cover M == numgens cover N then map(M,N,i * id_(cover M)) 
     else error "expected 0, or source and target with same number of generators")

map(Module,RingElement) := Matrix => options -> (M,r) -> (
     R := ring M;
     try r = r + R#0
     else error "encountered scalar of unrelated ring";
     if r == 0 then map(M,M,0)
     else if r == 1 then map(M,1)
     else r * (map(M,1)))

map(Module) := Matrix => options -> (M) -> (
     R := ring M;
     if options.Degree =!= null then error "Degree option encountered with identity matrix";
     newMatrix(M, M, reduce(M, rawIdentity(M.RawFreeModule))))

map(Module,ZZ) := Matrix => options -> (M,i) -> (
     if i === 0 then map(M,M,0)
     else if i === 1 then map(M,options)
     else i * map M)

map(Module,Matrix) := Matrix => options -> (M,f) -> (
     R := ring M;
     if R =!= ring f then error "expected the same ring";
     if # degrees M =!= # degrees target f then (
	  error "expected ambient modules of the same rank";
	  );
     diffs := degrees M - degrees target f;
     if not same diffs then (
	  error "expected to find uniform difference between degrees"
	  );
     map(M,source f ** R^{-first diffs},f)
     )

inducedMap = method (
     Options => {
	  Verify => true,
	  Degree => null 
	  })
inducedMap(Module,Module,Matrix) := Matrix => options -> (M,N,f) -> (
     sM := target f;
     sN := source f;
     if ambient M != sM
     then error "'inducedMap' expected target of map to be a subquotient of target module provided";
     if ambient N != sN
     then error "'inducedMap' expected source of map to be a subquotient of source module provided";
     g := f * generators N;
     h := generators M;
     p := map(M, N, g // h, Degree => options.Degree);
     if options.Verify then (
	  if g % h != 0
	  then error "'inducedMap' expected matrix to induce a map";
	  if not isWellDefined p
	  then error "'inducedMap' expected matrix to induce a well-defined map";
	  );
     p)
inducedMap(Module,Nothing,Matrix) := o -> (M,N,f) -> inducedMap(M,source f, f,o)
inducedMap(Nothing,Module,Matrix) := o -> (M,N,f) -> inducedMap(target f,N, f,o)
inducedMap(Nothing,Nothing,Matrix) := o -> (M,N,f) -> inducedMap(target f,source f, f,o)

inducedMap(Module,Module) := Matrix => o -> (M,N) -> (
     if ambient M != ambient N 
     then error "'inducedMap' expected modules with same ambient free module";
     inducedMap(M,N,id_(ambient N),o))

inducesWellDefinedMap = method(TypicalValue => Boolean)
inducesWellDefinedMap(Module,Module,Matrix) := (M,N,f) -> (
     sM := target f;
     sN := source f;
     if ambient M =!= sM
     then error "'inducesWellDefinedMap' expected target of map to be a subquotient of target module provided";
     if ambient N =!= sN
     then error "'inducesWellDefinedMap' expected source of map to be a subquotient of source module provided";
     (f * generators N) % (generators M) == 0
     and
     (f * relations N) % (relations M) == 0)     
inducesWellDefinedMap(Module,Nothing,Matrix) := (M,N,f) -> inducesWellDefinedMap(M,source f,f)
inducesWellDefinedMap(Nothing,Module,Matrix) := (M,N,f) -> inducesWellDefinedMap(target f,N,f)
inducesWellDefinedMap(Nothing,Nothing,Matrix) := (M,N,f) -> true

vars Ring := Matrix => R -> (
     g := generators R;
     if R.?vars then R.vars else R.vars =
     map(R^1,,{g}))

generators Module := Matrix => M -> if M.?generators then M.generators else id_(ambient M)

relations Module := Matrix => M -> (
     if M.?relations then M.relations 
     else (
	  R := ring M;
	  map(ambient M,R^0,0)
	  )
     )

degrees Matrix := f -> {degrees target f, degrees source f}

coverMap(Module) := Matrix => (M) -> map(M, cover M, gens M)

ambient Matrix := Matrix => f -> (
     M := target f;
     N := source f;
     if N.?generators then error "ambient matrix requested, but source has generators";
     if N.?relations then (
	  if M.?generators then M.generators * map(M.generators.source,N.relations.target,f)
	  else if M.?relations then map(M.relations.target,N.relations.target,f)
	  else map(M,N.relations.target,f)
	  )
     else (
	  if M.?generators then M.generators * map(M.generators.source,N,f)
	  else if M.?relations then map(M.relations.target,N,f)
	  else f
	  )
     )

degrees Ring := R -> degree \ gens R
