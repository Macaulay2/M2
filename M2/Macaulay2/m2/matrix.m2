--		Copyright 1995-2002 by Daniel R. Grayson and Michael Stillman
-- methods of "map" with a RawMatrix replace "getMatrix", which was a private function

needs "modules.m2"

oops := R -> error (
     if degreeLength R === 1
     then "expected degree to be an integer or list of integers of length 1"
     else ("expected degree to be a list of integers of length ", toString degreeLength R))

degreeCheck = (d,R) -> (				    -- assume d =!= null
     if class d === ZZ then d = {d};
     if class d === Sequence then d = toList d;
     if not instance(d,List) then oops R;
     d = spliceInside toList d;
     if not ( all(d,i -> class i === ZZ) and #d === degreeLength R ) then oops R;
     toSequence d)

map(Module,Module,RawMatrix) := opts -> (tar,src,f) -> (
     R := ring tar;
     if raw cover src =!= source f
     or raw cover tar =!= target f
     or opts.Degree =!= null and rawMultiDegree f =!= (deg := degreeCheck(opts.Degree,R))
     then f = rawMatrixRemake2(raw cover tar, raw cover src, if deg =!= null then deg else rawMultiDegree f, f, 0);
     new Matrix from {
	  symbol ring => R,
	  symbol target => tar,
	  symbol source => src,
	  symbol RawMatrix => f,
	  symbol cache => new CacheTable
	  })
map(Module,Nothing,RawMatrix) := opts -> (tar,nothing,f) -> (
     R := ring tar;
     olddeg := rawMultiDegree f;
     newdeg := if opts.Degree =!= null then degreeCheck(opts.Degree,R);
     if raw cover tar =!= target f 
     or newdeg =!= null and newdeg =!= olddeg
     then f = rawMatrixRemake2(
	  raw cover tar, 
	  if newdeg =!= null then rawSource f ** (raw R)^{toList newdeg-olddeg} else rawSource f,
	  if newdeg =!= null then newdeg else olddeg,
	  f, 0);
     new Matrix from {
	  symbol ring => R,
	  symbol target => tar,
	  symbol source => new Module from (ring tar,rawSource f),
	  symbol RawMatrix => f,
	  symbol cache => new CacheTable
	  })
map(Ring,RawMatrix) := opts -> (R,f) -> (
     if opts.Degree =!= null and rawMultiDegree f =!= (deg := degreeCheck(opts.Degree,R))
     then f = rawMatrixRemake2(rawTarget f, rawSource f, deg, f, 0);
     new Matrix from {
	  symbol ring => R,
	  symbol target => new Module from (R,rawTarget f),
	  symbol source => new Module from (R,rawSource f),
	  symbol RawMatrix => f,
	  symbol cache => new CacheTable
	  })

numeric(ZZ, Matrix) := (prec,f) -> (
     F := ring f;
     if instance(F, InexactField) then return f;
     if F === ZZ or F === QQ then return promote(f,RR_prec);
     error "expected matrix of numbers"
     )
numeric Matrix := f -> numeric(defaultPrecision,f)

-- the key for reduce hooks under GlobalHookStore
protect ReduceHooks
reduce = (tar,rawF) -> (
    if isFreeModule tar then return rawF;
    if (C := runHooks(ReduceHooks, (tar, rawF))) =!= null then C
    else error "reduce: no strategy implemented for this type of ring")

addHook(ReduceHooks, Strategy => Default, (tar, rawF) -> rawF % raw gb presentation tar)

InfiniteNumber * Matrix := (r,m) -> (map(target m, source m, matrix(r*(entries m))))
Matrix * InfiniteNumber := (m,r) -> r*m
Number * Matrix :=
RingElement * Matrix := (r,m) -> (
    if ring r =!= ring m then try r = promote(r,ring m) else m = promote(m,ring r);
     map(target m, source m, reduce(target m, raw r * raw m)))
Matrix * Number :=
Matrix * RingElement := (m,r) -> (
    if ring r =!= ring m then try r = promote(r,ring m) else m = promote(m,ring r);
     map(target m, source m, reduce(target m, raw m * raw r)))
Matrix / Number      :=
Matrix / RingElement := (m,r) -> m * (1/r)

toSameRing = (m,n) -> (
     if ring m =!= ring n then (
	  try (promote(m,ring n) , n) else
	  try (m , promote(n,ring m))
	  else error "expected compatible rings"
	  )
     else (m,n))

Matrix _ Sequence := RingElement => (m,ind) -> (
     if # ind === 2
     then promote(rawMatrixEntry(m.RawMatrix, ind#0, ind#1), ring m)
     else error "expected a sequence of length two"
     )

Number      == Matrix :=
RingElement == Matrix :=
Number      == Vector :=
RingElement == Vector := (r,m) -> m == r

Matrix == Matrix := (f,g) -> (
    if source f === source g
      then if target f === target g 
             then raw f === raw g
             else target f == target g and
                  raw super f === raw super g
      else source f == source g and
           target f == target g and 
           raw(super f * inducedMap(source f, source g)) === raw super g
    )
Vector == Matrix := (v,m) -> matrix v == m
Matrix == Vector := (m,v) -> m == matrix v

Matrix == Number :=
Matrix == RingElement := (m,f) -> m - f == 0		    -- slow!
Vector == Number      :=
Vector == RingElement := (v,f) -> matrix v == f
Matrix == ZZ := (m,i) -> if i === 0 then rawIsZero m.RawMatrix else m - i == 0

Matrix + Matrix := Matrix => (
     (f,g) -> map(target f, source f, reduce(target f, raw f + raw g))
     ) @@ toSameRing
Matrix + RingElement :=
Matrix + Number      := (f,r) -> if r == 0 then f else f + r*id_(target f)
RingElement + Matrix :=
Number      + Matrix := (r,f) -> f + r
Vector + Number := Vector + RingElement := (v,r) -> vector(matrix v + r)
Number + Vector := RingElement + Vector := (r,v) -> vector(r + matrix v)

Matrix - Matrix := Matrix => (
     (f,g) -> map(target f, source f, reduce(target f, raw f - raw g))
     ) @@ toSameRing
Matrix - RingElement :=
Matrix - Number      := (f,r) -> if r == 0 then f else f - r*id_(target f)
RingElement - Matrix :=
Number      - Matrix := (r,f) -> -f + r
Vector - Number := Vector - RingElement := (v,r) -> vector(matrix v - r)
Number - Vector := RingElement - Vector := (r,v) -> vector(r - matrix v)

- Matrix := Matrix => f -> new Matrix from {
     symbol ring => ring f,
     symbol source => source f,
     symbol target => target f,
     symbol RawMatrix => reduce(target f, -raw f),
     symbol cache => new CacheTable
     }

Matrix * Matrix := Matrix => (m,n) -> (
     if source m === target n then (
	  M := target m;
	  N := source n;
	  nraw := (
	       if m.?RingMap
	       then nraw = rawRingMapEval(raw m.RingMap, raw m.RingMap cover target n, n.RawMatrix)
	       else n.RawMatrix
	       );
	  q := reduce(M,m.RawMatrix * nraw);
	  map(
	       M,
	       N,
	       if m.?RingMap then (
		    if n.?RingMap then m.RingMap * n.RingMap else m.RingMap
		    )
	       else (
		    if n.?RingMap then n.RingMap
		    ),
	       q,
	       Degree => degree m + if m.?RingMap then m.RingMap.cache.DegreeMap degree n else degree n))
     else (
     	  R := ring m;
	  S := ring target n;
	  if R =!= S then ( -- use toSameRing?
	       try m = promote(m,S) else
	       try n = promote(n,R) else
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
	       if #dif === 0
	       then degree m + degree n
	       else if same dif
	       then degree m + degree n + dif#0
 	       else toList (degreeLength ring m:0)
	       );
	  f := m.RawMatrix * n.RawMatrix;
	  f = rawMatrixRemake2(rawTarget f, rawSource f, deg, f, 0);
	  f = reduce(M,f);
	  if n.?RingMap 
	  then map(M,N,n.RingMap,f)
	  else map(M,N,f)))

Matrix#1 = f -> (
    if source f =!= target f then error "expected source and target to agree"
    else id_(target f))
Matrix ^ ZZ := Matrix => BinaryPowerMethod

transpose Matrix := Matrix => (cacheValue symbol transpose) (
     (m) -> (
     	  if not (isFreeModule source m and isFreeModule target m) 
     	  then error "expected a map between free modules";
     	  map(dual source m, dual target m, rawDual m.RawMatrix)))

Matrix * Vector := Matrix Vector := Vector => (m,v) -> (
     u := m * v#0;
     new target u from {u})

blocks := m -> if m.cache.?components then flatten apply(m.cache.components,blocks) else { rank m }

protect Blocks
blockMatrixForm=false;  -- governs expression Matrix inclusion of blocks
expression Matrix := m -> MatrixExpression (
    if m == 0 then {symbol zero => (target m, source m)}
    else (
	x := applyTable(entries m, expression);
	d := degrees -* cover *- target m;
	if not all(d, i -> all(i, j -> j == 0)) then x=append(x,Degrees=>{d, degrees source m});
	if blockMatrixForm then (
	    b1 := blocks target m;
	    b2 := blocks source m;
	    if #b1>1 or #b2>1 then x=append(x,Blocks=>{b1,b2});
	    );
	x)
    )

net Matrix := net @@ expression
toString Matrix := toString @@ expression
texMath Matrix := texMath @@ expression
short Matrix := m -> MatrixExpression ( -- can't go thru MatrixExpression
    if m==0 then {symbol zero => (target m, source m)} else (
	n1:=rank target m;
	n2:=rank source m;
	if n1>shortLength then
	if n2>shortLength then
	{{short m_(0,0),cdots,short m_(0,n2-1)},{vdots,ddots,vdots},{short m_(n1-1,0),cdots,short m_(n1-1,n2-1)}}
	else
	{apply(n2,i->short m_(0,i)),toList(n2:vdots),apply(n2,i->short m_(n1-1,i))}
	else if n2>shortLength then
	apply(n1,i->{short m_(i,0),cdots,short m_(i,n2-1)})
	else applyTable(entries m,short)
	)
    )

describe Matrix := m -> (
    args:=(describe target m,describe source m);
    if m.?RingMap then args=append(args,describe m.RingMap);
    args=append(args, expression if m == 0 then 0 else entries m);
    if not all(degree m,zero) then args=append(args,expression(Degree=>degree m));
    Describe (expression map) args
    )
toExternalString Matrix := m -> toString describe m;

isIsomorphism = method(TypicalValue => Boolean)
isIsomorphism Matrix := f -> cokernel f == 0 and kernel f == 0

isHomogeneous Matrix := (cacheValue symbol isHomogeneous) ( m -> ( isHomogeneous target m and isHomogeneous source m and rawIsHomogeneous m.RawMatrix ) )

isWellDefined Matrix := f -> matrix f * presentation source f % presentation target f == 0

-----------------------------------------------------------------------------
-- directSum and friends
-----------------------------------------------------------------------------

ggConcatCols := (tar,src,mats) -> (
     map(tar,src,if mats#0 .?RingMap then mats#0 .RingMap,rawConcatColumns (raw\mats),Degree => if same(degree \ mats) then degree mats#0)
     )

ggConcatRows := (tar,src,mats) -> (
     map(tar,src,if mats#0 .?RingMap then mats#0 .RingMap,rawConcatRows (raw\mats),Degree => if same(degree \ mats) then degree mats#0)
     )

ggConcatBlocks = (tar,src,mats) -> (
     f := map(tar,src,rawConcatBlocks mats);
     if same(degree \ flatten mats)
     and degree f != degree mats#0#0
     then f = map(target f, source f, f, Degree => degree mats#0#0);
     f)

sameringMatrices = mats -> (
     if same apply(mats, m -> (if m.?RingMap then m.RingMap,ring target m, ring source m))
     then mats
     else (
	  R := try ring sum apply(toList mats, m -> 0_(ring target m)) else error "expected matrices over compatible rings";
	  apply(mats, m -> promote(m,R))))

directSum Matrix := f -> Matrix.directSum (1 : f)
Matrix.directSum = args -> (
     args = sameringMatrices args;
     R := ring args#0;
     if not all(args, f -> ring f === R) then error "expected matrices all over the same ring";
     new Matrix from {
	  symbol RawMatrix => rawDirectSum toSequence (raw\args),
	  symbol ring => R,
	  symbol source => directSum apply(args,source),
	  symbol target => directSum apply(args,target),
	  symbol cache => new CacheTable from { symbol components => toList args }
	  })

isDirectSum = method()
isDirectSum Module := (M) -> M.cache.?components

components Module := M -> if M.cache.?components then M.cache.components else {M}
components Matrix := f -> if f.cache.?components then f.cache.components else {f}

formation = method()
formation Module := M -> if M.cache.?formation then M.cache.formation

directSum Module := M -> Module.directSum (1 : M)
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
	  N.cache.components = toList args;
	  N.cache.formation = FunctionApplication (directSum, args);
	  N)
Module ^ ZZ := Module => (M, i) -> if i > 0 then Module.directSum (i:M) else 0*M

indices = method()
indices HashTable := X -> (
     if X.cache.?components then if X.cache.?indices then X.cache.indices else toList ( 0 .. #X.cache.components - 1 )
     else error "expected an object with components"
     )

-- This is used for methods like directSum or pullback/pushout
-- which accept an arbitrary list of objects of the same type.
applyUniformMethod = (symb, name) -> args -> (
    if #args === 0 then error("expected at least one argument for ", name);
    type := if uniform args then class args#0 else error("expected uniform objects for ", name);
    meth := lookup(symb, type) ?? error("no method for ", name, " of ", pluralsynonym type);
    if (Y := youngest args) =!= null and Y.?cache
    then Y.cache#(symb, args) ??= meth args else meth args)

directSum List := args -> directSum toSequence args
directSum Sequence := applyUniformMethod(symbol directSum, "direct sum")

-- Number.directSum = v -> directSum apply(v, a -> matrix{{a}})

Option ++ Option := directSum
directSum Option := o -> directSum(1 : o)
Option.directSum = args -> (
     if #args === 0 then error "expected at least one argument";
     objects := apply(args,last);
     labels  := toList args/first;
     type := if uniform objects then class objects#0 else error "incompatible objects in direct sum";
     meth := lookup(symbol directSum, type);
     if meth === null then error("no method for direct sum of ", pluralsynonym type);
     M := meth objects;
     M.cache.indices = labels;
     ic := M.cache.indexComponents = new HashTable from apply(#labels, i -> labels#i => i);
     -- now, in case M is a map (i.e., has a source and target), then label the source and target objects of the sum
     if M.?source and M.?target then (
	  M.source.cache.indexComponents = M.target.cache.indexComponents = ic; 
	  M.source.cache.indices = M.target.cache.indices = labels;
	  );
     M)
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

concatCols = mats -> (
     mats = nonnull toList mats;
     if # mats === 1 then return mats#0;
     mats = sameringMatrices mats;
     sources := apply(mats,source);
     -- if not all(sources, F -> isFreeModule F) then error "expected sources to be free modules";
     targets := apply(mats,target);
     -- if not same targets then error "expected matrices in the same row to have equal targets";
     ggConcatCols(targets#0, Module.directSum sources, mats))

concatRows = mats -> (
     mats = nonnull toList mats;
     if # mats === 1 then return mats#0;
     mats = sameringMatrices mats;
     sources := apply(mats,source);
     -- if not same sources then error "expected matrices in the same column to have equal sources";
     targets := apply(mats,target);
     ggConcatRows(Module.directSum targets, sources#0, mats))

Matrix | Matrix := Matrix => (f,g) -> concatCols(f,g)
RingElement | Matrix := (f,g) -> concatCols(f**id_(target g),g)
Matrix | RingElement := (f,g) -> concatCols(f,g**id_(target f))
Number | Matrix := (f,g) -> concatCols(f*id_(target g),g)
Matrix | Number := (f,g) -> concatCols(f,g*id_(target f))

Matrix || Matrix := Matrix => (f,g) -> concatRows(f,g)
RingElement || Matrix := (f,g) -> concatRows(f**id_(source g),g)
     -- we would prefer for f**id_(source g) to have the exact same source as g does
Matrix || RingElement := (f,g) -> concatRows(f,g**id_(source f))
Number || Matrix := (f,g) -> concatRows(f*id_(source g),g)
Matrix || Number := (f,g) -> concatRows(f,g*id_(source f))

-----------------------------------------------------------------------------
-- submatrix, submatrixByDegrees
-----------------------------------------------------------------------------
Matrix _ List := Matrix => (f,v) -> submatrix(f,listZ splice v)	-- get some columns
Matrix ^ List := Matrix => (f,v) -> submatrix(f,listZ splice v,) -- get some rows

Matrix _ ZZ := Vector => (m,i) -> (
     R := ring m;
     M := target m;
     h := m_{i};
     h = map(M, R^1, h, Degree => first degrees source h);
     new target h from {h})

-- given a map of free modules, find a submatrix of it
submatrixFree = (m, rows, cols) -> map(ring m, if rows === null
    then rawSubmatrix(raw cover m, listZZ cols)
    else rawSubmatrix(raw cover m, listZZ rows,
	if cols =!= null then listZZ cols else 0 .. numgens source m - 1))
-- given a module, find a part of the ambient module
-- along with corresponding generators and relations
sliceModule = (M, rows) -> (
    rows = listZZ rows;
    if rows === toList(0 .. numgens M - 1) then M else
    if isFreeModule M    then (ring M)^((-degrees M)_rows) else
    if not M.?relations  then image    submatrixFree(generators M, , rows) else
    if not M.?generators then cokernel submatrixFree(relations  M, rows, ) else
    subquotient(submatrixFree(generators M, , rows), relations M))

submatrix  = method(TypicalValue => Matrix)
submatrix' = method(TypicalValue => Matrix)

submatrix(Matrix, VisibleList, VisibleList) := (m, rows, cols) -> map(sliceModule(target m, rows), sliceModule(source m, cols), raw submatrixFree(m, rows, cols))
submatrix(Matrix, VisibleList, Nothing)     := (m, rows, null) -> map(sliceModule(target m, rows), source m,                    raw submatrixFree(m, rows, null))
submatrix(Matrix, VisibleList)              := (m,       cols) -> map(target m,                    sliceModule(source m, cols), raw submatrixFree(m, null, cols))
submatrix(Matrix, Nothing,     VisibleList) := (m, null, cols) -> submatrix(m, cols)
submatrix(Matrix, Nothing,     Nothing)     := (m, null, null) -> m

compl := (M, rows) -> if #(rows = listZZ rows) > 0 then toList(0 .. numgens M - 1) - set rows
submatrix'(Matrix, VisibleList, VisibleList) := (m, rows, cols) -> submatrix(m, compl(target m, rows), compl(source m, cols))
submatrix'(Matrix, VisibleList, Nothing)     := (m, rows, null) -> submatrix(m, compl(target m, rows), null)
submatrix'(Matrix, VisibleList)              := (m,       cols) -> submatrix(m, null, compl(source m, cols))
submatrix'(Matrix, Nothing,     VisibleList) := (m, null, cols) -> submatrix'(m, cols)
submatrix'(Matrix, Nothing,     Nothing)     := (m, null, null) -> m

submatrixByDegrees = method()
submatrixByDegrees(Matrix, Sequence, Sequence) := (m, tarBox, srcBox) -> (
    -- tarBox should be a sequence e.g. ({1},{4}), or ({1,2,3},{1,3,4}), or (, {1}) or ({1}, )
    -- each null is same as {}, meaning either no lower bound, or no higher bound.
    -- a list of length 1 is the same as an integer: e.g. ({1}, {4}) is same as (1,4).
    -- first: check both source and target are free modules
    -- check: length of lodegree (resp hidegree) is 0 or same as degree rank
    (tarL, tarH) := tarBox;
    (srcL, srcH) := srcBox;
    if tarL === null then tarL = {};
    if tarH === null then tarH = {};
    if srcL === null then srcL = {};
    if srcH === null then srcH = {};
    if instance(tarL, ZZ) then tarL = {tarL};
    if instance(tarH, ZZ) then tarH = {tarH};
    if instance(srcL, ZZ) then srcL = {srcL};
    if instance(srcH, ZZ) then srcH = {srcH};
    ndegs := numgens degreesRing ring m;
    if (#tarL > 0 and #tarL != ndegs)
      or
       (#tarH > 0 and #tarH != ndegs)
      or 
       (#srcL > 0 and #srcL != ndegs)
      or
       (#srcH > 0 and #srcH != ndegs)
       then error ("expected degree vector of length "|ndegs);
    tar := target m;
    src := source m;
    if not isFreeModule tar or not isFreeModule src then error "expected a matrix between free modules";
    ptar := rawSelectByDegrees(raw tar, tarL, tarH);
    psrc := rawSelectByDegrees(raw src, srcL, srcH);
    submatrix(m, ptar, psrc)
    )
submatrixByDegrees(Matrix, ZZ, ZZ) := (m, lo, hi) -> submatrixByDegrees(m, ({lo},{lo}), ({hi},{hi}))
submatrixByDegrees(Matrix, List, List) := (m, lo, hi) -> submatrixByDegrees(m, (lo,lo), (hi,hi))

-----------------------------------------------------------------------------

bothFree := (f,g) -> (
     if not isFreeModule source f or not isFreeModule target f
     or not isFreeModule source g or not isFreeModule target g then error "expected a homomorphism between free modules"
     else (f,g))

diff(Matrix, Matrix) := Matrix => ( (f,g) -> map(ring f, rawMatrixDiff(f.RawMatrix, g.RawMatrix)) ) @@ bothFree @@ toSameRing 
diff(RingElement, RingElement) := RingElement => (f,g) -> (diff(matrix{{f}},matrix{{g}}))_(0,0)
diff(Matrix, RingElement) := (m,f) -> diff(m,matrix{{f}})
diff(RingElement, Matrix) := (f,m) -> diff(matrix{{f}},m)
diff(Vector, RingElement) := (v,f) -> (diff(matrix{v},matrix{{f}}))_0
diff(RingElement, Vector) := (f,v) -> diff(matrix{{f}},transpose matrix{v})
diff(Vector, Vector) := (v,w) -> diff(matrix{v}, transpose matrix{w})
diff(Matrix, Vector) := (m,w) -> diff(m,transpose matrix {w})
diff(Vector, Matrix) := (v,m) -> diff(matrix {v}, m)

contract(Matrix, Matrix) := Matrix => ( (f,g) -> map(ring f, rawMatrixContract(f.RawMatrix, g.RawMatrix)) ) @@ bothFree @@ toSameRing
contract(RingElement, RingElement) := RingElement => (f,g) -> (contract(matrix{{f}},matrix{{g}}))_(0,0)
contract(Matrix, RingElement) := (m,f) -> contract(m,matrix{{f}})
contract(RingElement, Matrix) := (f,m) -> contract(matrix{{f}},m)
contract(Vector, RingElement) := (v,f) -> (contract(matrix{v},matrix{{f}}))_0
contract(RingElement, Vector) := (f,v) -> contract(matrix{{f}},transpose matrix{v})
contract(Vector, Vector) := (v,w) -> contract(matrix{v}, transpose matrix{w})
contract(Matrix, Vector) := (m,w) -> contract(m,transpose matrix {w})
contract(Vector, Matrix) := (v,m) -> contract(matrix {v}, m)

contract(Number, Number) := Number => (f,g) -> (contract(matrix{{f}},matrix{{g}}))_(0,0)
contract(RingElement, Number) := RingElement => (f,g) -> (contract(matrix{{f}},matrix{{g}}))_(0,0)
contract(Number, RingElement) := RingElement => (f,g) -> (contract(matrix{{f}},matrix{{g}}))_(0,0)
contract(Matrix, Number) := (m,f) -> contract(m,matrix{{f}})
contract(Number, Matrix) := (f,m) -> contract(matrix{{f}},m)
contract(Vector, Number) := (v,f) -> (contract(matrix{v},matrix{{f}}))_0
contract(Number, Vector) := (f,v) -> contract(matrix{{f}},transpose matrix{v})

diff'(Matrix, Matrix) := Matrix => ((m,n) -> ( flip(dual target n, target m) * diff(n,m) * flip(source m, dual source n) )) @@ bothFree @@ toSameRing
contract'(Matrix, Matrix) := Matrix => ((m,n) -> ( flip(dual target n, target m) * contract(n,m) * flip(source m, dual source n) )) @@ bothFree @@ toSameRing

jacobian = method()
jacobian Matrix := Matrix => (m) -> diff(transpose vars ring m, m)
jacobian Ring := Matrix => (R) -> jacobian presentation R ** R
jacobian RingElement := Matrix => f -> jacobian matrix {{f}}

leadTerm(ZZ, Matrix) := Matrix => (i,m) -> (
     map(target m, source m, rawInitial(i,m.RawMatrix)))

leadTerm(ZZ, RingElement) := RingElement => (i,f) -> (leadTerm(i,matrix{{f}}))_(0,0)

leadTerm(Matrix) := Matrix => m -> (
     map(target m, source m, rawInitial(-1,m.RawMatrix)))

borel Matrix := Matrix => m -> generators borel monomialIdeal m

clean(RR,Matrix) := (epsilon,M) -> map(target M, source M, clean(epsilon,raw M))
norm(RR,Matrix) := (p,M) -> new RR from norm(p,raw M)
norm(InfiniteNumber,Matrix) := (p,M) -> (
     prec := precision M;
     if prec === infinity then (
	  error "expected a matrix over RR or CC";
	  )
     else (
     	  norm(numeric(prec,p), M)
	  )
     )
norm(Matrix) := (M) -> (
     prec := precision M;
     if prec === infinity then (
	  error "expected a matrix over RR or CC";
	  )
     else (
     	  norm(numeric(prec,infinity), M)
	  )
     )

numRows Matrix := M -> numgens cover target M
numColumns Matrix := M -> numgens cover source M

--------------------------------------------------------------------------
------------------------ matrix and map for modules ----------------------
--------------------------------------------------------------------------

rawSetDegree = (M,N,m,d) -> rawMatrixRemake2(M, N, d, m, 0)

map(Module) := Matrix => opts -> (M) -> map(M,M,1,opts)

map(Module,ZZ,ZZ) := Matrix => opts -> (M,n,i) -> map(M,(ring M)^n,i)
map(Module,Module,ZZ) := Matrix => opts -> (M,N,i) -> (
     local R;
     if i === 0 then (
	  R = ring M;
	  (M',N') := (raw cover M, raw cover N);
	  r := rawZero(M', N',0);
	  deg := opts.Degree;
	  if deg =!= null then r = rawSetDegree(M', N', r, degreeCheck(deg,R));
	  new Matrix from {
	       symbol RawMatrix => r,
	       symbol source => N,
	       symbol target => M,
	       symbol ring => R,
	       symbol cache => new CacheTable
	       })
     else if i === 1 and M === N then (
	  R = ring M;
	  M' = raw cover M;
	  r = reduce(M, rawIdentity(M',0));
	  deg = opts.Degree;
	  if deg =!= null then r = rawSetDegree(M', M', r, degreeCheck(deg,R));
	  r = new Matrix from {
	       symbol RawMatrix => r,
	       symbol source => M,
	       symbol target => M,
	       symbol ring => R,
	       symbol cache => new CacheTable
	       };
	  r.cache.inverse = r;
	  r)
     else if numgens cover M == numgens cover N then map(M,N,i * id_(cover M),opts) 
     else error "expected 0, or source and target with same number of generators")

map(Module,Module,Number) := 
map(Module,Module,RingElement) := Matrix => opts -> (M,N,r) -> (
     if r == 0 then map(M,N,0,opts)
     else if numgens cover M == numgens cover N then map(M,N,r * id_(cover M),opts) 
     else error "expected 0, or source and target with same number of generators")

map(Module,Nothing,Matrix) := Matrix => opts -> (M,f) -> (
     if opts.Degree =!= null then error "expected no Degree option";
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

isSubquotient = method()
isSubquotient(Module,Module) := (M,N) -> (
     ring M === ring N
     and
     ambient M === ambient N
     and
     (generators M | relations M) % (generators N | relations N) == 0
     and
     relations N % relations M == 0
     )

-----------------------------------------------------------------------------
-- inducedMap
-----------------------------------------------------------------------------

inducedMap = method (
     Options => {
	  Verify => true,
	  Degree => null 
	  })
-- TODO: hookify this, so people can add more application specific induced maps
inducedMap(Module, Module)          := Matrix => opts -> (M, N) -> (
    if ambient M =!= ambient N then error "inducedMap: expected modules with same ambient free module";
    -- e.g. avoid a gb computation for inducedMap(M, image basis(d, M))
    if N.cache.?Monomials and M === target N.cache.Monomials
    then map(M, N, N.cache.Monomials, Degree => opts.Degree)
    else inducedMap(M, N, id_(ambient N), opts))
inducedMap(Module, Nothing, Matrix) := Matrix => opts -> (M, N, f) -> (
    B := image f;
    -- e.g. avoid a gb computation for inducedMap(image f, , f)
    if M === target B.cache.Monomials
    then map(M, source B.cache.Monomials, B.cache.Monomials, Degree => opts.Degree)
    else inducedMap(M, source f, f, opts))
inducedMap(Module,Module,Matrix) := Matrix => opts -> (N',M',f) -> (
     N := target f;
     M := source f;
     if ring N' =!= ring M' or ring N' =!= ring f then error "inducedMap: expected modules and map over the same ring";
    if isFreeModule N and isFreeModule M and (
	N =!= ambient N' and rank N === rank ambient N' or
	M =!= ambient M' and rank M === rank ambient M')
     then f = map(N = ambient N', M = ambient M', f)
     else (
	if ambient N' =!= ambient N then error "inducedMap: expected new target and target of map provided to be subquotients of same free module";
	if ambient M' =!= ambient M then error "inducedMap: expected new source and source of map provided to be subquotients of same free module");
     c := runHooks((inducedMap, Module, Module, Matrix), (opts, N', M', f));
     (f', g, gbN', gbM) := if c =!= null then c else error "inducedMap: no method implemented for this type of input";
     if opts.Verify then (
	  if relations M % relations M' != 0 then error "inducedMap: expected new source not to have fewer relations";
	  if relations N % relations N' != 0 then error "inducedMap: expected new target not to have fewer relations";
	  if generators M' % gbM != 0 then error "inducedMap: expected new source not to have more generators";
	  if g % gbN' != 0 then error "inducedMap: expected matrix to induce a map";
	  if not isWellDefined f' then error "inducedMap: expected matrix to induce a well-defined map";
	  );
     f')
inducedMap(Nothing,Module,Matrix) := o -> (M,N,f) -> inducedMap(target f,N, f,o)
inducedMap(Nothing,Nothing,Matrix) := o -> (M,N,f) -> inducedMap(target f,source f, f,o)

addHook((inducedMap, Module, Module, Matrix), Strategy => Default, (opts, N', M', f) -> (
     N := target f;
     M := source f;
     gbM  := gb(M,  ChangeMatrix => true);
     gbN' := gb(N', ChangeMatrix => true);
     g := generators N * cover f * (generators M' // gbM);
     f' := g // gbN';
     f' = map(N',M',f',Degree => if opts.Degree === null then degree f else opts.Degree);
     (f', g, gbN', gbM)))

-- TODO: deprecate this in favor of isWellDefined
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

-----------------------------------------------------------------------------

vars Ring := Matrix => R -> (
     g := generators R;
     if R.?vars then R.vars else R.vars =
     map(R^1,,{g}))

generators Module := Matrix => opts -> M -> (
     if M.?generators then M.generators
     else if M.cache.?generators then M.cache.generators
     else M.cache.generators = id_(ambient M))

Module_* := M -> apply(numgens M, i -> M_i)

relations Module := Matrix => M -> (
     if M.?relations then M.relations 
     else (
	  R := ring M;
	  map(ambient M,R^0,0)
	  )
     )

degrees Matrix := f -> {degrees target f, degrees source f}

coverMap(Module) := Matrix => (M) -> map(M, cover M, id_(cover M))

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

degrees Ring := R -> degree \ generators R

leadComponent = method()
leadComponent Matrix := List => m -> nonnull for c to numColumns m - 1 list position(numRows m, r -> m_(r,c) != 0, Reverse => true)
leadComponent Vector := ZZ   => v -> try first leadComponent matrix v else null

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
