 --		Copyright 1993-2002 by Daniel R. Grayson

needs "monoids.m2"  -- for degreesMonoid
needs "reals.m2" -- for inexact number
needs "gateway.m2" -- for id

-----------------------------------------------------------------------------
-- Matrix

Matrix = new Type of HashTable
Matrix.synonym = "matrix"
raw Matrix := f -> f.RawMatrix
ring Matrix := f -> (
     S := ring target f;
     R := ring source f;
     if R =!= S then error "expected module map with source and target over the same ring";
     if f.?RingMap then error "expected module map with no ring map";
     R)
-- FIXME: can't set the typical output type because Module isn't defined yet!
source Matrix := f -> f.source
target Matrix := f -> f.target

precision Matrix := precision @@ ring

lift(Matrix,InexactNumber) := opts -> (M,RR) -> lift(M,default RR,opts)
lift(Matrix,InexactNumber') :=
lift(Matrix,RingElement) := 
lift(Matrix,Number) := Matrix => opts -> (f,S) -> (
     R := ring f;
     if R === S then return f;
     if not isFreeModule target f or not isFreeModule source f then error "lift: expected source and target to be free modules";
     lift(f, R, S, opts))     

promote(Matrix,InexactNumber) := (M,RR) -> promote(M,default RR)
promote(Matrix,InexactNumber') :=
promote(Matrix,RingElement) := 
promote(Matrix,Number) := Matrix => (f,S) -> (
     R := ring f;
     if R === S then return f;
     if not isFreeModule target f or not isFreeModule source f then error "promote: expected source and target to be free modules";
     promote(f, R, S))

scan( {ZZ,QQ}, K -> (
	  promote(List,K,K) := (m,K,L) -> m;
	  promote(Matrix,K,K) := (m,K,L) -> m;
	  lift(Matrix,K,K) := opts -> (m,K,L) -> m;
	  ))

scan((
	  (ZZ, { QQ, RR', CC', RRi' }),
	  (QQ, { RR', CC', RRi' }),
	  (RR',{ RR', CC', RRi' }),
	  (CC', { CC' })
	  ), 
     (K,Ls) -> scan(Ls, L -> (
	       p := makepromoter 0;
	       promote(Matrix,K,L) := (m,K,L) -> basicPromoteMatrix(m,L,p);
	       promote(List,K,L) := (m,K,L) -> m;
	       lift(Matrix,L,K) := opts -> (m,L,K) -> (basicLiftMatrix opts)(m,K,p);
	       )))	  

promote(Matrix,QQ,CC') := (m,K,L) -> promote( promote(m,RR_(precision L)), L) -- Mike could/should do this in the engine!

-----------------------------------------------------------------------------
-- Vector

vector = method()
vector Matrix := f -> (
     if not isFreeModule source f or numgens source f =!= 1 then error "expected source to be free with rank 1";
     new target f from {f}
    )
vector List := v -> vector matrix apply(splice v, i -> {i});
vector RingElement := vector Number := x -> vector {x}

-----------------------------------------------------------------------------

Vector = new Type of BasicList				    -- an instance v will have one entry, an n by 1 matrix m, with class v === target m
Vector.synonym = "vector"
Vector _ ZZ := (v,i) -> (ambient v#0)_(i,0)
entries Vector := v -> entries ambient v#0 / first
norm Vector := v -> norm v#0
expression Vector := v -> VectorExpression apply(flatten entries super v#0,expression)
net Vector := v -> net expression v
describe Vector := v -> Describe expression FunctionApplication(
    vector, (describe module v, describe \ flatten entries matrix v))
toExternalString Vector := toString @@ describe
toString Vector := v -> toString expression v
texMath Vector := v -> texMath expression v
--html Vector := v -> html expression v

Vector.AfterPrint = Vector.AfterNoPrint = v -> moduleAbbrv module v ?? Vector

ring Vector := v -> ring class v
module Vector := v -> target v#0
leadTerm Vector := v -> new class v from leadTerm v#0
leadTerm (ZZ, Vector) := (n,v) -> new class v from leadTerm(n,v#0)
degree Vector := v -> (
     f := ambient v#0;
     first degrees source map(target f,,f))
matrix Vector := opts -> v -> v#0
new Matrix from Vector := (Matrix,v) -> v#0
new Vector from Matrix := (M,f) -> (
     if not isFreeModule source f or numgens source f =!= 1 then error "expected source to be free with rank 1";
     if M =!= target f then error "module must be target of matrix";
     new M from {f})
super Vector := Vector => v -> vector super v#0

Vector || Vector := Vector => (v,w) -> vector(v#0||w#0)
Vector ^ List := (v,l) -> vector (v#0^l)

promote(Vector,InexactNumber) := 
promote(Vector,InexactNumber') :=
promote(Vector,RingElement) := 
promote(Vector,Number) := Vector => (v,S) -> vector (promote(v#0,S))
numeric Vector := v -> numeric(defaultPrecision,v)
numeric(ZZ,Vector) := (prec,v) -> (
     F := ring v;
     if instance(F, InexactField) then return v;
     if F === ZZ or F === QQ then return promote(v,RR_prec);
     error "expected vector of numbers"
     )
lift(Vector,InexactNumber) :=
lift(Vector,InexactNumber') :=
lift(Vector,RingElement) :=
lift(Vector,Number) := Vector => o -> (v,S) -> vector (lift(v#0,S))

+ Vector := Vector => identity
- Vector := Vector => v -> new class v from {-v#0}
Number * Vector := RingElement * Vector := Vector => (r,v) -> vector(r * v#0)
Vector * Number := Vector * RingElement := Vector => (v,r) -> vector(v#0 * r)
Vector / Number := Vector / RingElement := Vector => (v,r) -> vector(v#0 / r)
Vector + Vector := Vector => (v,w) -> vector(v#0+w#0)
Vector - Vector := Vector => (v,w) -> vector(v#0-w#0)
Vector ** Vector := Vector => (v,w) -> vector(v#0**w#0)

Vector == Vector := (v,w) -> v === w

Ring * RingElement := (R,f) -> (
     if ring f === R then ideal(f)
     else ideal(promote(f,R)))
Ring * Vector := (R,v) -> (
     if ring v =!= R then error "different rings encountered";
     image v#0
     )
isHomogeneous Vector := (v) -> isHomogeneous v#0

-----------------------------------------------------------------------------
-- Module

Module = new Type of ImmutableType
Module.synonym = "module"
new Module from List := (Module,v) -> new Module of Vector from hashTable v
new Module from (Ring, RawFreeModule) := (Module, R, rM) -> (
    new Module of Vector from hashTable {
	symbol cache => new CacheTable from {
	    cache => new MutableHashTable  -- this hash table is mutable, hence has a hash number that can serve as its age
	    },
	symbol RawFreeModule => rM,
	symbol ring => R,
	symbol numgens => rawRank rM
	})

vector(Module, Matrix) := (M, f) -> vector map(M,,entries f)
vector(Module, List)   := (M, v) -> vector map(M,,apply(splice v, i -> {i}))
vector(Module, RingElement) := vector(Module, Number) := (M, x) -> vector(M, {x})
vector(Ring,       Matrix)      :=
vector(RingFamily, Matrix)      := (R, f) -> vector(R^(numRows f), f)
vector(Ring,       List)        :=
vector(RingFamily, List)        := (R, v) -> vector(R^(#v), v)
vector(Ring,       Number)      :=
vector(Ring,       RingElement) :=
vector(RingFamily, Number)      :=
vector(RingFamily, RingElement) := (R, x) -> vector(R^1, {x})

Module#id = M -> map(M, M, 1)
raw Module := M -> M.RawFreeModule
ring Module := M -> M.ring
module Module := identity

lift(Module,InexactNumber) := opts -> (M,RR) -> lift(M,default RR,opts)
lift(Module,InexactNumber') :=
lift(Module,RingElement) := 
lift(Module,Number) := Module => opts -> (M,S) -> (
     R := ring M;
     if R === S then return M;
     if not isFreeModule M then error "lift: expected module to be free";
     lift(M, R, S, opts))     

promote(Module,InexactNumber) := (M,RR) -> promote(M,default RR)
promote(Module,InexactNumber') :=
promote(Module,RingElement) := 
promote(Module,Number) := Module => (M,S) -> (
     R := ring M;
     if R === S then return M;
     if not isFreeModule M then error "promote: expected module to be free";
     promote(M, R, S))

isModule = method(TypicalValue => Boolean)
isModule Thing := M -> false
isModule Module := M -> true

isFreeModule = method(TypicalValue => Boolean)
isFreeModule Thing := x -> false
isFreeModule Module := M -> not M.?relations and not M.?generators

isSubmodule = method(TypicalValue => Boolean)
isSubmodule Thing := x -> false
isSubmodule Module := M -> not M.?relations

isQuotientModule = method(TypicalValue => Boolean)
isQuotientModule Thing := x -> false
isQuotientModule Module := M -> not M.?generators

isIdeal = method(TypicalValue => Boolean)
isIdeal Thing := x -> false
isIdeal Module := M -> isSubmodule M and (
     F := ambient M;
     rank F === 1 and all(degree F_0, i -> i === 0)
     )

numgens Module := M -> (
     if M.?generators then numgens M.generators.source
     else if M.?relations then numgens M.relations.target
     else M.numgens
     )

expression Module := M -> (
     if M.?relations
     then if M.?generators
     then (expression subquotient) (expression (M.generators, M.relations))
     else (expression cokernel) (expression M.relations)
     else if M.?generators
     then (expression image) (expression M.generators)
     else (
	 n := numgens M;
	 new Superscript from {unhold expression ring M, if n =!= 0 then unhold expression n else moduleZERO }
     )
 )
toString Module := M -> toString expression M
net Module := M -> net expression M
texMath Module := M -> texMath expression M

-- returns null if can't shorten (free module or assigned to variable)
moduleAbbrv = M -> (
    if isFreeModule M then expression M
    else if hasAttribute(M, ReverseDictionary)
    then getAttribute(M, ReverseDictionary))

short Module := M -> moduleAbbrv M ?? expression M

describe Module := M -> Describe (
     if M.?relations
     then if M.?generators
     then (expression subquotient) (unhold describe M.generators, unhold describe M.relations)
     else (expression cokernel) (describe M.relations)
     else if M.?generators
     then (expression image) (describe M.generators)
     else new Superscript from {unhold expression ring M, if all(degrees M, deg -> all(deg, zero)) then expression numgens M
	 else expression runLengthEncode(-degrees M)}
     )
toExternalString Module := M -> toString describe M

-- TODO: where is it set before being cached?
degrees Module := -*(cacheValue symbol degrees) (*-N -> (
    r := degreeLength(R := ring N);
    if r == 0 then toList(numgens N : {}) else (
	degs := pack(r, rawMultiDegree raw cover N);
	if not (M := monoid R).?degreeGroup
	or isFreeModule(G := M.degreeGroup) then degs
	else apply(degs, reduceDegree_G)))
--    )

-----------------------------------------------------------------------------
-- free modules and vector spaces

Ring ^ ZZ   := Module => (R, n) -> (
    if not R.?RawRing then error "non-engine free modules with degrees not implemented yet";
    new Module from (R, rawFreeModule(R.RawRing, n)))

Ring ^ List := Module => (R, degs) -> (
    if not R.?RawRing then error "non-engine free modules with degrees not implemented yet";
    -- check the args
    degs = - splice degs;
    degrk := degreeLength R;
    if #degs === 0 then ()
    else if isListOfIntegers degs        then ( if degrk != 1
	then error("expected each multidegree to be of length ", degrk))
    else if isListOfListsOfIntegers degs then ( if any(degs, deg -> degrk != #deg)
	then error("expected each multidegree to be of length ", degrk))
    else error "expected a list of integers or a list of lists of integers";
    -- then flatten the args
    fdegs := toSequence flatten degs;
    new Module from (R, rawFreeModule(R.RawRing, if #fdegs === 0 then #degs else fdegs)))

RingFamily ^ ZZ   :=
RingFamily ^ List := Module => (T, degs) -> (default T)^degs

-----------------------------------------------------------------------------
-- Containment and Equality of Modules
-----------------------------------------------------------------------------

-- the key for issub hooks under GlobalHookStore
protect ContainmentHooks
issub = (f, g) -> f === g or ring f === ring g and tryHooks(ContainmentHooks, (f, g),
    -- This is used by isSubset and for checking equality of modules and ideals.
    -- Specialized strategies may be added as hooks, for instance for local rings.
    -- TODO: how can do better in the homogeneous case?
    (f, g) -> -1 === rawGBContains(raw gb g, raw f))

-- check equality of the column spans as sets
isequal := (f, g) -> f === g or issub(f, g) and issub(g, f)

-- gives generators for the entire coset of the module
cosetgens := M -> if M.?relations then M.relations | generators M else generators M

Module == Module := (M, N) -> M === N or ring M === ring N and tryHooks((symbol ==, Module, Module), (M, N),
    -- Specialized strategies for equality of modules may be added as hooks.
    -- The following is the default strategy using generators and relations.
    (M, N) -> (
	-- TODO: first look to see if the minimal presentation
	-- of M and N are cached, and if so compare them with ===
	-- check whether M and N are equal as cosets to zero,
	-- e.g. 0*S^1 == 0*S^2 but 0*S^1 != subquotient(|x|, |x|)
	cosetgens M == 0 and cosetgens N == 0 or
	-- check that ambient free modules are the same
	degrees ambient M === degrees ambient N
	and (
	    -- check that neither have relations
	    not M.?relations and not N.?relations
	    -- or that they have the same relations
	    or M.?relations and N.?relations and isequal(M.relations, N.relations)
	    )
	and (
	    -- check that both have generators and, together with their relations, their spans are equal
	    -- (e.g. this ensures that subquotient(|x|, |x2,y|) & subquotient(|x,y|,|x2,y|) are equal)
	    if M.?generators and N.?generators then isequal(cosetgens M, cosetgens N)
	    -- otherwise check that the generators are superfluous, i.e. image gens M == ambient M (or the same for N)
	    else if M.?generators then issub(generators N, if isHomogeneous M then substitute(cosetgens M, 0) else cosetgens M)
	    else if N.?generators then issub(generators M, if isHomogeneous N then substitute(cosetgens N, 0) else cosetgens N)
	    else true
	    )
	)
    )

protect isZero
ZZ == Module := (n, M) -> M == n
Module == ZZ := (M, n) -> M.cache.isZero ??= (
    if n =!= 0 then error "attempted to compare module to nonzero integer";
    -- the default strategy for issub computes a gb for the relations
    if M.?relations  then issub(generators M, M.relations) else
    if M.?generators then M.generators == 0 else M.numgens == 0)

isSubset(Module, Module) := (M, N) -> (
    -- here is where we could use gb of a subquotient!
    ambient M === ambient N and
    if  not M.?relations and not N.?relations then issub(generators M, generators N)
    else if M.?relations and     N.?relations then (
	isequal(M.relations, N.relations) and issub(generators M, generators N | N.relations))
    -- see the code for subquotient: if present, M.relations is nonzero; same for N
    -- so one of the modules has nonzero relations and the other doesn't
    else false)

-----------------------------------------------------------------------------

-- used for sorting a list of modules
Module ? Module := (M, N) -> if rank M != rank N then rank M ? rank N else degrees M ? degrees N

-----------------------------------------------------------------------------

schreyerOrder = method()
schreyerOrder Module := Matrix => (F) -> (
     if not isFreeModule F then error "expected a free module";
     m := rawGetSchreyer F.RawFreeModule;
     src := new Module from (ring F, rawSource m);
     tar := new Module from (ring F, rawTarget m);
     map(tar,src,m))

schreyerOrder Matrix := Matrix => (m) -> map(ring m, schreyerOrder raw m)
schreyerOrder RawMatrix := RawMatrix => (m) -> rawMatrixRemake2(rawTarget m, rawSchreyerSource m, rawMultiDegree m, m, 0)

possiblyLift := x -> if denominator x === 1 then numerator x else x -- x is in QQ

rank Module := (cacheValue symbol rank) (M -> (
	  R := ring M;
	  if isFreeModule M then numgens M 
	  else if isField R or R === ZZ then (
	       if M.?relations then (
		    if M.?generators then (
			 g := M.relations | M.generators;
			 h := M.relations;
			 numgens source generators gb g - numgens source generators gb h)
		    else (
	       		 p := M.relations;
	       		 numgens target p - numgens source generators gb p))
	       else (
		    if M.?generators then (
			 numgens source generators gb M.generators)
		    else numgens M))
	  else if dim M < dim ring M then 0
	  else possiblyLift( degree M / degree R )))

ambient Module := Module => M -> (
     if M.?generators then M.generators.target
     else if M.?relations then M.relations.target
     else M)

cover(Module) := Module => (M) -> (
     if M.?generators then M.generators.source
     else if M.?relations then M.relations.target
     else M)

super(Module) := Module => (M) -> (
     if M.?generators then (
     	  if M.?relations then cokernel M.relations
	  else M.generators.target
	  )
     else M
     )

-----------------------------------------------------------------------------

Module#AfterPrint = M -> (
    ring M,"-module",
    if M.?generators then
    if M.?relations then (", ",subquotient," of ",ambient M)
    else (", submodule of ",ambient M)
    else if M.?relations then (", ",quotient," of ",ambient M)
    else if rank ambient M > 0 then
    (", free",
	if not all(degrees M, d -> all(d, zero))
	then (", degrees ",runLengthEncode if degreeLength ring M === 1 then flatten degrees M else degrees M)
	)
    )

RingElement * Module := Module => ZZ * Module := (r,M) -> subquotient (r ** generators M, relations M)
Module * RingElement := Module => Module * ZZ := (M,r) -> subquotient ((generators M) ** r, relations M)

isHomogeneous Module := Boolean => (cacheValue symbol isHomogeneous) (
     (M) -> (
     	  isHomogeneous ring M 
	  and 
     	  (not M.?generators or isHomogeneous M.generators)
     	  and
     	  (not M.?relations or isHomogeneous M.relations)
	  ))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
