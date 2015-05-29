 --		Copyright 1993-2002 by Daniel R. Grayson

-----------------------------------------------------------------------------
vector = method()
vector List := v -> (
     m := matrix apply(v, i -> {i});
     new target m from {m})

-----------------------------------------------------------------------------
-- Matrix

ModuleMap = new Type of HashTable
ModuleMap.synonym = "module map"

Matrix = new Type of ModuleMap
Matrix.synonym = "matrix"
raw Matrix := f -> f.RawMatrix
ring Matrix := f -> (
     S := ring target f;
     R := ring source f;
     if R =!= S then error "expected module map with source and target over the same ring";
     if f.?RingMap then error "expected module map with no ring map";
     R)
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
	  (ZZ, { QQ, RR', CC' }),
	  (QQ, { RR', CC' }),
	  (RR',{ RR', CC' }),
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

Vector = new Type of BasicList				    -- an instance v will have one entry, an n by 1 matrix m, with class v === target m
Vector.synonym = "vector"
Vector _ ZZ := (v,i) -> (ambient v#0)_(i,0)
net Vector := v -> net super first v
entries Vector := v -> entries ambient v#0 / first
norm Vector := v -> norm v#0
toExternalString Vector := 				    -- not quite right
toString Vector := v -> concatenate ( "vector ", toString entries super v )
ring Vector := v -> ring class v
module Vector := v -> target first v
leadTerm Vector := v -> new class v from leadTerm v#0
degree Vector := v -> (
     f := ambient v#0;
     first degrees source map(target f,,f))
matrix Vector := opts -> v -> v#0
new Matrix from Vector := (Matrix,v) -> v#0
new Vector from Matrix := (M,f) -> (
     if not isFreeModule source f or numgens source f =!= 1 then error "expected source to be free with rank 1";
     if M === Vector then error "expected a module";
     new target f from {f})

Number * Vector := RingElement * Vector := (r,v) -> new class v from {r * v#0}
Vector + Vector := (v,w) -> (
     if class v =!= class w then error "expected vectors from the same module";
     m := v#0 + w#0;
     new target m from {m})
Vector - Vector := (v,w) -> (
     if class v =!= class w then error "expected vectors from the same module";
     m := v#0 - w#0;
     new target m from {m})

Vector ** Vector := (v,w) -> (
     if ring v =!= ring w then error "expected vectors over the same ring";
     u := v#0 ** w#0;
     new target u from {u})

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
new Module from Sequence := (Module,x) -> (
     (R,rM) -> (
	  assert instance(R,Ring);
	  assert instance(rM,RawFreeModule);
	  new Module of Vector from hashTable {
     	       symbol cache => new CacheTable from { 
		    cache => new MutableHashTable	    -- this hash table is mutable, hence has a hash number that can serve as its age
		    },
     	       symbol RawFreeModule => rM,
     	       symbol ring => R,
     	       symbol numgens => rawRank rM
     	       })) x

degreesMonoid Module := GeneralOrderedMonoid => M -> degreesMonoid ring M
degreesRing Module := PolynomialRing => M -> degreesRing ring M
degreeLength Module := M -> degreeLength ring M
raw Module := M -> M.RawFreeModule
ring Module := M -> M.ring

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

degreeLength Module := M -> degreeLength ring M

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

toString Module := M -> (
     if M.?relations then (
	  if M.?generators
	  then "subquotient(" | toString M.generators | "," | toString M.relations | ")"
	  else "cokernel " | toString M.relations
	  )
     else (
	  if M.?generators
	  then "image " | toString M.generators
	  else (
	       if numgens M === 0
	       then "0"
	       else toString expression M
	       )
	  )
     )

toExternalString Module := M -> (
     if M.?relations then (
	  if M.?generators
	  then "subquotient(" | toExternalString M.generators | "," | toExternalString M.relations | ")"
	  else "cokernel " | toExternalString M.relations
	  )
     else (
	  if M.?generators
	  then "image " | toExternalString M.generators
	  else (
	       if all(degrees M, deg -> all(deg, zero)) 
	       then "(" | toString ring M | ")^" | numgens M
	       else "(" | toString ring M | ")^" | toExternalString (- degrees M)
	       )
	  )
     )

expression Module := M -> (
     if M.?relations 
     then if M.?generators
     then new FunctionApplication from { subquotient, (expression M.generators, expression M.relations) }
     else new FunctionApplication from { cokernel, expression M.relations }
     else if M.?generators
     then new FunctionApplication from { image, expression M.generators }
     else if numgens M === 0 then 0
     else new Power from {expression ring M, numgens M}
     )

-- net Module := M -> net expression M

net Module := M -> (
     -- we want compactMatrixForm to govern the matrix here, also.
     if M.?relations 
     then if M.?generators
     then net new FunctionApplication from { subquotient, (net M.generators, net M.relations) }
     else net new FunctionApplication from { cokernel, net M.relations }
     else if M.?generators
     then net new FunctionApplication from { image, net M.generators }
     else if numgens M === 0 then "0"
     else (
	  R := ring M;
	  net new Superscript from { if hasAttribute(R,ReverseDictionary) then getAttribute(R,ReverseDictionary) else expression R, numgens M}
	  )
     )

Module == Module := (M,N) -> (
     -- this code might not be the quickest - Mike should check it
     ring M === ring N
     and degrees ambient M === degrees ambient N
     and (
	  if M.?relations 
	  then N.?relations and (
	       -- if isHomogeneous N.relations and isHomogeneous M.relations
	       -- then gb N.relations == gb M.relations
	       -- else 
		    (
		    -- temporary
		    isSubset(image M.relations, image N.relations)
		    and
		    isSubset(image N.relations, image M.relations)
		    )
	       )
     	  else not N.?relations
	  )
     and (
	  if M.?generators then (
	       if N.?generators then (
		    f := (
			 if M.?relations 
			 then M.relations|M.generators
		    	 else M.generators);
		    g := (
			 if N.?relations
			 then N.relations|N.generators
			 else N.generators);
		    -- if isHomogeneous f and isHomogeneous g
		    -- then gb f == gb g
		    -- else 
			 (
			 -- temporary
		    	 isSubset(image f, image g)
		    	 and
		    	 isSubset(image g, image f)
			 )
		    )
	       else (
		    f = (
			 if M.?relations
			 then M.relations|M.generators
			 else M.generators
			 );
		    if isHomogeneous f then f = substitute(f,0);
		    isSubset(ambient N, image f)))
	  else (
	       if N.?generators then (
		    g = (
			 if N.?relations 
			 then N.relations|N.generators 
			 else N.generators
			 );
		    if isHomogeneous g then g = substitute(g,0);
		    isSubset(ambient M, image g))
	       else true)))

degrees Module := N -> if N.?degrees then N.cache.degrees else N.cache.degrees = (
     if not isFreeModule N then N = cover N;
     rk := numgens N;
     R := ring N;
     nd := degreeLength R;
     if nd == 0 then toList (rk : {})
     else pack(nd,rawMultiDegree N.RawFreeModule))

Module ^ ZZ := Module => (M,i) -> directSum (i:M)

Ring ^ List := Module => (
     (R,degs) -> (
	  degs = - splice degs;
	  if R.?RawRing then (
	       -- check the args
	       ndegs := degreeLength R;
	       if #degs === 0 then ()
	       else if all(degs,i -> class i === ZZ) then (
		    if ndegs =!= 1
	       	    then error ("expected each multidegree to be of length ", toString ndegs))
	       else if all(degs,v -> class v === List) then (
		    scan(degs,v -> (
			      if #v =!= ndegs
			      then error (
				   "expected each multidegree to be of length ",
				   toString ndegs
				   );
			      if not all(v,i->class i === ZZ)
			      then error "expected each multidegree to be a list of integers")))
	       else error "expected a list of integers or a list of lists of integers";
	       -- then adjust the args
	       fdegs := flatten degs;
	       -- then do it
	       if # fdegs === 0 
	       then new Module from (R,rawFreeModule(R.RawRing,#degs))
	       else new Module from (R,rawFreeModule(R.RawRing,toSequence fdegs))
	       )
	  else error "non-engine free modules with degrees not implemented yet"
	  ))

SparseDisplayThreshhold := 15

Ring ^ ZZ := Module => (R,n) -> if R.?RawRing then new Module from (R, rawFreeModule(R.RawRing,n)) else notImplemented()

InexactFieldFamily ^ ZZ := Module => (T,n) -> (default T)^n

schreyerOrder = method()
schreyerOrder Module := Matrix => (F) -> (
     if not isFreeModule F then error "expected a free module";
     m := rawGetSchreyer F.RawFreeModule;
     src := new Module from (ring F, rawSource m);
     tar := new Module from (ring F, rawTarget m);
     map(tar,src,m))

schreyerOrder Matrix := Matrix => (m) -> map(target m, new Module from (ring m, rawSchreyerSource raw m), m)
schreyerOrder RawMatrix := RawMatrix => (m) -> rawMatrixRemake2(rawTarget m, rawSchreyerSource m, rawMultiDegree m, m, 0)

euler Module := (M) -> euler hilbertPolynomial M
euler Ring := (R) -> euler R^1

eulers Module := (M) -> (
     h := hilbertPolynomial M;
     apply(toList ( 0 .. dim h ), i -> euler diff(h,i) ))
eulers Ring := (R) -> eulers R^1

genera Module := (M) -> (
     e := eulers M;
     d := dim M - 1;
     apply(#e, i -> (-1)^(i+d) * (e#i - 1)))
genera Ring := (R) -> genera R^1

genus Module := (M) -> (
     e := euler M;
     d := dim M - 1;
     (-1)^d * (e - 1))
genus Ring := (R) -> genus R^1

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
	  else degree M
	       ))

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

End = (M) -> Hom(M,M)

Module#{Standard,AfterPrint} = M -> (
     << endl;				  -- double space
     n := rank ambient M;
     << concatenate(interpreterDepth:"o") << lineNumber << " : "
     << ring M
     << "-module";
     if M.?generators then
     if M.?relations then << ", subquotient of " << ambient M
     else << ", submodule of " << ambient M
     else if M.?relations then << ", quotient of " << ambient M
     else if n > 0 then (
	  << ", free";
	  if not all(degrees M, d -> all(d, zero)) 
	  then << ", degrees " << runLengthEncode if degreeLength M === 1 then flatten degrees M else degrees M;
	  );
     << endl;
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
