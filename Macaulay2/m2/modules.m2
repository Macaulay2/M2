 --		Copyright 1993-2002 by Daniel R. Grayson

-----------------------------------------------------------------------------
vector = (v) -> (
     m := matrix apply(v, i -> {i});
     new target m from {m})
-----------------------------------------------------------------------------
-- BasicModule = new Type of Type
ImmutableType = new Type of HashTable
Module = new Type of ImmutableType
degreesRing Module := M -> degreesRing ring M
degreeLength Module := M -> degreeLength ring M
new Module of HashTable from List := (Module,Type,v) -> hashTable v
Module.synonym = "module"
raw Module := M -> M.RawFreeModule
ring Module := M -> M.ring

globalAssignment ImmutableType

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
	       else toString ring M | "^" | toString numgens M
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
	       if numgens M === 0
	       then "0"
	       else toString ring M | "^" | toString (- degrees M)
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
     else new Superscript from {expression ring M, numgens M}
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
     else net new Superscript from {net ring M, numgens M}
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

-----------------------------------------------------------------------------

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

Vector = new Type of BasicList				    -- an instance v will have one entry, an n by 1 matrix m, with class v === target m
Vector.synonym = "vector"
Vector _ ZZ := (v,i) -> (ambient v#0)_(i,0)
net Vector := v -> net first v
entries Vector := v -> entries ambient v#0 / first
toExternalString Vector := 				    -- not quite right
toString Vector := v -> concatenate ( "vector ", toString entries v )
ring Vector := v -> ring class v
module Vector := v -> target first v
leadTerm Vector := v -> new class v from leadTerm v#0
degree Vector := v -> (
     f := ambient v#0;
     first degrees source map(target f,,f))
new Matrix from Vector := (Matrix,v) -> v#0
new Vector from Matrix := (M,f) -> (
     if not isFreeModule source f or numgens source f =!= 1 then error "expected source to be free with rank 1";
     if M === Vector then error "expected a module";
     new target f from {f})

RingElement * Vector := (r,v) -> new class v from {r * v#0}
Vector + Vector := (v,w) -> (
     m := v#0 + w#0;
     new target m from {m})
Vector - Vector := (v,w) -> (
     m := v#0 - w#0;
     new target m from {m})
Vector == Vector := (v,w) -> v === w

Ring * RingElement := (R,f) -> (
     if ring f === R then ideal(f)
     else ideal(promote(f,R)))
Ring * Vector := (R,v) -> (
     if ring v =!= R then error "different rings encountered";
     image v#0
     )
isHomogeneous Vector := (v) -> isHomogeneous v#0

newModule = method()
newModule(Ring,RawFreeModule) := (R,rM) -> new Module of Vector from {
     symbol cache => new CacheTable,
     symbol RawFreeModule => rM,
     symbol ring => R,
     symbol numgens => rawRank rM
     }

degrees Module := N -> if N.?degrees then N.cache.degrees else N.cache.degrees = (
     if not isFreeModule N then N = cover N;
     rk := numgens N;
     R := ring N;
     nd := degreeLength R;
     ind := if R.?monoid then ind = R.monoid.internalDegreeLength else nd;
     if nd == 0 then toList (rk : {})
     else (
       	  d := pack(ind,rawMultiDegree N.RawFreeModule);
       	  if R.?Repair then d = apply(d,R.Repair);
       	  d))

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
	       if R.?Adjust then degs = apply(degs,R.Adjust);
	       fdegs := flatten degs;
	       -- then do it
	       if # fdegs === 0 
	       then newModule(R,rawFreeModule(R.RawRing,#degs))
	       else newModule(R,rawFreeModule(R.RawRing,toSequence fdegs))
	       )
	  else error "non-engine free modules with degrees not implemented yet"
	  ))

SparseDisplayThreshhold := 15

Ring ^ ZZ := Module => (R,n) -> (
     if R.?RawRing
     then newModule (R, rawFreeModule(R.RawRing,n))
     else notImplemented()
     )

schreyerOrder = method()
schreyerOrder Module := Matrix => (F) -> (
     if not isFreeModule F then error "expected a free module";
     m := rawGetSchreyer F.RawFreeModule;
     src := newModule(ring F, rawSource m);
     tar := newModule(ring F, rawTarget m);
     map(tar,src,m))

schreyerOrder Matrix := Matrix => (m) -> map(target m, newModule(ring m, rawSchreyerSource raw m), m)
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

rank Module := M -> (
     if isFreeModule M then numgens M 
     else if degreeLength ring M === 0 and isField ring M then numgens minimalPresentation M
     else if dim M < dim ring M then 0
     else degree M // degree ring M
     )

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

Module.AfterPrint = M -> (
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
	  then << ", degrees " << if degreeLength M === 1 then flatten degrees M else degrees M;
	  );
     << endl;
     )

RingElement * Module := Module => ZZ * Module := (r,M) -> subquotient (r ** generators M, relations M)

isHomogeneous Module := Boolean => (M) -> ring M === ZZ or (
     isHomogeneous ring M and (
     if M.cache.?isHomogeneous 
     then M.cache.isHomogeneous 
     else M.cache.isHomogeneous = (
     	  (not M.?generators or isHomogeneous M.generators)
     	  and
     	  (not M.?relations or isHomogeneous M.relations)
	  )
     ))


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
