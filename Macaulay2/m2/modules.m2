 --		Copyright 1993-2002 by Daniel R. Grayson

-----------------------------------------------------------------------------
vector = (v) -> (
     if # v === 0
     then error "expected a nonempty list";
     scan(v,
	  r -> (
	       if not instance(class r,Ring)
	       and class r != Symbol
     	       then error "expected a list of ring elements"));
     if not uniform(v) 
     then try (
	  z := sum unique apply(v,x -> 0_(class x));
	  v = apply(v, x -> x + z);
	  )
     else error "can't promote all elements to the same ring";
     R := class first v;
     M := R^(# v);
     new M from v)

-----------------------------------------------------------------------------
-- BasicModule = new Type of Type
Module = new Type of Type
Module.synonym = "module"
raw Module := M -> M.RawFreeModule

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
isIdeal Module := M -> isSubmodule M and rank ambient M === 1

numgens Module := M -> (
     if M.?generators then numgens M.generators.source
     else if M.?relations then numgens M.relations.target
     else M.numgens
     )

toString Module := M -> if M.?name then M.name else (
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

toExternalString Module := M -> if M.?name then M.name else (
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

Vector = new Type of HashTable
Vector.synonym = "vector"
ring Vector := v -> ring class v
raw Vector := v -> v.RawVector

entries Vector := v -> (
     R := ring v;
     apply( entries v.RawVector, r -> new R from r )
     )

expression Vector := v -> (
     F := class v;
     R := ring v;
     n := numgens ambient F;
     SparseVectorExpression { n, apply(0 .. n-1, entries v, identity) })
toString Vector := x -> toString expression x
net Vector := x -> net expression x
Vector + Vector := Vector => (x,y) -> (
     M := class x;
     if M != class y then error "no method for '+'";
     new M from x.RawVector + y.RawVector
     )
Vector - Vector := Vector => (x,y) -> (
     M := class x;
     if M != class y then error "no method for '-'";
     new M from x.RawVector - y.RawVector)
- Vector := Vector => x -> (
     M := class x;
     new M from - x.RawVector)

ZZ * Vector := Vector => (r,x) -> (
     F := class x;
     new F from r * x.RawVector)

leadTerm Vector := RingElement => x -> (
     R := ring x;
     F := class x;
     sendgg(ggPush x, ggleadterm);
     new F);

leadComponent Vector := RingElement => x -> (
     R := ring x;
     sendgg(ggPush x, ggleadcomp);
     eePopInt());

leadCoefficient Vector := RingElement => x -> (
     k := coefficientRing ring x;
     sendgg(ggPush x, ggleadcoeff);
     k.pop())

leadMonomial Vector := MonoidElement => x -> (
     R := ring x;
     M := monoid R;
     sendgg(ggPush x, ggleadmonom);
     M.pop())

isHomogeneous Vector := x -> rawIsHomogeneous x.RawVector;
degree Vector := x -> (
     d := rawMultiDegree x.RawVector;
     R := ring x;
     if R.?Repair then R.Repair d else d
     )
Vector == Vector := (x,y) -> x.RawVector === y.RawVector;
Vector == ZZ := (x,i) -> (
     if i == 0 then rawIsZero x.RawVector
     else error "comparison with nonzero integer"
     )
ZZ == Vector := (i,x) -> x == i
Vector _ ZZ := (v,i) -> new ring v from rawVectorEntry(v.RawVector,i)

RingElement * Vector := Vector => (r,x) -> (
     M := class x;
     y := r.RawRingElement * x.RawVector;
     if M.?relations then (
	  -- reduce y modulo a gb for the relations
	  g := gb M;
	  error "not re-implemented yet";
	  );
     new M from y)

Vector * RingElement := Vector => (x,r) -> (
     M := class x;
     y := x.RawVector * r.RawRingElement;
     if M.?relations then (
	  -- reduce y modulo a gb for the relations
	  g := gb M;
	  error "not re-implemented yet";
	  );
     new M from y)

new Vector from RawVector := (M,v) -> new M from { symbol RawVector => v }

newModule = method(TypicalValue => Module)
newModule(Ring,RawFreeModule) := (R,rM) -> (
     M := new Module of Vector;
     M.RawFreeModule = rM;
     M.ring = R;
     M.numgens = rawRank rM;
     M#0 = new M from rawZero rM;
     M)

degrees Module := M -> if M.?degrees then M.degrees else M.degrees = (
     if not isFreeModule M then M = cover M;
     rk := numgens M;
     R := ring M;
     nd := degreeLength R;
     if nd == 0 then toList (rk : {})
     else (
	  d := pack(nd,rawMultiDegree M.RawFreeModule);
	  if R.?Repair then d = apply(d,R.Repair);
	  d))

Module ^ ZZ := Module => (M,i) -> directSum (i:M)

Ring ^ List := Module => (
     (R,degs) -> (
	  degs = - splice degs;
	  if R.?RawRing then (
	       ndegs := degreeLength R;
	       if R.?Adjust then degs = apply(degs,R.Adjust);
	       fdegs := flatten degs;
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
	       if # fdegs === 0 
	       then newModule(R,rawFreeModule(R.RawRing,#degs))
	       else newModule(R,rawFreeModule(R.RawRing,toSequence fdegs))
	       )
	  else error "non-engine free modules with degrees not implemented yet"
	  ))

components = method()
components(Vector) := (x) -> apply(numgens class x,i->x_i)

SparseDisplayThreshhold := 15

Ring ^ ZZ := Module => (R,n) -> (
     if R.?RawRing
     then newModule (R, rawFreeModule(R.RawRing,n))
     else notImplemented()
     )

-- euler(Module) := (M) -> (
--      f := poincare M;
--      R := ring M;
--      N := numgens R - 1;
--      u := symbol u;
--      G := group [u];
--      U := u;
--      use ZZ G;
--      h := U^-N * (substitute(f,{(ring f)_0 => 1-u})) * (sum(N+1,i->U^i));
--           -- f might have negative exponents in it here!
--      c := toList apply(0 .. N, i -> h_(U^-i));
--      k := position(reverse c, j -> j != 0);
--      if k === null then k = N+1;
--      c = drop(c,-k);
--      << "sectional euler characteristics:" << endl;
--      scan(#c, i -> << " " << toString i || "" || toString c#i);
--      << endl;
--      )
euler(Module) := (M) -> (
     h := hilbertPolynomial M;
     apply(toList ( 0 .. dim h ), i -> euler diff(h,i) ))
euler(Ring) := (R) -> euler R^1

genera(Module) := (M) -> (
     e := euler M;
     d := dim M - 1;
     apply(#e, i -> (-1)^(i+d) * (e#i - 1)))
genera(Ring) := (R) -> genera R^1
genus(Ring) := R -> first genera R

rank Module := M -> (
     if isFreeModule M then numgens M 
     else if degreeLength ring M === 0 and isField ring M then numgens prune M
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
     << "o" << lineNumber() << " : "
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
     if M.?isHomogeneous 
     then M.isHomogeneous 
     else M.isHomogeneous = (
     	  (not M.?generators or isHomogeneous M.generators)
     	  and
     	  (not M.?relations or isHomogeneous M.relations)
	  )
     ))

degreesRing Module := M -> degreesRing ring M
degreeLength Module := M -> degreeLength ring M
