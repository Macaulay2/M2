 --		Copyright 1993-1999 by Daniel R. Grayson

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
submodule = method(SingleArgumentDispatch=>true, TypicalValue => Module)

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

generators Module := Matrix => M -> if M.?generators then M.generators else id_(ambient M)

relations Module := Matrix => M -> (
     if M.?relations then M.relations 
     else (
	  R := ring M;
	  map(ambient M,R^0,0)
	  )
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

net Module := M -> net (
     if M.?relations 
     then if M.?generators
     then new FunctionApplication from { subquotient, (M.generators, M.relations) }
     else new FunctionApplication from { cokernel, M.relations }
     else if M.?generators
     then new FunctionApplication from { image, M.generators }
     else if numgens M === 0 then 0
     else new Superscript from {ring M, numgens M}
     )

Module == Module := (M,N) -> (
     -- this code might not be the quickest - Mike should check it
     ring M === ring N
     -- and numgens ambient M === numgens ambient N 
     -- and ( callgg(ggisequal,M,N); eePopBool())  -- just checks the free modules
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

Vector = new Type of MutableHashTable
ring Vector := v -> ring class v

expression Vector := v -> (
     F := class v;
     R := ring v;
     if class R === PolynomialRing or class R === QuotientRing then (
	  M := monoid R;
	  A := coefficientRing R;
	  SparseMonomialVectorExpression { numgens ambient F, 
	       convert(
		    ConvertRepeat ConvertJoin(
			 ConvertInteger,
			 M.ConvertToExpression,
			 A.ConvertToExpression),
		    callgg(ggtonet, v))})
     else (
	  SparseVectorExpression { numgens ambient F, 
	       convert(
		    ConvertRepeat ConvertJoin(ConvertInteger,R.ConvertToExpression),
		    callgg(ggtonet, v))}))
toString Vector := x -> if (ring x).?newEngine then see x else toString expression x
net Vector := x -> if (ring x).?newEngine then see x else net expression x
Vector + Vector := Vector => (x,y) -> (
     M := class x;
     if M != class y then error "no method for '+'";
     sendgg(ggPush x, ggPush y, ggadd);
     new M)
Vector - Vector := Vector => (x,y) -> (
     M := class x;
     if M != class y then error "no method for '-'";
     sendgg(ggPush x, ggPush y, ggsubtract);
     new M)
- Vector := Vector => x -> (
     sendgg(ggPush x, ggnegate);
     new class x)

ZZ * Vector := Vector => (r,x) -> (
     R := ring x;
     F := class x;
     sendgg(ggPush R, ggPush r, ggfromint, ggPush x, ggmult);
     new F)

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

isHomogeneous Vector := x -> (
     sendgg(ggPush x, ggishomogeneous);
     eePopBool())
degree Vector := x -> (
     sendgg(ggPush x, ggdegree);
     eePopIntarray())
Vector == Vector := (x,y) -> (
     sendgg(ggPush x, ggPush y, ggisequal);
     eePopBool())
Vector == ZZ := (x,i) -> (
     if i == 0 then (
	  sendgg(ggPush x, ggiszero);
	  eePopBool())
     else (
	  error "no method for '=='"
	  ))
ZZ == Vector := (i,x) -> x == i
Vector _ ZZ := (v,i) -> (
     M := class v;
     R := ring M;
     n := numgens ambient M;
     if i < 0 or i >= n then error "subscript out of range for vector";
     sendgg( ggPush v, ggINT, gg i, ggelem);
     new R)

reduceit := M -> (
     if M.?relations then (
	  g := gb M;
	  sendgg(
	       ggPush g,
	       ggINT, gg 1, ggpick,  -- swap
	       ggreduce)))

RingElement * Vector := Vector => (r,x) -> (
     R := class r;
     M := class x;
     if R =!= ring M then error "ring element and vector involve different rings";
     sendgg(ggPush r, ggPush x, ggmult);
     reduceit M;
     new M)

new Vector := M -> (
     if not instance(M, Module) then error "expected a module";
     x := new MutableHashTable;
     x.handle = newHandle "";
     x)

new Vector from List := (M,w) -> (
     if class M =!= Module then error "expected a module";
     if # w != numgens M then error (
	  "expected a list of length '", toString numgens M, "'");
     R := ring M;
     w = apply(w, r -> if class r != R then R#0 + r else r);
     sendgg(
	  apply(w, ggPush),
	  ggPush M,
	  ggvector	  -- the sparse version of this is ggsparsevector
	  );
     reduceit M;
     new M);

new Module from Ring := (Module,R) -> (
     M := new Module of Vector;
     M.handle = newHandle ggdup;
     M.ring = R;
     if R.?newEngine then M.newEngine = true;
     M.numgens = (sendgg if R.?newEngine then ggrank else gglength; eePopInt());
     M#0 = (sendgg(ggPush M, ggzero); new M);
     M)

degrees Module := M -> (
     R := ring M;
     if M.?degrees then M.degrees
     else (
	  rk := numgens M;
	  nd := degreeLength R;
	  M.degrees = (
	       if nd == 0 
	       then toList (rk : {})
	       else (
	  	    sendgg ggPush M;
		    pack(eePopIntarray(), nd)))))

Module ^ ZZ := Module => (M,i) -> directSum (i:M)

Ring ^ List := Module => (
     (R,degs) -> (
	  degs = - splice degs;
	  if R.?Engine and R.Engine then (
	       ndegs := degreeLength R;
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
	       then (
		    n := #degs;
		    -- if R#?(symbol ^,n)
		    -- then R#(symbol ^,n)
		    -- else R#(symbol ^,n) = 
		    (
			 sendgg( ggPush R, ggPush n, ggfree);
	       		 new Module from R))
	       else (
		    -- if R#?(symbol ^, fdegs)
		    -- then R#(symbol ^, fdegs)
		    -- else R#(symbol ^, fdegs) = 
		    (
			 sendgg(ggPush R, ggPush fdegs, ggfree);
	       		 new Module from R)))
	  else error "non-engine free modules with degrees not implemented yet"
	  ))

components = method()
components(Vector) := (x) -> apply(numgens class x,i->x_i)

SparseDisplayThreshhold := 15

Ring ^ ZZ := Module => (
     (R,n) -> (
	  if R.?Engine and R.Engine
	  then (
	       -- if R#?(symbol ^,n) 
	       -- then R#(symbol ^,n)
	       -- else R#(symbol ^,n) = 
	       (
	       	    sendgg(ggPush R, ggPush n, ggfree);
     	       	    new Module from R)
	       )
	  else notImplemented()
	  )
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
     apply(#e, i -> (-1)^i * (1 - e#i)))
genera(Ring) := (R) -> genera R^1

rank Module := M -> (
     if isFreeModule M then numgens M 
     else if degreeLength ring M === 0 and isField ring M then numgens prune M
     else (
	  f := poincare M // poincare ring M;
	  T := (ring f)_0;
	  substitute(f,{T=>1})
	  )
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

AfterPrint Module := M -> (
     << endl;				  -- double space
     n := rank ambient M;
     << "o" << lineNumber() << " : "
     << ring M
     << " - module";
     if M.?generators then
     if M.?relations then << ", subquotient of " << ambient M
     else << ", submodule of " << ambient M
     else if M.?relations then << ", quotient of " << ambient M
     else if n > 0 then << ", free";
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

