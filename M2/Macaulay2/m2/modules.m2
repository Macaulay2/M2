 --		Copyright 1994 by Daniel R. Grayson

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

document { quote vector,
     TT "vector {a,b,c,...}", " -- produces an element of a free module from a list.",
     PARA,
     "The elements a,b,c,... must be elements of the same ring, or be
     convertible to elements of the same ring."
     }

-----------------------------------------------------------------------------
Module = new Type of Type
document { quote Module,
     TT "Module", " -- the class of all modules which are handled
     by the ", TO "engine", ".",
     PARA,
     "The most general module M is represented as a submodule of a 
     quotient module of a free module F.  The matrix of relations used to
     produce the quotient module is stored as ",
     PRE "M.relations",
     " and the matrix of generators is stored as ", TT "M.generators", ".",
     PARA,
     "Functions which create modules:",
     MENU {
	  TO "Ring ^ ZZ",
	  TO "cokernel",
	  TO "homology",
	  TO "ideal",
	  TO "image",
	  TO "kernel"
	  },
     PARA,
     "Tests:",
     MENU {
	  TO "isFreeModule",
	  TO "isIdeal",
	  TO "isModule",
	  TO "isQuotientModule",
	  TO "isSubmodule"
	  },
     "Operations on modules:",
     MENU {
	  TO "==",
	  TO "M_i",
	  (TO "Module + Module", " -- the sum of submodules I, J"),
	  ("M ", TO "++", "N -- direct sum"),
	  ("M ", TO "**", "N -- tensor product"),
	  ("M ", TO ":", "N -- the submodule quotient M : N"),
	  ("M ", TO "/", "N -- the cokernel module (M+N)/N"),
	  TO "ambient",
	  (TO "annihilator", " M -- the annihilator of M"),
	  TO "codim",
	  TO "cover",
	  TO "degree",
	  TO "degrees",
	  TO "dim",
	  TO "dual",
	  TO "End",
	  TO "euler",
	  (TO "fittingIdeal", "(i,m) -- the i-th Fitting ideal of the module M"),
	  (TO "Ext", "^i(M,N) -- the ext module"),
	  TO "gcdDegree",
	  TO "genera",
	  TO "generators",
	  (TO "poincare", "(M,t) -- the numerator of the Hilbert series of M"),
	  (TO "hilbertFunction", "(d,M) -- the Hilbert function of a module."),
	  (TO "hilbertPolynomial", "(M) -- the Hilbert polynomial of a module"),
	  (TO "hilbertSeries", "(M) -- the Hilbert series of a module."),
	  (TO "Hom", "(M,N) -- the module of homomorphisms"),
	  (TO "intersect", "(I,J) -- intersection of modules"),
	  TO "lcmDegree",
	  TO "numgens",
	  TO "mingens",
	  TO "pdim",
	  (TO "presentation", " M -- a presentation matrix for M"),
	  (TO "prune", " M -- a minimal presentation for M"),
	  TO "quotient",
	  TO "rank",
	  TO "relations",
	  TO "removeLowestDimension",
	  (TO "resolution", "M -- a finite free resolution of M"),
	  TO "super",
	  TO "top",
  	  (TO "Tor", "_i(M,N) -- the tor module"),
	  (TO "trim", " -- replace generators and relations by minimal sets"),
	  TO "truncate"
     	  },
     "Operations on elements of modules:",
     MENU {
	  TO "+",
	  TO "-",
	  TO "*",
	  TO "components",
	  TO "leadCoefficient",
	  TO "leadMonomial"
	  },
     PARA,
     SEEALSO( "Vector")
     }


isModule = method()
isModule Thing := M -> false
isModule Module := M -> true

document { quote isModule,
     TT "isModule M", " -- tells whether its argument is a module."
     }

isFreeModule = method()
isFreeModule Thing := x -> false
isFreeModule Module := M -> not M.?relations and not M.?generators

document { quote isFreeModule,
     TT "isFreeModule M", " -- determine whether M is evidently a free module.  No
     computation is done.",
     PARA,
     "To determine whether M is isomorphic to a free module, one may prune
     M first.",
     EXAMPLE "R = ZZ/101[x,y]",
     EXAMPLE "M = kernel vars R",
     EXAMPLE "isFreeModule M",
     EXAMPLE "isFreeModule prune M"
     }

isSubmodule = method()
isSubmodule Thing := x -> false
isSubmodule Module := M -> not M.?relations

document { quote isSubmodule,
     TT "isSubmodule M", " -- tells whether M is provided as a submodule
     of a free module."
     }

isQuotientModule = method()
isQuotientModule Thing := x -> false
isQuotientModule Module := M -> not M.?generators

document { quote isQuotientModule,
     TT "isQuotientModule M", " -- tells whether M is provided as a
     quotient module of a free module."
     }

isIdeal = method()
isIdeal Thing := x -> false
isIdeal Module := M -> isSubmodule M and rank ambient M === 1

document { quote isIdeal,
     TT "isIdeal I", " -- tells whether a module is an ideal.",
     PARA,
     "An ideal is a submodule of a free module of rank 1."
     }

numgens Module := M -> (
     if M.?generators then numgens M.generators.source
     else if M.?relations then numgens M.relations.target
     else M.numgens
     )
document { quote numgens,
     TT "numgens X", " -- yields the number of generators used to present
     a module or ring.",
     PARA,
     "For a polynomial ring or quotient of one, this is also the number
     of variables.  For a free module, this is the same as the rank.  For
     a general module presented as a subquotient, it is the number of columns
     in the matrix of generators."
     }

generators Module := M -> if M.?generators then M.generators else id_(ambient M)

relations Module := M -> (
     if M.?relations then M.relations 
     else (
	  R := ring M;
	  map(ambient M,R^0,0)
	  )
     )
document { quote relations,
     TT "relations M", " -- produce the relations defining a module M.",
     PARA,
     "The relations are represented as a matrix, and if not stored
     in the module under M.relations, the matrix is understood to be
     empty.",
     PARA,
     SEEALSO ("generators","subquotient")
     }

name Module := M -> (
     if M.?relations then (
	  if M.?generators
	  then "subquotient(" | name M.generators | "," | name M.relations | ")"
	  else "cokernel " | name M.relations
	  )
     else (
	  if M.?generators
	  then "image " | name M.generators
	  else (
	       if numgens M === 0
	       then "0"
	       else name ring M | "^" | name numgens M
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
	       if isHomogeneous N.relations and isHomogeneous M.relations
	       then gb N.relations == gb M.relations
	       else (
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
		    if isHomogeneous f and isHomogeneous g
		    then gb f == gb g
		    else (
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

TEST "
R = ZZ/101[a,b,c]
M = cokernel matrix {{a,b^2,c^3}}
N = image M_{0}
assert( M == N )
"

Vector = new Type of MutableHashTable
ring Vector := v -> ring class v
document { quote Vector,
     TT "Vector", " -- the class of all elements of free modules which
     are handled by the ", TO "engine", ".",
     PARA,
     "If ", TT "R", " is a ring handled by the engine, and ", TT "M", " is a free
     module over ", TT "R", ", then M is a subclass of Vector.",
     PARA,
     SEEALSO "Module"
     }
expression Vector := v -> (
     F := class v;
     R := ring v;
     if class R === PolynomialRing or class R === QuotientRing then (
	  M := monoid R;
	  A := coefficientRing R;
	  new SparseMonomialVectorExpression from { numgens ambient F, 
	       convert(
		    ConvertRepeat ConvertJoin(
			 ConvertInteger,
			 M.ConvertToExpression,
			 A.ConvertToExpression),
		    callgg(ggtonet, v))})
     else (
	  new SparseVectorExpression from { numgens ambient F, 
	       convert(
		    ConvertRepeat ConvertJoin(ConvertInteger,R.ConvertToExpression),
		    callgg(ggtonet, v))}))
name Vector := x -> name expression x
net Vector := x -> net expression x
Vector + Vector := (x,y) -> (
     M := class x;
     if M != class y then error "no method for '+'";
     sendgg(ggPush x, ggPush y, ggadd);
     new M)
Vector - Vector := (x,y) -> (
     M := class x;
     if M != class y then error "no method for '-'";
     sendgg(ggPush x, ggPush y, ggsubtract);
     new M)
- Vector := x -> (
     sendgg(ggPush x, ggnegate);
     new class x)

ZZ * Vector := (r,x) -> (
     R := ring x;
     F := class x;
     sendgg(ggPush R, ggPush r, ggfromint, ggPush x, ggmult);
     new F)

leadTerm Vector := x -> (
     R := ring x;
     F := class x;
     sendgg(ggPush x, ggleadterm);
     new F);

leadComponent Vector := x -> (
     R := ring x;
     sendgg(ggPush x, ggleadcomp);
     eePopInt());

leadCoefficient Vector := x -> (
     k := coefficientRing ring x;
     sendgg(ggPush x, ggleadcoeff);
     k.pop())

leadMonomial Vector := x -> (
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
document { "v_i",
     TT "v_i", " -- produce the i-th entry of a vector or module element v.",
     PARA,
     EXAMPLE "R = ZZ/101[a..f]",
     EXAMPLE "v = vector {a,b,c}",
     EXAMPLE "v_1",
     SEEALSO ("_")
     }

reduceit := M -> (
     if M.?relations then (
	  g := gb M;
	  sendgg(
	       ggPush g,
	       ggINT, gg 1, ggpick,  -- swap
	       ggreduce)))

RingElement * Vector := (r,x) -> (
     R := class r;
     M := class x;
     if R =!= ring M then error "ring element and vector involve different rings";
     sendgg(ggPush r, ggPush x, ggmult);
     reduceit M;
     new M)

new Vector := M -> (
     if class M =!= Module then error "expected a module";
     x := new MutableHashTable;
     x.handle = newHandle "";
     x)

new Vector from List := (M,w) -> (
     if class M =!= Module then error "expected a module";
     if # w != numgens M then error (
	  "expected a list of length '", name numgens M, "'");
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
     sendgg gglength;
     M.numgens = eePopInt();
     M.ring = R;
     M#0 = (
	  sendgg(ggPush M, ggzero);
	  new M);
     M)

degrees Module := M -> (
     R := ring M;
     if M.?degrees then M.degrees
     else (
	  rk := numgens M;
	  nd := degreeLength R;
	  M.degrees = (
	       if nd == 0 
	       then elements (rk : {})
	       else (
	  	    sendgg ggPush M;
		    pack(eePopIntarray(), nd)))))

document { quote degrees,
     TT "degrees M", " -- provides a list of multi-degrees for the basis
     elements of a free module M.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "E = R^5",
     EXAMPLE "degrees E",
     EXAMPLE "F = R^{1,2,3,4}",
     EXAMPLE "degrees F"
     }

Module ^ ZZ := (M,i) -> directSum (i:M)

Ring ^ List := (
     (R,degs) -> (
	  degs = - splice degs;
	  if R.?Engine and R.Engine then (
	       ndegs := degreeLength R;
	       fdegs := flatten degs;
	       if all(degs,i -> class i === ZZ) then (
		    if ndegs =!= 1
	       	    then error ( 
			 "expected each multidegree to be of length ",
			 string ndegs)
		    else ())
	       else if all(degs,v -> class v === List) then (
		    scan(degs,v -> (
			      if #v =!= ndegs
			      then error (
				   "expected each multidegree to be of length ",
				   string ndegs
				   );
			      if not all(v,i->class i === ZZ)
			      then error "expected each multidegree to be a list of integers")))
	       else error "expected a list of integers or a list of lists of integers";
	       if # fdegs === 0 
	       then (
		    n := #degs;
		    if R#?(quote ^,n)
		    then R#(quote ^,n)
		    else R#(quote ^,n) = (
			 sendgg( ggPush R, ggPush n, ggfree);
	       		 new Module from R))
	       else (
		    if R#?(quote ^, fdegs)
		    then R#(quote ^, fdegs)
		    else R#(quote ^, fdegs) = (
			 sendgg(ggPush R, ggPush fdegs, ggfree);
	       		 new Module from R)))
	  else error "non-engine free modules with degrees not implemented yet"
	  ))
document { "Ring ^ List",
     TT "R^{i,j, k, ...}", " -- produce a free module over R whose generators have
     degrees -i, -j, -k, ...",
     PARA,
     "The degrees i, j, ... may themselves be multi-degrees, represented
     as lists of integers.  The operator ", TO ":", " may be used to
     indicate repetitions.",
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "F = R^{1,4:2,3,3:4}",
     EXAMPLE "degrees F",
     SEEALSO ("degrees", "^")
     }

components = method()
components(Vector) := (x) -> apply(numgens class x,i->x_i)
document { quote components,
     TT "components x", " -- produces a list of the components of an element of a 
     free module.",
     BR,NOINDENT,
     TT "components M", " -- the list of components for a module ", TT "M", " which was
     formed as a direct sum, or ", TT "{M}", " if ", TT "M", " was not formed as a 
     direct sum.  Works also for homomorphism, chain complexes, and graded modules.",
     PARA,
     SEEALSO ("vector", "directSum", "++")
     }

SparseDisplayThreshhold := 15

Ring ^ ZZ := (
     (R,n) -> (
	  if R.?Engine and R.Engine
	  then (
	       if R#?(quote ^,n) then R#(quote ^,n)
	       else R#(quote ^,n) = (
	       	    sendgg(ggPush R, ggPush n, ggfree);
     	       	    new Module from R)
	       )
	  else notImplemented()
	  )
     )

document { "Ring ^ ZZ",
     TT "R ^ n", " -- produce a free module of rank n over the ring R",
     PARA,
     SEEALSO("^", "isFreeModule", "Ring ^ List")
     }

-- euler(Module) := (M) -> (
--      f := poincare M;
--      R := ring M;
--      N := numgens R - 1;
--      u := quote u;
--      G := group [u];
--      U := u;
--      use ZZ G;
--      h := U^-N * (substitute(f,{(ring f)_0 => 1-u})) * (sum(N+1,i->U^i));
--           -- f might have negative exponents in it here!
--      c := elements apply(0 .. N, i -> h_(U^-i));
--      k := position(reverse c, j -> j != 0);
--      if k === null then k = N+1;
--      c = drop(c,-k);
--      << "sectional euler characteristics:" << endl;
--      scan(#c, i -> << " " << name i || "" || name c#i);
--      << endl;
--      )
euler(Module) := (M) -> (
     h := hilbertPolynomial M;
     apply(elements ( 0 .. dim h ), i -> euler diff(h,i) ))
euler(Ring) := (R) -> euler R^1

document { quote euler,
     TT "euler M", " -- provide a list of the successive sectional Euler 
     characteristics of a module, ring, or ideal.",
     PARA,
     "The i-th one in the list is the Euler characteristic of the i-th
     hyperplane section of M."
     }

genera(Module) := (M) -> (
     e := euler M;
     apply(#e, i -> (-1)^i * (1 - e#i)))
genera(Ring) := (R) -> genera R^1

document { quote genera,
     TT "genera M", " -- provide a list of the successive sectional 
     arithmetic genera of a module, ring, or ideal.",
     PARA,
     "The i-th one in the list is the arithmetic genus of the i-th
     hyperplane section of M."
     }

TEST ///
R = ZZ/101[a,b,c]/c^4
assert ( genera R == {3,3} )
assert ( euler R == {-2,4} )
R = ZZ/101[a,b,c]/c^3
assert ( genera R == {1,2} )
assert ( euler R == {0,3} )
///

rank Module := M -> (
     if isFreeModule M then numgens M 
     else (
	  f := poincare M;
	  T := (ring f)_0;
	  substitute(f,{T=>1})
	  )
     )
document { quote rank,
     TT "rank M", " -- computes the rank of the module M.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y,z]",
     EXAMPLE "p = vars R;",
     EXAMPLE "rank kernel p",
     EXAMPLE "rank cokernel p"
     }

ambient Module := M -> (
     if M.?generators then M.generators.target
     else if M.?relations then M.relations.target
     else M)

cover(Module) := (M) -> (
     if M.?generators then M.generators.source
     else if M.?relations then M.relations.target
     else M)

document { quote cover,
     TT "cover M", " -- yields the free module whose basis elements correspond
     to the generators of M.",
     PARA,
     SEEALSO ("ambient", "super")
     }

super(Module) := (M) -> (
     if M.?generators then (
     	  if M.?relations then cokernel M.relations
	  else M.generators.target
	  )
     else M
     )

document { quote super,
     TT "super M", " -- yields the module which the module M is a submodule of.",
     BR, NOINDENT,
     TT "super f", " -- if f is a map whose target is a submodule of M, yields the
     composite of f with the inclusion into M.",
     PARA,
     SEEALSO ( "cover", "ambient" )
     }

End = (M) -> Hom(M,M)
document { quote End,
     TT "End M", " -- constructs the module of endomorphisms of M."
     }

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

ZZ * Module := 
RingElement * Module := (r,M) -> subquotient (r ** generators M, relations M)

isHomogeneous Module := (M) -> ring M === ZZ or (
     isHomogeneous ring M and (
     if M.?isHomogeneous 
     then M.isHomogeneous 
     else M.isHomogeneous = (
     	  (not M.?generators or isHomogeneous M.generators)
     	  and
     	  (not M.?relations or isHomogeneous M.relations)
	  )
     ))

