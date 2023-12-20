newPackage(
    "Varieties",
    Date     => "11 Oct 2023",
    Version  => "0.1",
    Keywords => {"Algebraic Geometry"},
    Headline => "routines for working with affine and projective varieties and coherent sheaves on them",
    Authors  => {},
    PackageExports => {},
    PackageImports => {-*"Truncations"*-}, -- FIXME: need this for Ext
    AuxiliaryFiles => true,
    DebuggingMode  => false
    )

export {
    -- Types
    "Variety",
    "AffineVariety",
    "ProjectiveVariety",
    "CoherentSheaf",
    "SheafExpression",
    "SheafOfRings",
    "SumOfTwists",
    "LowerBound",
    -- Methods
    "variety",
    "Proj",
    "Spec",
    "sheaf",
    "sheafExt",
    "sheafHom",
    "tangentSheaf",
    "cotangentSheaf",
    -- Functors
    "hh", -- TODO: should this be defined in Core?
    "OO",
    }

importFrom_Core {
    "getAttribute", "hasAttribute", "ReverseDictionary",
    "toString'", "expressionValue", "unhold", -- TODO: prune these
    }

-----------------------------------------------------------------------------
-- Utilities to be added to Core
-----------------------------------------------------------------------------

applyMethod = (key, X) -> (
    if (F := lookup key) =!= null then F X else error "no method available") -- expand this error message later

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

checkRing := A -> (
    -- TODO: make this unnecessary
    if not degreeLength A === 1 then error "expected degreeLength of ring to be 1";
    if not same degrees A then error "expected variables all of the same degree";
    )

-- given a list {a,a,b,b,b,c,...} returns a list {{2,a}, {3,b}, {1,c}, ...}
runLengthEncoding := x -> if #x === 0 then x else (
     p := join({0}, select(1 .. #x - 1, i -> x#i =!= x#(i-1)), {#x});
     apply(#p-1, i -> (p#(i+1)-p#i, x#(p#i))))

-----------------------------------------------------------------------------
-- Variety, etc. type declarations and basic constructors
-----------------------------------------------------------------------------

Variety = new Type of MutableHashTable
Variety.synonym = "variety"
Variety.GlobalAssignHook = globalAssignFunction
Variety.GlobalReleaseHook = globalReleaseFunction

AffineVariety = new Type of Variety
AffineVariety.synonym = "affine variety"

ProjectiveVariety = new Type of Variety
ProjectiveVariety.synonym = "projective variety"

-- constructors
Spec = method(TypicalValue => AffineVariety)
Spec Ring := (stashValue symbol Spec) (R ->
    new AffineVariety from {
	symbol ring => R,
	symbol cache => new CacheTable
	}
    )

Proj = method(TypicalValue => ProjectiveVariety)
Proj Ring := (stashValue symbol Proj) (R ->
    new ProjectiveVariety from {
	symbol ring => if isHomogeneous R then R else error "Proj: expected a homogeneous ring",
	symbol cache => new CacheTable
	}
    )

-- TODO: PP(1,2,3) for weighted Proj and PP(V) for vector space V and PP(E) for bundle E?
--PP = new ScriptedFunctor from {
--     superscript => (
--	  i -> R -> (
--	       x := symbol x;
--	       Proj (R[ x_0 .. x_i ])
--	       )
--	  )
--     }

-- this is a kludge to make Spec ZZ/101[x,y]/(y^2-x^3) and Proj ZZ/101[x,y]/(x^2-y^2) work as expected
-- TODO: also make Spec kk{x,y} or Spec kk<|x,y|> work when they are supported
-- TODO: document this in Proj and Spec
    AffineVariety/Thing :=     AffineVariety => (X, I) -> Spec((ring X)/I)
ProjectiveVariety/Thing := ProjectiveVariety => (X, I) -> Proj((ring X)/I) -- TODO: should this be saturated?
    AffineVariety Array :=     AffineVariety => (X, M) -> Spec((ring X) M)
ProjectiveVariety Array := ProjectiveVariety => (X, M) -> Proj((ring X) M)

-- basic methods
ring  Variety := X -> X.ring
ideal Variety := X -> ideal ring X -- TODO: should this give the irrelevant ideal?
codim Variety := options(codim, QuotientRing) >> o -> X -> codim(ring X, o)

dim     AffineVariety := X -> dim ring X
dim ProjectiveVariety := X -> dim ring X - 1 -- TODO: - Picard rank instead?

char     AffineVariety := X -> char ring X
char ProjectiveVariety := X -> char(ring X / saturate ideal X) -- TODO: saturate with respect to B?

-- TODO: should these be defined, but return 0 for an AffineVariety?
degree ProjectiveVariety := X -> degree ring X
genus  ProjectiveVariety := X -> genus  ring X
genera ProjectiveVariety := X -> genera ring X
-- euler ProjectiveVariety is defined further down
-- TODO: define degrees, eulers

ambient     AffineVariety :=     AffineVariety => X -> Spec ambient ring X
ambient ProjectiveVariety := ProjectiveVariety => X -> Proj ambient ring X

-- arithmetic ops
-- TODO: uncomment the projective ones when Proj works with multigraded rings
-- TODO: use ** instead of * to match NormalToricVarieties, etc.
AffineVariety     *      AffineVariety :=     AffineVariety => (X, Y) -> Spec(ring X ** ring Y)
--ProjectiveVariety *  ProjectiveVariety := ProjectiveVariety => (X, Y) -> Proj(ring X ** ring Y)
AffineVariety     ** Ring              :=     AffineVariety => (X, R) -> X * Spec R
--ProjectiveVariety ** Ring              := ProjectiveVariety => (X, R) -> X ** Proj R

-- This method returns either a Variety, an AbstractVariety (from Schubert2),
-- a NormalToricVariety, or any other variety stashed in R.variety.
-- TODO: instead of an error, return Proj R when there is no variety,
-- then replace Proj ring M in code for sheaf with variety ring M
variety = method(TypicalValue => Variety)
variety Ring        := S -> if S.?variety then S.variety else error "no variety associated with ring"
variety Ideal       := I -> Proj(ring I/I) -- TODO: should this be saturated?
variety RingElement := f -> variety ring f -- TODO: should this be V(f) instead?

sameVariety := Fs -> if not same apply(Fs, variety) then error "expected coherent sheaves on the same variety"

-- printing
expression       Variety := X -> if hasAttribute(X, ReverseDictionary) then expression getAttribute(X, ReverseDictionary) else (describe X)#0
-- TODO: are these all necessary?
net              Variety :=      net @@ expression
texMath          Variety :=  texMath @@ expression
toString         Variety := toString @@ expression
toExternalString Variety := toString @@ describe

-- used to be in m2/mathml.m2
mathML Variety := lookup(mathML, Thing)

describe     AffineVariety := X -> Describe (expression Spec) (expression X.ring)
describe ProjectiveVariety := X -> Describe (expression Proj) (expression X.ring)

-----------------------------------------------------------------------------
-- SheafOfRings and CoherentSheaf type declarations and basic constructors
-----------------------------------------------------------------------------

SheafOfRings = new Type of HashTable
SheafOfRings.synonym = "sheaf of rings"

CoherentSheaf = new Type of HashTable
CoherentSheaf.synonym = "coherent sheaf"

-- constructors
sheaf = method()
-- TODO: sheaf Ring and sheaf Module should return a sheaf over variety of the ring rather than Proj,
-- and if a variety doesn't already exist then either Proj or Spec should be defined and cached.
sheaf Ring := Ring ~ := SheafOfRings =>     R  -> sheaf(Proj R, R)
sheaf Variety        := SheafOfRings =>  X     -> sheaf(X, ring X)
sheaf(Variety, Ring) := SheafOfRings => (X, R) -> (
    if ring X =!= R then error "sheaf: expected ring of the variety";
    new SheafOfRings from { symbol variety => X, symbol ring => R } )

-- TODO: should the module of a sheaf be fixed, or should it be allowed to change?
-- TODO: https://github.com/Macaulay2/M2/issues/1358
sheaf Module := Module ~ := CoherentSheaf =>     M  -> sheaf(Proj ring M, M)
sheaf(Variety, Module)   := CoherentSheaf => (X, M) -> (
    if M.cache#?(sheaf, X) then return M.cache#(sheaf, X);
    M.cache#(sheaf, X) = (
	if ring M =!= ring X then error "sheaf: expected module and variety to have the same ring";
	if instance(X, ProjectiveVariety) and not isHomogeneous M then error "sheaf: expected a homogeneous module";
	new CoherentSheaf from {
	    symbol variety => X,
	    symbol module => M,
	    symbol cache => new CacheTable
	    }
	))

-- TODO: consider adding IdealSheaf or SheafOfIdeals type
-- sheaf Ideal := Ideal ~ := CoherentSheaf => I -> sheaf(Proj ring M, I)

OO = new ScriptedFunctor from {
     subscript => X -> applyMethod((symbol _,     OO, class X), (OO, X)),
     argument  => X -> applyMethod((symbol SPACE, OO, class X), (OO, X)),
     }
OO.texMath = ///{\mathcal O}///
installMethod(symbol_, OO, Variety, (OO, X) -> sheaf(X, ring X))

-- basic methods
variety SheafOfRings  :=
variety CoherentSheaf := F -> F.variety

ring SheafOfRings  :=
ring CoherentSheaf := F -> ring F.variety

module SheafOfRings  := Module => F -> module F.ring
module CoherentSheaf := Module => F -> F.module

codim   CoherentSheaf := options(codim, Module) >> o -> F -> codim(F.module, o)
rank    CoherentSheaf := F -> rank    F.module
degrees CoherentSheaf := F -> degrees F.module
numgens CoherentSheaf := F -> numgens F.module
betti   CoherentSheaf := o -> F -> betti(F.module, o)

super   CoherentSheaf := CoherentSheaf => F -> sheaf(F.variety, super   F.module)
ambient CoherentSheaf := CoherentSheaf => F -> sheaf(F.variety, ambient F.module)
cover   CoherentSheaf := CoherentSheaf => F -> sheaf(F.variety, cover   F.module)

-- twist and powers
-- TODO: check projectivity
-- TODO: https://github.com/Macaulay2/M2/issues/2288
SheafOfRings(ZZ)   := CoherentSheaf => (O, a) -> O^1(a)
CoherentSheaf(ZZ)  := CoherentSheaf => (F, a) -> sheaf(F.variety, F.module ** (ring F)^{a})
SheafOfRings  ^ ZZ := SheafOfRings  ^ List := CoherentSheaf => (O, n) -> sheaf(O.variety, (ring O)^n)
CoherentSheaf ^ ZZ := CoherentSheaf ^ List := CoherentSheaf => (F, n) -> sheaf(F.variety, F.module^n)
dual CoherentSheaf := CoherentSheaf => options(dual, Module) >> o -> F -> sheaf(F.variety, dual(F.module, o))

-- arithmetic ops
CoherentSheaf.directSum = args -> ( sameVariety args; sheaf(variety args#0, directSum apply(args, module)) )
CoherentSheaf ++ CoherentSheaf := CoherentSheaf => (F, G) -> sheaf(F.variety, F.module ++ G.module)
CoherentSheaf ** CoherentSheaf := CoherentSheaf => (F, G) -> sheaf(F.variety, F.module ** G.module)
CoherentSheaf  / CoherentSheaf := CoherentSheaf => (F, G) -> sheaf(F.variety, F.module  / G.module)
CoherentSheaf  / Ideal         := CoherentSheaf => (F, I) -> sheaf(F.variety, F.module  / I)
Ideal * CoherentSheaf          := CoherentSheaf => (I, F) -> sheaf(F.variety, I * F.module)
directSum CoherentSheaf        := CoherentSheaf =>  F     -> CoherentSheaf.directSum(1 : F)

-- multilinear ops
exteriorPower (ZZ, CoherentSheaf) := CoherentSheaf => o -> (i, F) -> sheaf(variety F,  exteriorPower(i, module F, o))
symmetricPower(ZZ, CoherentSheaf) := CoherentSheaf => o -> (i, F) -> sheaf(variety F, symmetricPower(i, module F, o))

annihilator CoherentSheaf := Ideal => o -> F -> annihilator(module F, o)

-- printing
expression SheafOfRings := O -> Subscript { OO, expression O.variety }
net        SheafOfRings :=      net @@ expression
texMath    SheafOfRings :=  texMath @@ expression
toString   SheafOfRings := toString @@ expression

describe   CoherentSheaf := F -> Describe (Subscript { expression sheaf, expression F.variety }) (expression F.module)
expression CoherentSheaf := F -> (
    (X, M) := (variety F, module F);
    if M.?relations or M.?generators or numgens M === 0 then return SheafExpression expression M;
    degs := runLengthEncoding(- degrees M); -- a list of O_X^r(d) for each summand
    sums := apply(degs, (r, d) -> (
	    s := new Superscript from {expression OO_X, expression r};
	    -- TODO: get rid of the extra space in OO_X^1 (1,2) when #d > 1
	    if all(d, zero) then s else new Adjacent from {
		s, (if #d == 1 then new Parenthesize from d else expression toSequence d)}));
    fold((a, b) -> a++b, sums))
net      CoherentSheaf :=      net @@ expression
texMath  CoherentSheaf :=  texMath @@ expression
toString CoherentSheaf := toString @@ expression

-- used to be in m2/mathml.m2
mathML SheafOfRings :=
mathML CoherentSheaf := lookup(mathML, Thing)

CoherentSheaf#AfterPrint = F -> ("coherent sheaf on ", variety F,
    if isFreeModule(M := module F)    then (", free of rank ",   rank F)    else
    if M.?generators and M.?relations then (", subquotient of ", ambient F) else
    if M.?generators                  then (", subsheaf of ",    ambient F) else
    if M.?relations                   then (", quotient of ",    ambient F)
    )

-- used to be in m2/jupyter.m2
CoherentSheaf#{Jupyter, AfterPrint} = F -> (
    << "[CLS]" << endl; CoherentSheaf#{Standard,AfterPrint}(F) )

-- used to be in m2/expressions.m2
SheafExpression = new WrapperType of Expression;
toString'(Function, SheafExpression) := (fmt,x) -> toString'(fmt,new FunctionApplication from { sheaf, x#0 })
net SheafExpression := x -> net x#0
texMath SheafExpression := x -> texMath x#0
expressionValue SheafExpression := x -> sheaf expressionValue x#0

-----------------------------------------------------------------------------
-- SumOfTwists type declarations and basic constructors
-----------------------------------------------------------------------------

-- used as a bound for sums of twists
LowerBound = new SelfInitializingType of BasicList
-- TODO: implement for multigraded twists
>  InfiniteNumber := >  ZZ            := LowerBound => b -> LowerBound{b+1}
>= InfiniteNumber := >= ZZ := >= List := LowerBound => b -> LowerBound{b}

SumOfTwists = new Type of BasicList
SumOfTwists.synonym = "sum of twists"

-- constructors
SheafOfRings(*)  := SumOfTwists => O -> O^1(>=-infinity)
CoherentSheaf(*) := SumOfTwists => F ->   F(>=-infinity)
SheafOfRings  LowerBound := SumOfTwists => (O, b) -> O^1(b)
CoherentSheaf LowerBound := SumOfTwists => (F, b) -> new SumOfTwists from {F, b}

-- basic methods
ring    SumOfTwists := S ->    ring S#0
variety SumOfTwists := S -> variety S#0

-- printing
expression SumOfTwists := S -> (expression S#0) (if S#1#0 === -infinity then expression symbol(*) else (expression symbol>=) (expression S#1#0))
net        SumOfTwists :=      net @@ expression
texMath    SumOfTwists :=  texMath @@ expression
toString   SumOfTwists := toString @@ expression

-----------------------------------------------------------------------------
-- helpers for sheaf cohomology
-----------------------------------------------------------------------------

degreeList = (M) -> (
     if dim M > 0 then error "expected module of finite length";
     H := poincare M;
     T := (ring H)_0;
     H = H // (1-T)^(numgens ring M);
     exponents H / first)

globalSectionsModule = (G,bound) -> (
     -- compute global sections
     if degreeLength ring G =!= 1 then error "expected degree length 1";
     M := module G;
     A := ring G;
     M = cokernel presentation M;
     S := saturate image map(M,A^0,0);
     if S != 0 then M = M/S;
     F := presentation A;
     R := ring F;
     N := cokernel lift(presentation M,R) ** cokernel F;
     r := numgens R;
     wR := R^{-r};
     if bound < infinity and pdim N >= r-1 then (
	  E1 := Ext^(r-1)(N,wR);
	  p := (
	       if dim E1 <= 0
	       then max degreeList E1 - min degreeList E1 + 1
	       else 1 - first min degrees E1 - bound
	       );
	  if p === infinity then error "global sections module not finitely generated, can't compute it all";
	  if p > 0 then M = Hom(image matrix {apply(numgens A, j -> A_j^p)}, M);
	  );
     minimalPresentation M)

cohomology(ZZ,SumOfTwists) :=  Module => opts -> (i,S) -> (
     F := S#0;
     R := ring F;
     if not isAffineRing R then error "expected coherent sheaf over a variety over a field";
     b := first S#1;
     if i == 0 then globalSectionsModule(F,b) else HH^(i+1)(module F,Degree => b))

cohomology(ZZ,CoherentSheaf) := Module => opts -> (i,F) -> (
     R := ring F;
     if not isAffineRing R then error "expected coherent sheaf over a variety over a field";
     k := coefficientRing R;
     k^(
	  if i === 0
	  then rank source basis(0, HH^i F(>=0))
	  else (
	       p := presentation R;
	       A := ring p;
	       n := numgens A;
	       M := cokernel lift(presentation module F,A) ** cokernel p;
	       rank source basis(0, Ext^(n-1-i)(M,A^{-n})))))
cohomology(ZZ,ProjectiveVariety,CoherentSheaf) := Module => opts -> (i,X,F) -> cohomology(i,F,opts)

cohomology(ZZ,SheafOfRings) := Module => opts -> (i,O) -> HH^i O^1

-----------------------------------------------------------------------------
-- Module of twisted global sections Γ_*(F)
-----------------------------------------------------------------------------

-- TODO: optimize caching here
-- TODO: should F>=0 be hardcoded?
minimalPresentation CoherentSheaf := prune CoherentSheaf := CoherentSheaf => opts -> (cacheValue symbol minimalPresentation) (
    F -> sheaf(F.variety, minimalPresentation(HH^0 F(>=0), opts)))

-----------------------------------------------------------------------------
-- cotangentSheaf and tangentSheaf
-----------------------------------------------------------------------------

cotangentSheaf = method(Options => {Minimize => true})
tangentSheaf = method(Options => {Minimize => true})

-- weightedVars = S -> (
--      map(S^1, S^-(degrees S), {apply(generators S, flatten degrees S, times)})
--      )

cotangentSheaf ProjectiveVariety := CoherentSheaf => opts -> (cacheValue (symbol cotangentSheaf => opts)) ((X) -> (
	  R := ring X;
	  F := presentation R;
	  S := ring F;
	  checkRing S;
	  d := vars S ** R;
	  e := jacobian F ** R;
     	  -- assert (d*e == 0);
	  om := sheaf(X, homology(d,e));
	  if opts.Minimize then om = minimalPresentation om;
	  om))

cotangentSheaf(ZZ,ProjectiveVariety) := CoherentSheaf => opts -> (i,X) -> (
     if X#?(cotangentSheaf,i)
     then X#(cotangentSheaf,i) 
     else X#(cotangentSheaf,i) = exteriorPower(i,cotangentSheaf(X,opts)))

tangentSheaf ProjectiveVariety := CoherentSheaf => opts -> (X) -> dual cotangentSheaf(X,opts)

-----------------------------------------------------------------------------
-- singularLocus
-----------------------------------------------------------------------------

singularLocus     AffineVariety :=     AffineVariety => X -> Spec singularLocus ring X
singularLocus ProjectiveVariety := ProjectiveVariety => X -> (
     R := ring X;
     f := presentation R;
     A := ring f;
     checkRing A;
     Proj(A / saturate (minors(codim(R,Generic=>true), jacobian f) + ideal f)))

-----------------------------------------------------------------------------

eulers CoherentSheaf := F -> (
     if class variety F =!= ProjectiveVariety then error "expected a coherent sheaf over a projective variety";
     eulers module F)
euler CoherentSheaf := F -> (
     if class variety F =!= ProjectiveVariety then error "expected a coherent sheaf over a projective variety";
     euler module F)
genera CoherentSheaf := F -> (
     if class variety F =!= ProjectiveVariety then error "expected a coherent sheaf over a projective variety";
     genera module F)
genus CoherentSheaf := F -> (
     if class variety F =!= ProjectiveVariety then error "expected a coherent sheaf over a projective variety";
     genus module F)
degree CoherentSheaf := F -> (
     if class variety F =!= ProjectiveVariety then error "expected a coherent sheaf over a projective variety";
     degree F.module)
pdim CoherentSheaf := F -> pdim module F

-----------------------------------------------------------------------------

hilbertSeries ProjectiveVariety := opts -> X -> ( notImplemented(); hilbertSeries(ring X,opts) )
hilbertSeries CoherentSheaf := opts -> F -> (
     if class variety F =!= ProjectiveVariety then error "expected a coherent sheaf over a projective variety";
     notImplemented();
     hilbertSeries(module F,opts))

hilbertPolynomial ProjectiveVariety := ProjectiveHilbertPolynomial => opts -> X -> hilbertPolynomial(ring X, opts)
hilbertPolynomial CoherentSheaf := opts -> F -> (
     if class variety F =!= ProjectiveVariety then error "expected a coherent sheaf over a projective variety";
     hilbertPolynomial(F.module,opts))

hilbertFunction(List,CoherentSheaf) := hilbertFunction(ZZ,CoherentSheaf) := (d,F) -> ( notImplemented(); hilbertFunction(d,F.module))
hilbertFunction(List,ProjectiveVariety) := hilbertFunction(ZZ,ProjectiveVariety) := (d,X) -> ( notImplemented(); hilbertFunction(d,ring X))

-----------------------------------------------------------------------------

-- TODO: simplify using the fact that tensor is a binary method
binaryPower := (W,n,times,unit,inverse) -> (
     if n === 0 then return unit();
     if n < 0 then (W = inverse W; n = -n);
     Z := null;
     while (
	  if odd n then if Z === null then Z = W else Z = times(Z,W);
	  n = n // 2;
	  n =!= 0
	  )
     do W = times(W, W);
     Z)

-- TODO: find a better home for these and binaryPower
Monoid        ^** ZZ := (M,n) -> binaryPower(M,n,tensor,() -> monoid [], x -> error "Monoid ^** ZZ: expected non-negative integer")
Ring          ^** ZZ := (R,n) -> binaryPower(R,n,tensor,() -> coefficientRing R, x -> error "Ring ^** ZZ: expected non-negative integer")
Module        ^** ZZ := (F,n) -> binaryPower(F,n,tensor,() -> (ring F)^1, dual)
CoherentSheaf ^** ZZ := (F,n) -> binaryPower(F,n,tensor,() -> OO_(F.variety)^1, dual)

-----------------------------------------------------------------------------
-- Sheaf Hom and Ext
-----------------------------------------------------------------------------

-- TODO: assert that sheaves are on the same variety
sheafHom = method(TypicalValue => CoherentSheaf)
sheafHom(CoherentSheaf,CoherentSheaf) := (F,G) -> sheaf(variety F, Hom(module F, module G))
Hom(CoherentSheaf,CoherentSheaf) := Module => (F,G) -> HH^0(variety F, sheafHom(F, G))

Hom(SheafOfRings,CoherentSheaf) := Module => (O,G) -> Hom(O^1,G)
Hom(CoherentSheaf,SheafOfRings) := Module => (F,O) -> Hom(F,O^1)
Hom(SheafOfRings,SheafOfRings) := Module => (O,R) -> Hom(O^1,R^1)

sheafHom(SheafOfRings,CoherentSheaf) := Module => (O,G) -> sheafHom(O^1,G)
sheafHom(CoherentSheaf,SheafOfRings) := Module => (F,O) -> sheafHom(F,O^1)
sheafHom(SheafOfRings,SheafOfRings) := Module => (O,R) -> sheafHom(O^1,R^1)

sheafExt = new ScriptedFunctor from {
     superscript => (
	  i -> new ScriptedFunctor from {
	       argument => (M,N) -> (
		    f := lookup(sheafExt,class i,class M,class N);
		    if f === null then error "no method available"
		    else f(i,M,N)
		    )
	       }
	  )
     }
sheafExt(ZZ,CoherentSheaf,CoherentSheaf) := CoherentSheaf => (
     (n,F,G) -> sheaf_(variety F) Ext^n(module F, module G)
     )

sheafExt(ZZ,SheafOfRings,CoherentSheaf) := Module => (n,O,G) -> sheafExt^n(O^1,G)
sheafExt(ZZ,CoherentSheaf,SheafOfRings) := Module => (n,F,O) -> sheafExt^n(F,O^1)
sheafExt(ZZ,SheafOfRings,SheafOfRings) := Module => (n,O,R) -> sheafExt^n(O^1,R^1)

-----------------------------------------------------------------------------
-- code donated by Greg Smith <ggsmith@math.berkeley.edu>

-- The following algorithms and examples appear in Gregory G. Smith,
-- Computing global extension modules, Journal of Symbolic Computation
-- 29 (2000) 729-746.
-- See tests/normal/ext-global.m2 for the examples
-----------------------------------------------------------------------------

Ext(ZZ,CoherentSheaf,SumOfTwists) := Module => opts -> (m,F,G') -> (
     -- depends on truncate methods
     needsPackage "Truncations";
     G := G'#0;
     e := G'#1#0;
     if variety G =!= variety F
     then error "expected sheaves on the same variety";
     if not instance(variety G,ProjectiveVariety)
     then error "expected sheaves on a projective variety";
     M := module F;
     N := module G;
     R := ring M;
     if not isAffineRing R
     then error "expected sheaves on a variety over a field";
     local E;
     if dim M === 0 or m < 0 then E = R^0
     else (
          f := presentation R;
          S := ring f;
          n := numgens S -1;
          l := min(dim N, m);
	  P := resolution(cokernel lift(presentation N,S) ** cokernel f);
	  p := length P;
	  if p < n-l then E = Ext^m(M, N, opts)
	  else (
	       a := max apply(n-l..p,j -> (max degrees P_j)#0-j);
	       r := a-e-m+1;
	       E = Ext^m(truncate(r,M), N, opts)));
     if (min degrees E) === infinity then E
     else if (min degrees E)#0 > e then minimalPresentation E
     else minimalPresentation truncate(e,E))

Ext(ZZ,SheafOfRings,SumOfTwists) := Module => opts -> (m,O,G') -> Ext^m(O^1,G',opts)

Ext(ZZ,CoherentSheaf,CoherentSheaf) := Module => opts -> (n,F,G) -> (
     E := Ext^n(F,G(>=0),opts);
     k := coefficientRing ring E;
     k^(rank source basis(0,E)))

Ext(ZZ,SheafOfRings,CoherentSheaf) := Module => opts -> (n,O,G) -> Ext^n(O^1,G,opts)
Ext(ZZ,CoherentSheaf,SheafOfRings) := Module => opts -> (n,F,O) -> Ext^n(F,O^1,opts)
Ext(ZZ,SheafOfRings,SheafOfRings) := Module => opts -> (n,O,R) -> Ext^n(O^1,R^1,opts)

-----------------------------------------------------------------------------
-- end of code donated by Greg Smith <ggsmith@math.berkeley.edu>
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- hh: Hodge decomposition
-----------------------------------------------------------------------------
-- TODO: HodgeTally for pretty printing the Hodge diamond

hh = new ScriptedFunctor from {
     superscript => (
	  pq -> new ScriptedFunctor from {
	       argument => X -> (
		    a := (pq,X);
		    f := lookup_hh ( class \ a );
		    if f === null then error "no method available";
	       	    f a
		    )
	       }
	  )
     }

hh(Sequence,ProjectiveVariety) := (pq,X) -> if X.cache.?hh and X.cache.hh#?pq then X.cache.hh#pq else (
     (p,q) := pq;
     if class p =!= ZZ or class q =!= ZZ then error "expected integer superscripts";
     d := dim X;
     pqs := { (p,q), (q,p), (d-p,d-q), (d-q,d-p) };
     (p,q) = min { (p,q), (q,p), (d-p,d-q), (d-q,d-p) };
     h := rank HH^q cotangentSheaf(p,X);
     if not X.cache.?hh then X.cache.hh = new MutableHashTable;
     scan(pqs, pq -> X.cache.hh#pq = h);
     h)

euler ProjectiveVariety := X -> (
     d := dim X;
     sum(0 .. d, j -> hh^(j,j) X + 2 * sum(0 .. j-1, i -> (-1)^(i+j) * hh^(i,j) X)))

-----------------------------------------------------------------------------
-- Tests
-----------------------------------------------------------------------------

TEST ///
  X = Spec ZZ/101[x,y]/(y^2-x^3)
///

-- needs "./Varieties/tests.m2"

-----------------------------------------------------------------------------
-- Documentation
-----------------------------------------------------------------------------

beginDocumentation()

-- TODO: move "Tutorial: Fano varieties" and "Tutorial: Divisors" here
-- TODO: "fibers of a map between varieties"
-- TODO: update the node "varieties" in overviewA.m2 and "coherent sheaves" in overview2.m2
-- TODO: eventually move local cohomology as well?

doc ///
Node
  Key
    Varieties
  Headline
    affine and projective algebraic geometry
///

needs "./Varieties/doc-varieties.m2"
needs "./Varieties/doc-sheaves.m2"
needs "./Varieties/doc-functors.m2"
needs "./Varieties/euler-doc.m2"
needs "./Varieties/genus-doc.m2"
needs "./Varieties/genera-doc.m2"
needs "./Varieties/tangentSheaf-doc.m2"
needs "./Varieties/cotangentSheaf-doc.m2"

-----------------------------------------------------------------------------
-- Development
-----------------------------------------------------------------------------

end--

uninstallPackage "Varieties"
restart
installPackage "Varieties"
viewHelp "Varieties"

restart
debug needsPackage "Varieties"
check "Varieties"
