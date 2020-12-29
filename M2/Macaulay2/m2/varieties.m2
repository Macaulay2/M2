--		Copyright 1993-1998 by Daniel R. Grayson

Variety = new Type of MutableHashTable
Variety.synonym = "variety"
Variety.GlobalAssignHook = globalAssignFunction
Variety.GlobalReleaseHook = globalReleaseFunction
AffineVariety = new Type of Variety
AffineVariety.synonym = "affine variety"
ProjectiveVariety = new Type of Variety
ProjectiveVariety.synonym = "projective variety"
ring Variety := X -> X.ring
toString Variety := toString @@ expression
toExternalString Variety := toString @@ describe
net Variety := net @@ expression
texMath Variety := x -> texMath expression x
expression Variety := (X) -> if hasAttribute(X,ReverseDictionary) then expression getAttribute(X,ReverseDictionary) else (describe X)#0
describe AffineVariety := (X) -> Describe (expression Spec) (expression X.ring)
describe ProjectiveVariety := (X) -> Describe (expression Proj) (expression X.ring)

char AffineVariety := X -> char ring X
char ProjectiveVariety := X -> (
     I := saturate ideal X;
     char( ring I / I ))

ambient ProjectiveVariety := X -> Proj ambient ring X
ambient     AffineVariety := X -> Spec ambient ring X
ideal Variety := X -> ideal ring X
Spec = method()

Spec Ring := AffineVariety => (R) -> if R.?Spec then R.Spec else R.Spec = (
     new AffineVariety from {
     	  symbol ring => R,
	  symbol cache => new CacheTable
     	  }
     )
Proj = method()

Proj Ring := ProjectiveVariety => (R) -> if R.?Proj then R.Proj else R.Proj = (
     if not isHomogeneous R then error "expected a homogeneous ring";
     new ProjectiveVariety from {
     	  symbol ring => R,
	  symbol cache => new CacheTable
     	  }
     )

sheaf = method()

SheafOfRings = new Type of HashTable
SheafOfRings.synonym = "sheaf of rings"
expression SheafOfRings := O -> Subscript { OO, expression O.variety }
net SheafOfRings := net @@ expression
texMath SheafOfRings := x -> texMath expression x
Ring ~ := sheaf Ring := SheafOfRings => R -> new SheafOfRings from { symbol variety => Proj R, symbol ring => R }
sheaf(Variety,Ring) := SheafOfRings => (X,R) -> (
     if ring X =!= R then error "expected the variety of the ring";
     new SheafOfRings from { symbol variety => X, symbol ring => R } )
ring SheafOfRings := O -> O.ring

CoherentSheaf = new Type of HashTable
CoherentSheaf.synonym = "coherent sheaf"
describe CoherentSheaf := F -> (expression sheaf) (describe F.module)

runLengthEncoding := x -> if #x === 0 then x else (
     p := join({0}, select(1 .. #x - 1, i -> x#i =!= x#(i-1)), {#x});
     apply(#p-1, i -> (p#(i+1)-p#i, x#(p#i))))

expression CoherentSheaf := F -> (
     M := module F;
     if M.?relations or M.?generators or numgens M === 0 then SheafExpression expression M
     else (
	    X := variety F;
	    rle := runLengthEncoding (- degrees F);
	    expr := null;
	    scan(rle,
		(n,d) -> (
		    s := new Superscript from {expression OO_X, expression n};
		    if not all(d, zero) then s = Adjacent {s, if #d === 1 then Parenthesize unhold expression d#0 else expression toSequence d};
		    if expr === null then expr = s else expr = expr ++ s;
		    ));
	    expr
	    )
	)

net CoherentSheaf := (F) -> net expression F
texMath CoherentSheaf := (F) -> texMath expression F
toString CoherentSheaf := (F) -> toString expression F

CoherentSheaf#{Standard,AfterPrint} = F -> (
     X := variety F;
     M := module F;
     << endl;				  -- double space
     n := rank ambient F;
     << concatenate(interpreterDepth:"o") << lineNumber << " : coherent sheaf on " << X;
     if M.?generators then
     if M.?relations then << ", subquotient of " << ambient F
     else << ", subsheaf of " << ambient F
     else if M.?relations then << ", quotient of " << ambient F
     else if n > 0 then (
	  << ", free";
	  -- if not all(degrees M, d -> all(d, zero))
	  -- then << ", degrees " << if degreeLength M === 1 then flatten degrees M else degrees M;
	  );
     << endl;
     )

sheaf(Variety,Module) :=  CoherentSheaf => (X,M) -> if M.cache#?(sheaf,X) then M.cache#(sheaf,X) else M.cache#(sheaf,X) = (
     if ring M =!= ring X then error "expected module and variety to have the same ring";
     if instance(X,ProjectiveVariety) and not isHomogeneous M
     then error "expected a homogeneous module";
     new CoherentSheaf from {
     	  symbol module => M,
	  symbol variety => X
	  }
     )
Module ~ := sheaf Module := CoherentSheaf => (M) -> sheaf(Proj ring M,M)

-- I've removed these methods because they should really produce a coherent sheaf of ideals rather than a coherent sheaf of modules!
-- Ideal ~ := sheaf Ideal := CoherentSheaf => (M) -> sheaf(Proj ring M,module M)

variety = method()
variety CoherentSheaf := Variety => (F) -> F.variety
variety SheafOfRings := Variety => O -> O.variety
-- The following two methods returns either a Variety or 
-- (from Schubert2) an AbstractVariety, or
-- (from NormalToricVarieties) a NormalToricVariety
-- and perhaps other uses later...
variety Ring := S -> if S.?variety then S.variety else error "no variety associated with ring"
variety RingElement := f -> variety ring f

ring CoherentSheaf := (F) -> ring F.module
numgens CoherentSheaf := (F) -> numgens F.module
module CoherentSheaf := Module => F -> F.module
module SheafOfRings  := Module => F -> module F.ring
Ideal * CoherentSheaf := (I,F) -> sheaf(F.variety, I * module F)
CoherentSheaf ++ CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(F.variety, F.module ++ G.module)
CoherentSheaf ** CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(F.variety, F.module ** G.module)
CoherentSheaf ZZ := CoherentSheaf => (F,n) -> sheaf(variety F, F.module ** (ring F)^{n})
SheafOfRings ZZ := CoherentSheaf => (O,n) -> O^1(n)
CoherentSheaf ^ ZZ := CoherentSheaf => (F,n) -> sheaf(F.variety, F.module^n)
CoherentSheaf / CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(F.variety, F.module / G.module)
CoherentSheaf / Ideal := CoherentSheaf => (F,I) -> sheaf(F.variety, F.module / I)

variety Ideal := I -> Proj(ring I/I)

SheafOfRings ^ ZZ := SheafOfRings ^ List := (O,n) -> (
     R := ring O;
     X := variety O;
     sheaf_X R^n
     )

annihilator CoherentSheaf := Ideal => o -> (F) -> annihilator F.module

codim   CoherentSheaf := options (codim,Module) >> opts -> (F) -> codim(module F,opts)
rank    CoherentSheaf := (F) -> rank  module F
degrees CoherentSheaf := (F) -> degrees module F

exteriorPower(ZZ,CoherentSheaf) := CoherentSheaf => options -> (i,F) -> 
    sheaf(variety F, exteriorPower(i,F.module,options))

super   CoherentSheaf := CoherentSheaf => (F) -> sheaf super   module F
ambient CoherentSheaf := CoherentSheaf => (F) -> sheaf ambient module F
cover   CoherentSheaf := CoherentSheaf => (F) -> sheaf cover   module F

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

LowerBound = new SelfInitializingType of BasicList
>  InfiniteNumber := >  ZZ := LowerBound => i -> LowerBound{i+1}
>= InfiniteNumber := >= ZZ := LowerBound => i -> LowerBound{i}
SheafOfRings(*) := O -> O^1(*)
CoherentSheaf(*) := F -> F(>=-infinity)

SumOfTwists = new Type of BasicList
CoherentSheaf LowerBound := SumOfTwists => (F,b) -> new SumOfTwists from {F, b}
SheafOfRings LowerBound := SumOfTwists => (O,b) -> O^1 b
net SumOfTwists := S -> net S#0 | if S#1#0 === neginfinity then "(*)" else "(>=" | net S#1#0 | ")"
texMath SumOfTwists := S -> texMath S#0 | if S#1#0 === neginfinity then "(*)" else "(\\ge" | texMath S#1#0 | ")"

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

cohomology(ZZ,SheafOfRings) := Module => opts -> (i,O) -> HH^i O^1

applyMethod = (key,x) -> (
     f := lookup key;
     if f === null then error "no method available";	    -- expand this error message later
     f x)

OO = new ScriptedFunctor from {
     subscript => X -> applyMethod((symbol _,OO,class X),(OO,X)),
     argument => X -> applyMethod((symbol SPACE,OO,class X),(OO,X)),
     }
OO.texMath = ///{\mathcal O}///
installMethod(symbol _,OO,Variety,(OO,X) -> sheaf_X ring X)
sheaf Variety := X -> sheaf_X ring X

--PP = new ScriptedFunctor from {
--     superscript => (
--	  i -> R -> (
--	       x := symbol x;
--	       Proj (R[ x_0 .. x_i ])
--	       )
--	  )
--     }

minimalPresentation CoherentSheaf := prune CoherentSheaf := opts -> F -> sheaf minimalPresentation HH^0 F(>=0)

cotangentSheaf = method(Options => {Minimize => true})
tangentSheaf = method(Options => {Minimize => true})

-- weightedVars = S -> (
--      map(S^1, S^-(degrees S), {apply(generators S, flatten degrees S, times)})
--      )

checkRing := A -> (
     if not degreeLength A === 1 then error "expected degreeLength of ring to be 1";
     if not same degrees A then error "expected variables all of the same degree";
     )

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

dim AffineVariety := X -> dim ring X
dim ProjectiveVariety := X -> dim ring X - 1
codim ProjectiveVariety := options(codim,PolynomialRing) >> opts -> X -> codim(ring X,opts)

genera ProjectiveVariety := X -> genera ring X
genus ProjectiveVariety := X -> genus ring X

AffineVariety * AffineVariety := AffineVariety => (X,Y) -> Spec(ring X ** ring Y)
AffineVariety ** Ring := AffineVariety => (X,R) -> Spec(ring X ** R)
-- this is not right:
-- ProjectiveVariety ** Ring := ProjectiveVariety => (X,R) -> Proj(ring X ** R)

singularLocus(ProjectiveVariety) := X -> (
     R := ring X;
     f := presentation R;
     A := ring f;
     checkRing A;
     Proj(A / saturate (minors(codim(R,Generic=>true), jacobian f) + ideal f)))

singularLocus(AffineVariety) := X -> Spec singularLocus ring X

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
degree ProjectiveVariety := X -> degree ring X

pdim CoherentSheaf := F -> pdim module F

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

dual CoherentSheaf := {} >> o -> F -> sheaf_(F.variety) dual F.module
betti CoherentSheaf := opts -> F -> (
     if class variety F =!= ProjectiveVariety then error "expected a coherent sheaf over a projective variety";
     betti (F.module,opts))

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

Module        ^** ZZ := (F,n) -> binaryPower(F,n,tensor,() -> (ring F)^1, dual)
CoherentSheaf ^** ZZ := (F,n) -> binaryPower(F,n,tensor,() -> OO_(F.variety)^1, dual)

sheafHom = method(TypicalValue => CoherentSheaf)
sheafHom(CoherentSheaf,CoherentSheaf) := (F,G) -> sheaf Hom(module F, module G)
Hom(CoherentSheaf,CoherentSheaf) := Module => (F,G) -> HH^0 sheafHom(F,G)

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
     (n,F,G) -> sheaf Ext^n(module F, module G)
     )

sheafExt(ZZ,SheafOfRings,CoherentSheaf) := Module => (n,O,G) -> sheafExt^n(O^1,G)
sheafExt(ZZ,CoherentSheaf,SheafOfRings) := Module => (n,F,O) -> sheafExt^n(F,O^1)
sheafExt(ZZ,SheafOfRings,SheafOfRings) := Module => (n,O,R) -> sheafExt^n(O^1,R^1)

-----------------------------------------------------------------------------
-- code donated by Greg Smith <ggsmith@math.berkeley.edu>

-- The following algorithms and examples appear in Gregory G. Smith,
-- Computing global extension modules, Journal of Symbolic Computation
-- 29 (2000) 729-746.
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

------------------------------------
-- Code donated by Frank Schreyer --
------------------------------------

randomKRationalPoint = method()
randomKRationalPoint Ideal := I -> (
     R:=ring I;
     if char R == 0 then error "expected a finite ground field";
     if not class R === PolynomialRing then error "expected an ideal in a polynomial ring";
     if not isHomogeneous I then error "expected a homogenous ideal";
     n:=dim I;
     if n<=1 then error "expected a positive dimensional scheme";
     c:=codim I;
     Rs:=R;
     Re:=R;
     f:=I;
     if not c==1 then (
         -- projection onto a hypersurface
         parametersystem:=ideal apply(n,i->R_(i+c));
         if not dim(I+parametersystem)== 0 then return print "make coordinate change";
         kk:=coefficientRing R;
         Re=kk(monoid[apply(dim R,i->R_i),MonomialOrder => Eliminate (c-1)]);
         rs:=(entries selectInSubring(1,vars Re))_0;
         Rs=kk(monoid[rs]);
         f=ideal substitute(selectInSubring(1, generators gb substitute(I,Re)),Rs);
         if not degree I == degree f then return print "make coordinate change"
         );
     H:=0;pts:=0;pts1:=0;trial:=1;pt:=0;ok:=false;
     while (
         H=ideal random(Rs^1,Rs^{dim Rs-2:-1});
         pts=decompose (f+H);
         pts1=select(pts,pt-> degree pt==1 and dim pt ==1);
         ok=( #pts1>0); 
         if ok then (pt=saturate(substitute(pts1_0,R)+I);ok==(degree pt==1 and dim pt==0));
         not ok) do (trial=trial+1);
     pt
     )


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
