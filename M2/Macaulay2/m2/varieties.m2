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
genera Variety := X -> genera ring X
ambient ProjectiveVariety := X -> Proj ambient ring X
ambient     AffineVariety := X -> Spec ambient ring X
ideal Variety := X -> ideal ring X
Spec = method()

net Variety := (X) -> if X.?name then X.name else net expression X

expression AffineVariety := (X) -> new FunctionApplication from { Spec, X.ring }
Spec Ring := AffineVariety => (R) -> if R.?Spec then R.Spec else R.Spec = (
     new AffineVariety from {
     	  symbol ring => R,
	  symbol cache => new CacheTable
     	  }
     )
Proj = method()
expression ProjectiveVariety := (X) -> (
     R := ring X;
     if class R === QuotientRing
     then new FunctionApplication from { variety, ideal R }
     else new FunctionApplication from { Proj, R }
     )
Proj Ring := ProjectiveVariety => (R) -> if R.?Proj then R.Proj else R.Proj = (
     if not isHomogeneous R then error "expected a homgeneous ring";
     new ProjectiveVariety from {
     	  symbol ring => R,
	  symbol cache => new CacheTable
     	  }
     )

sheaf = method()

SheafOfRings = new Type of HashTable
SheafOfRings.synonym = "sheaf of rings"
expression SheafOfRings := O -> new Subscript from { OO, O.variety }
net SheafOfRings := O -> net expression O
Ring ~ := sheaf Ring := SheafOfRings => R -> new SheafOfRings from { symbol variety => Proj R, symbol ring => R }
sheaf(Variety,Ring) := SheafOfRings => (X,R) -> (
     if ring X =!= R then error "expected the variety of the ring";
     new SheafOfRings from { symbol variety => X, symbol ring => R } )
ring SheafOfRings := O -> O.ring

CoherentSheaf = new Type of HashTable
CoherentSheaf.synonym = "coherent sheaf"
expression CoherentSheaf := F -> new FunctionApplication from { sheaf, F.module }

-- net CoherentSheaf := (F) -> net expression F

net CoherentSheaf := F -> (
     M := module F;
     if M.?relations 
     then if M.?generators
     then net new FunctionApplication from { subquotient, (net M.generators, net M.relations) }
     else net new FunctionApplication from { cokernel, net M.relations }
     else if M.?generators
     then net new FunctionApplication from { image, net M.generators }
     else if numgens M === 0 then "0"
     else (
	  X := variety F;
	  net new Superscript from {net OO_X, numgens M}
	  )
     )

CoherentSheaf.AfterPrint = F -> (
     X := variety F;
     M := module F;
     << endl;				  -- double space
     n := rank ambient F;
     << "o" << lineNumber() << " : coherent sheaf on " << X;
     if M.?generators then
     if M.?relations then << ", subquotient of " << ambient F
     else << ", subsheaf of " << ambient F
     else if M.?relations then << ", quotient of " << ambient F
     else if n > 0 then (
	  << ", free";
	  if not all(degrees M, d -> all(d, zero))
	  then << ", degrees " << if degreeLength M === 1 then flatten degrees M else degrees M;
	  );
     << endl;
     )

sheaf(Variety,Module) :=  CoherentSheaf => (X,M) -> if M#?(sheaf,X) then M#(sheaf,X) else M#(sheaf,X) = (
     if ring M =!= ring X then error "expected module and variety to have the same ring";
     if instance(X,ProjectiveVariety) and not isHomogeneous M
     then error "expected a homogeneous module";
     new CoherentSheaf from {
     	  symbol module => M,
	  symbol variety => X
	  }
     )
Module ~ := sheaf Module := CoherentSheaf => (M) -> sheaf(Proj ring M,M)

variety = method()
variety CoherentSheaf := Variety => (F) -> F.variety
variety SheafOfRings := Variety => O -> O.variety
ring CoherentSheaf := (F) -> ring F.module
numgens CoherentSheaf := (F) -> numgens F.module
module CoherentSheaf := Module => (F) -> F.module
Ideal * CoherentSheaf := (I,F) -> sheaf(F.variety, I * module F)
CoherentSheaf ++ CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(F.variety, F.module ++ G.module)
tensor(CoherentSheaf,CoherentSheaf) := CoherentSheaf => options -> (F,G) -> F**G
CoherentSheaf ** CoherentSheaf := CoherentSheaf => (F,G) -> prune sheaf(F.variety, F.module ** G.module)
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

annihilator CoherentSheaf := Ideal => (F) -> annihilator F.module

codim   CoherentSheaf := (F) -> codim module F
rank    CoherentSheaf := (F) -> rank  module F
degrees CoherentSheaf := (F) -> degrees module F

exteriorPower(ZZ,CoherentSheaf) := CoherentSheaf => (i,F) -> sheaf(variety F, exteriorPower(i,F.module))

super   CoherentSheaf := CoherentSheaf => (F) -> sheaf super   module F
ambient CoherentSheaf := CoherentSheaf => (F) -> sheaf ambient module F
cover   CoherentSheaf := CoherentSheaf => (F) -> sheaf cover   module F

degreeList := (M) -> (
     if dim M > 0 then error "expected module of finite length";
     H := poincare M;
     T := (ring H)_0;
     H = H // (1-T)^(numgens ring M);
     exponents H / first)

globalSectionsModule := (G,bound) -> (
     -- compute global sections
     M := module G;
     A := ring G;
     M = cokernel presentation M;
     S := saturate image map(M,A^0,0);
     if S != 0 then M = M/S;
     F := presentation A;
     R := ring F;
     N := coker lift(presentation M,R) ** coker F;
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
     prune M)

LowerBound = new SelfInitializingType of BasicList
>  InfiniteNumber := >  ZZ := i -> LowerBound{i+1}
>= InfiniteNumber := >= ZZ := i -> LowerBound{i}
CoherentSheaf(*) := F -> F(>=-infinity)

SumOfTwists = new Type of HashTable
CoherentSheaf LowerBound := SumOfTwists => (F,b) -> new SumOfTwists from { "object" => F, "bound" => b}
net SumOfTwists := S -> net S#"object" | "(>=" | net S#"bound" | ")"

cohomology(ZZ,SumOfTwists) :=  Module => opts -> (i,S) -> (
     F := S#"object";
     R := ring F;
     if not isAffineRing R then error "expected coherent sheaf over a variety over a field";
     b := first S#"bound";
     if i == 0 then globalSectionsModule(F,b) else HH^(i+1)(module F,Degree => b))

cohomology(ZZ,CoherentSheaf) := Module => opts -> (i,F) -> (
     R := ring F;
     if not isAffineRing R then error "expected coherent sheaf over a variety over a field";
     k := coefficientRing R;
     n := rank source basis(0, HH^i F(>=0));
     k^n)

cohomology(ZZ,SheafOfRings) := Module => opts -> (i,O) -> HH^i O^1

structureSheaf := method()				    -- we need good syntax for nameless method packages
structureSheaf(Variety) := (X) -> sheaf_X ring X
sheaf Variety := X -> sheaf_X ring X

OO = new ScriptedFunctor from { subscript => structureSheaf }

--PP = new ScriptedFunctor from {
--     superscript => (
--	  i -> R -> (
--	       x := symbol x;
--	       Proj (R[ x_0 .. x_i ])
--	       )
--	  )
--     }

prune CoherentSheaf := F -> (
     X := variety F;
     sheaf_X prune HH^0 F(>=0)
     )

cotangentSheaf = method()
cotangentSheaf ProjectiveVariety := CoherentSheaf => (X) -> (
     if X.cache.?cotangentSheaf
     then X.cache.cotangentSheaf
     else X.cache.cotangentSheaf = (
	  R := ring X;
	  F := presentation R;
	  prune sheaf(X, homology(vars ring F ** R,jacobian F ** R))
	  )
     )
cotangentSheaf(ZZ,ProjectiveVariety) := CoherentSheaf => (i,X) -> (
     if X#?(cotangentSheaf,i)
     then X#(cotangentSheaf,i) 
     else X#(cotangentSheaf,i) = exteriorPower(i,cotangentSheaf X))

TEST ///
     R = ZZ/101[a,b,c,d]/(a^4+b^4+c^4+d^4)
     X = Proj R
     result = table(3,3,(p,q) -> timing ((p,q) => hilbertFunction(0, HH^q(cotangentSheaf(p,X)))))
     assert( {{1, 0, 1}, {0, 20, 0}, {1, 0, 1}} === applyTable(result,last@@last) )
     print new MatrixExpression from result
     ///

dim AffineVariety := X -> dim ring X
dim ProjectiveVariety := X -> dim ring X - 1
codim ProjectiveVariety := X -> codim ring X
genera ProjectiveVariety := X -> genera ring X
genus ProjectiveVariety := X -> genus ring X
AffineVariety * AffineVariety := AffineVariety => (X,Y) -> Spec(ring X ** ring Y)
AffineVariety ** Ring := AffineVariety => (X,R) -> Spec(ring X ** R)
ProjectiveVariety ** Ring := ProjectiveVariety => (X,R) -> Proj(ring X ** R)

singularLocus(ProjectiveVariety) := X -> (
     R := ring X;
     f := presentation R;
     A := ring f;
     Proj(A / saturate (minors(codim R, jacobian f) + ideal f)))

degree CoherentSheaf := F -> degree F.module
degree ProjectiveVariety := X -> degree ring X

pdim CoherentSheaf := F -> pdim module F

hilbertSeries ProjectiveVariety := options -> X -> hilbertSeries(ring X,options)
hilbertSeries CoherentSheaf := options -> F -> hilbertSeries(module F,options)

hilbertPolynomial ProjectiveVariety := ProjectiveHilbertPolynomial => opts -> X -> hilbertPolynomial(ring X, opts)
hilbertPolynomial CoherentSheaf := options -> F -> hilbertPolynomial(F.module,options)

hilbertFunction(List,CoherentSheaf) := hilbertFunction(ZZ,CoherentSheaf) := (d,F) -> hilbertFunction(d,F.module)
hilbertFunction(List,ProjectiveVariety) := hilbertFunction(ZZ,ProjectiveVariety) := (d,X) -> hilbertFunction(d,ring X)

dual CoherentSheaf := F -> sheaf_(F.variety) dual F.module
betti CoherentSheaf := F -> betti F.module

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
CoherentSheaf ^** ZZ := (F,n) -> binaryPower(F,n,tensor,() -> OO_(F.variety), dual)
degreesRing CoherentSheaf := F -> degreesRing ring F
degreeLength CoherentSheaf := F -> degreeLength ring F

sheafHom = method(TypicalValue => CoherentSheaf)
sheafHom(CoherentSheaf,CoherentSheaf) := (F,G) -> prune sheaf Hom(module F, module G)
Hom(CoherentSheaf,CoherentSheaf) := Module => (F,G) -> HH^0 sheafHom(F,G)

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
sheafExt(ZZ,CoherentSheaf,CoherentSheaf) := (n,F,G) -> prune sheaf Ext^n(module F, module G)
Ext(ZZ,CoherentSheaf,CoherentSheaf) := Module => (n,F,G) -> error "global Ext not implemented yet"
Ext(ZZ,CoherentSheaf,SumOfTwists) := Module => (n,F,G') -> error "global Ext not implemented yet"
