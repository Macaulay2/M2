--		Copyright 1993-1998 by Daniel R. Grayson

Variety = new Type of HashTable
Variety.synonym = "variety"
AffineVariety = new Type of Variety
AffineVariety.synonym = "affine variety"
ProjectiveVariety = new Type of Variety
ProjectiveVariety.synonym = "projective variety"
ring Variety := X -> X.ring
ideal Variety := X -> ideal ring X
hilbertPolynomial Variety := ProjectiveHilbertPolynomial => opts -> X -> hilbertPolynomial ring X
Spec = method()
expression AffineVariety := (X) -> new FunctionApplication from { Spec, X.ring }
net AffineVariety := (X) -> net expression X
Spec Ring := AffineVariety => (R) -> if R.?Spec then R.Spec else R.Spec = (
     new AffineVariety from {
     	  symbol ring => R,
	  symbol cache => new CacheTable
     	  }
     )
Proj = method()
expression ProjectiveVariety := (X) -> new FunctionApplication from { Proj, X.ring }
net ProjectiveVariety := (X) -> net expression X
Proj Ring := ProjectiveVariety => (R) -> if R.?Proj then R.Proj else R.Proj = (
     if not isHomogeneous R then error "expected a homgeneous ring";
     new ProjectiveVariety from {
     	  symbol ring => R,
	  symbol cache => new CacheTable
     	  }
     )
CoherentSheaf = new Type of MutableHashTable
CoherentSheaf.synonym = "coherent sheaf"
expression CoherentSheaf := F -> new FunctionApplication from { sheaf, F.module }
net CoherentSheaf := (F) -> net expression F
sheaf = method(TypicalValue => CoherentSheaf)
sheaf(Variety,Module) := (X,M) -> if M#?(sheaf,X) then M#(sheaf,X) else M#(sheaf,X) = (
     if ring M =!= ring X then error "expected module and variety to have the same ring";
     if instance(X,ProjectiveVariety) and not isHomogeneous M
     then error "expected a homogeneous module";
     new CoherentSheaf from {
     	  symbol module => M,
	  symbol variety => X
	  }
     )
Module ~ := sheaf Module := CoherentSheaf => (M) -> sheaf(Proj ring M,M)
Ring ~ := sheaf Ring := CoherentSheaf => (R) -> sheaf R^1
variety = method()
variety CoherentSheaf := Variety => (F) -> F.variety
ring CoherentSheaf := (F) -> ring F.module
module CoherentSheaf := Module => (F) -> F.module
Ideal * CoherentSheaf := (I,F) -> sheaf(variety F, I * module F)
CoherentSheaf ++ CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(variety F, F.module ++ G.module)
CoherentSheaf ** CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(variety F, F.module ** G.module)
CoherentSheaf ZZ := CoherentSheaf => (F,n) -> sheaf(variety F, F.module ** (ring F)^{n})
CoherentSheaf / CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(variety F, F.module / G.module)
annihilator CoherentSheaf := Ideal => (F) -> annihilator F.module
codim CoherentSheaf := (F) -> codim F.module
rank CoherentSheaf := (F) -> rank F.module
exteriorPower(ZZ,CoherentSheaf) := CoherentSheaf => (i,F) -> sheaf(variety F, exteriorPower(i,F.module))
degrees CoherentSheaf := (F) -> degrees F.module

degreeList := (M) -> (
     if dim M > 0 then error "expected module of finite length";
     H := poincare M;
     T := (ring H)_0;
     H = H // (1-T)^(numgens ring M);
     exponents H / first)

cohomology(ZZ,CoherentSheaf) :=  Module => opts -> (i,G) -> (
     M := module G;
     if i =!= 0 
     then HH^(i+1)(M,opts)
     else (
	  -- compute global sections
	  A := ring M;
	  M = cokernel presentation M;
	  M = M / saturate image map(M,A^0,0);
	  F := presentation A;
	  R := ring F;
	  N := coker lift(presentation M,R) ** coker F;
	  r := numgens R;
	  wR := R^{-r};
	  if pdim N >= r-1 then (
	       E1 := Ext^(r-1)(N,wR);
	       p := 1 + opts.Degree + (
		    if dim E1 <= 0 
		    then (max degreeList E1 - min degreeList E1)
		    else (- first min degrees E1)
		    );
	       if p > 0 then M = Hom(image matrix {apply(numgens A, j -> A_j^p)}, M);
	       );
	  M))

structureSheaf := method()		  -- private
structureSheaf(Variety) := (X) -> sheaf(X, (ring X)^1)

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
     sheaf_X prune HH^0 F
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
AffineVariety * AffineVariety := AffineVariety => (X,Y) -> Spec(ring X ** ring Y)
AffineVariety ** Ring := AffineVariety => (X,R) -> Spec(ring X ** R)
ProjectiveVariety ** Ring := ProjectiveVariety => (X,R) -> Proj(ring X ** R)

singularLocus(ProjectiveVariety) := X -> (
     R := ring X;
     f := presentation R;
     A := ring f;
     Proj(A / saturate (minors(codim R, jacobian f) + ideal f)))

degree CoherentSheaf := F -> degree F.module
hilbertSeries CoherentSheaf := options -> F -> hilbertSeries(F.module,options)
hilbertPolynomial CoherentSheaf := options -> F -> hilbertPolynomial(F.module,options)
hilbertFunction(List,CoherentSheaf) := 
hilbertFunction(ZZ,CoherentSheaf) := (d,F) -> hilbertFunction(d,F.module)
