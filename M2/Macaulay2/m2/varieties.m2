--		Copyright 1993-1998 by Daniel R. Grayson

Variety = new Type of MutableHashTable
AffineVariety = new Type of Variety
ProjectiveVariety = new Type of Variety
ring Variety := (X) -> X.ring
Spec = method()
expression AffineVariety := (X) -> new FunctionApplication from { Spec, X.ring }
net AffineVariety := (X) -> net expression X
Spec Ring := AffineVariety => (R) -> if R.?Spec then R.Spec else R.Spec = (
     new AffineVariety from {
     	  symbol ring => R
     	  }
     )
Proj = method()
expression ProjectiveVariety := (X) -> new FunctionApplication from { Proj, X.ring }
net ProjectiveVariety := (X) -> net expression X
Proj Ring := ProjectiveVariety => (R) -> if R.?Proj then R.Proj else R.Proj = (
     if not isHomogeneous R then error "expected a homgeneous ring";
     new ProjectiveVariety from {
     	  symbol ring => R
     	  }
     )
CoherentSheaf = new Type of MutableHashTable
expression CoherentSheaf := F -> new FunctionApplication from { sheaf, F.module }
net CoherentSheaf := (F) -> net expression F
sheaf = method(TypicalValue => CoherentSheaf)
sheaf(Module,Variety) := (M,X) -> if M#?(sheaf,X) then M#(sheaf,X) else M#(sheaf,X) = (
     if ring M =!= ring X then error "expected module and variety to have the same ring";
     if instance(X,ProjectiveVariety) and not isHomogeneous M
     then error "expected a homogeneous module";
     new CoherentSheaf from {
     	  symbol module => M,
	  symbol variety => X
	  }
     )
Module ~ := sheaf Module := CoherentSheaf => (M) -> sheaf(M,Proj ring M)
Ring ~ := sheaf Ring := CoherentSheaf => (R) -> sheaf R^1
variety = method()
variety CoherentSheaf := Variety => (F) -> F.variety
ring CoherentSheaf := (F) -> ring F.module
module CoherentSheaf := Module => (F) -> F.module
CoherentSheaf ++ CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(F.module ++ G.module)
CoherentSheaf ** CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(F.module ** G.module)
CoherentSheaf ZZ := CoherentSheaf => (F,n) -> sheaf(F.module ** (ring F)^{n})
CoherentSheaf / CoherentSheaf := CoherentSheaf => (F,G) -> sheaf(F.module / G.module)
annihilator CoherentSheaf := Ideal => (F) -> annihilator F.module
codim CoherentSheaf := (F) -> codim F.module
rank CoherentSheaf := (F) -> rank F.module
exteriorPower(ZZ,CoherentSheaf) := CoherentSheaf => (i,F) -> sheaf(exteriorPower(i,F.module))
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
	  e := opts.Degree;
	  M = M / saturate 0_M;
	  A := ring M;
	  F := presentation A;
	  R := ring F;
	  N := coker lift(presentation M,R) ** coker F;
	  r := numgens R;
	  wR := R^{-r};
	  if pdim N < r-1
	  then M
	  else (
	       E1 := Ext^(r-1)(N,wR);
	       p := max(0,
		    if dim E1 <= 0 
		    then max degreeList E1 - min degreeList E1 + 1
		    else min degrees E1 + e + 1
		    );
	       J := ideal apply(numgens A, j -> A_j^p);
	       Hom(module J,M)
	       )
	  )
     )

structureSheaf := method()		  -- private
structureSheaf(Variety) := (X) -> sheaf((ring X)^1, X)

OO = new ScriptedFunctor from { subscript => structureSheaf }

--PP = new ScriptedFunctor from {
--     superscript => (
--	  i -> R -> (
--	       x := symbol x;
--	       Proj (R[ x_0 .. x_i ])
--	       )
--	  )
--     }

cotangentSheaf = method()
cotangentSheaf ProjectiveVariety := CoherentSheaf => (X) -> (
     if X.?cotangentSheaf
     then X.cotangentSheaf
     else X.cotangentSheaf = (
	  R := ring X;
	  F := presentation R;
	  sheaf( prune 
	       homology(vars ring F ** R,jacobian F ** R), X)))
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
