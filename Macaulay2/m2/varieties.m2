--		Copyright 1993-1998 by Daniel R. Grayson

Variety = new Type of MutableHashTable
document { quote Variety,
     TT "Variety", " -- the class of all algebraic varieties.",
     PARA,
     NOINDENT,
     "Types of algebraic varieties:",
     MENU {
	  TO "AffineVariety",
	  TO "ProjectiveVariety",
	  },
     SEEALSO "CoherentSheaf"
     }

AffineVariety = new Type of Variety
document { quote AffineVariety,
     TT "AffineVariety", " -- the class of all algebraic varieties, a subclass of
     ", TO "Variety", ".",
     PARA,
     "To create an affine variety, use ", TO "Spec", ".",
     EXAMPLE {
	  "R = QQ[x,y];",
	  "Spec R"
	  },
     SEEALSO "CoherentSheaf"
     }

ProjectiveVariety = new Type of Variety
document { quote ProjectiveVariety,
     TT "ProjectiveVariety", " -- the class of all projective varieties, a subclass of
     ", TO "Variety", ".",
     PARA,
     "To create an affine variety, use ", TO "Proj", ".",
     EXAMPLE {
	  "R = QQ[x,y];",
	  "Proj R"
	  },
     SEEALSO "CoherentSheaf"
     }

ring Variety := (X) -> X.ring		  -- documented in "ring"
Spec = method()
expression AffineVariety := (X) -> new FunctionApplication from { Spec, X.ring }
net AffineVariety := (X) -> net expression X
Spec Ring := (R) -> if R.?Spec then R.Spec else R.Spec = (
     new AffineVariety from {
     	  quote ring => R
     	  }
     )
document { quote Spec,
     TT "Spec R", " -- create an affine variety or scheme from the ring ", TT "R", ".",
     PARA,
     EXAMPLE {
	  "R = QQ[x,y];",
	  "Spec R"
	  },
     SEEALSO "AffineVariety"
     }

Proj = method()
expression ProjectiveVariety := (X) -> new FunctionApplication from { Proj, X.ring }
net ProjectiveVariety := (X) -> net expression X
Proj Ring := (R) -> if R.?Proj then R.Proj else R.Proj = (
     if not isHomogeneous R then error "expected a homgeneous ring";
     new ProjectiveVariety from {
     	  quote ring => R
     	  }
     )
document { quote Proj,
     TT "Proj R", " -- create a projective variety or scheme from the graded ring ", TT "R", ".",
     PARA,
     EXAMPLE {
	  "R = QQ[x,y];",
	  "Proj R"
	  },
     SEEALSO "ProjectiveVariety"
     }

CoherentSheaf = new Type of MutableHashTable
document { quote CoherentSheaf,
     TT "CoherentSheaf", " -- the class of all coherent sheaves on varieties or schemes.",
     PARA,
     NOINDENT,
     "Methods for making coherent sheaves:",
     MENU {
	  TO "OO",
	  TO "sheaf",
	  TO "cotangentSheaf",
	  TO (quote ~, Module),
	  TO (quote ~, Ring),
	  },
     PARA,
     NOINDENT,
     "Operations on coherent sheaves:",
     MENU {
	  TO variety,
	  TO (ring, CoherentSheaf),
	  TO (module, CoherentSheaf),
	  TO (quote ++, CoherentSheaf, CoherentSheaf),
	  TO (quote **, CoherentSheaf, CoherentSheaf),
	  TO (quote " ", CoherentSheaf, ZZ),
	  TO (quote /, CoherentSheaf, CoherentSheaf),
	  TO annihilator,
	  TO (codim, CoherentSheaf),
	  TO (rank, CoherentSheaf),
	  TO (exteriorPower, ZZ, CoherentSheaf),
	  TO (degrees, CoherentSheaf),
	  TO (cohomology,ZZ,CoherentSheaf),
	  }
     }

expression CoherentSheaf := F -> new FunctionApplication from { sheaf, F.module }
net CoherentSheaf := (F) -> net expression F
sheaf = method()
document { quote sheaf,
     TT "sheaf", " -- a function used for creating sheaves on varieties.",
     PARA,
     NOINDENT,
     "Here are the ways to use it:",
     MENU {
	  TO (sheaf, Module),
	  TO (sheaf, Ring),
	  TO (sheaf, Module, Variety),
	  },
     SEEALSO "CoherentSheaf"
     }

sheaf(Module,Variety) := (M,X) -> if M#?(sheaf,X) then M#(sheaf,X) else M#(sheaf,X) = (
     if ring M =!= ring X then error "expected module and variety to have the same ring";
     if instance(X,ProjectiveVariety) and not isHomogeneous M
     then error "expected a homogeneous module";
     new CoherentSheaf from {
     	  quote module => M,
	  quote variety => X
	  }
     )
document { (sheaf, Module, Variety),
     TT "sheaf(M,X)", " -- produce the coherent sheaf on the variety ", TT "X", " corresponding
     to the module ", TT "M", ".",
     PARA,
     "If ", TT "X", " is the affine variety ", TT "Spec R", ", then ", TT "M", " should be an ", TT "R", "-module.  If ", TT "X", " is 
     the projective variety ", TT "Proj R", ", then ", TT "M", " should be a homogeneous ", TT "R", "-module.",
     PARA,
     SEEALSO "CoherentSheaf"
     }

Module ~ := sheaf Module := (M) -> sheaf(M,Proj ring M)
document { (sheaf, Module),
     TT "sheaf M", " -- produce the coherent sheaf on a projective variety ", TT "X", "
     corresponding to a homegeneous module ", TT "M", ".",
     PARA,
     SEEALSO "CoherentSheaf"
     }

document { (quote ~, Module),
     TT "M~", " -- produce the coherent sheaf on a projective variety ", TT "X", "
     corresponding to a homegeneous module ", TT "M", ".",
     PARA,
     SEEALSO "CoherentSheaf"
     }

Ring ~ := sheaf Ring := (R) -> sheaf R^1
document { (sheaf, Ring),
     TT "sheaf R", " -- produce the structure sheaf on the projective variety ", TT "Proj R", ".",
     PARA,
     SEEALSO "CoherentSheaf"
     }

document { (quote ~, Ring),
     TT "R~", " -- produce the structure sheaf on the projective variety ", TT "Proj R", ".",
     PARA,
     SEEALSO "CoherentSheaf"
     }

variety = method()
variety CoherentSheaf := (F) -> F.variety
document { (variety, CoherentSheaf),
     TT "variety F", " -- produce the variety over which a coherent sheaf is defined.",
     PARA,
     EXAMPLE {
	  "X = Proj(QQ[x,y,z])",
	  "OO_X(3)",
	  "variety oo"
	  },
     PARA,
     SEEALSO "CoherentSheaf"
     }
document { quote variety,
     TT "variety F", " -- produce the variety associated to ", TT "F", ".",
     PARA,
     MENU {
	  TO (variety, CoherentSheaf),
	  }
     }

ring CoherentSheaf := (F) -> ring F.module
document { (ring, CoherentSheaf),
     TT "ring F", " -- produce the coordinate ring of the variety on which a coherent sheaf
     ", TT "F", " is defined.",
     PARA,
     SEEALSO "CoherentSheaf"
     }

module CoherentSheaf := (F) -> F.module
document { (module, CoherentSheaf),
     TT "module F", " -- produce the module from which the coherent sheaf ", TT "F", " was defined.",
     PARA,
     EXAMPLE {
	  "X = Proj(QQ[x,y,z])",
	  "F = OO_X(3)",
	  "module F",
	  "degrees oo",
	  },
     SEEALSO "CoherentSheaf"
     }

CoherentSheaf ++ CoherentSheaf := (F,G) -> sheaf(F.module ++ G.module)
document { (quote ++, CoherentSheaf, CoherentSheaf),
     TT "F ++ G", " -- direct sum of coherent sheaves.",
     PARA,
     SEEALSO "CoherentSheaf"
     }

CoherentSheaf ** CoherentSheaf := (F,G) -> sheaf(F.module ** G.module)
document { (quote **, CoherentSheaf, CoherentSheaf),
     TT "F ** G", " -- direct sum of coherent sheaves.",
     PARA,
     SEEALSO "CoherentSheaf"
     }

CoherentSheaf ZZ := (F,n) -> sheaf(F.module ** (ring F)^{n})
document { (quote " ", CoherentSheaf, ZZ),
     TT "F(n)", " -- twist a coherent sheaf F on a projective variety by
     the n-th power of the canonical line bundle.",
     PARA,
     EXAMPLE {
	  "X = Proj(QQ[x,y,z])",
	  "F = OO_X",
	  "G = F(3)",
	  "module G",
	  "degrees oo",
	  },
     SEEALSO "CoherentSheaf"
     }

CoherentSheaf / CoherentSheaf := (F,G) -> sheaf(F.module / G.module)
document { (quote /, CoherentSheaf, CoherentSheaf),
     TT "F / G", " -- quotient of coherent sheaves.",
     PARA,
     SEEALSO "CoherentSheaf"
     }

annihilator CoherentSheaf := (F) -> annihilator F.module

codim CoherentSheaf := (F) -> codim F.module
document { (codim, CoherentSheaf),
     TT "codim F", " -- calculate the codimension of the support of a coherent sheaf.",
     PARA,
     SEEALSO "CoherentSheaf"
     }

rank CoherentSheaf := (F) -> rank F.module
document { (rank, CoherentSheaf),
     TT "rank F", " -- calculate the rank of a coherent sheaf.",
     PARA,
     SEEALSO "CoherentSheaf"
     }

exteriorPower(ZZ,CoherentSheaf) := (i,F) -> sheaf(exteriorPower(i,F.module))
document { (exteriorPower, ZZ, CoherentSheaf),
     TT "exteriorPower(i,F)", " -- calculate the ", TT "i", "-th exterior power of a coherent sheaf
     ", TT "F", ".",
     PARA,
     SEEALSO "CoherentSheaf"
     }

degrees CoherentSheaf := (F) -> degrees F.module
document { (degrees, CoherentSheaf),
     TT "degrees F", " -- produce a list of the degrees of the generators of the module
     defining a coherent sheaf ", TT "F", ".",
     PARA,
     SEEALSO "CoherentSheaf"
     }

degreeList := (M) -> (
     if dim M > 0 then error "expected module of finite length";
     H := poincare M;
     T := (ring H)_0;
     H = H // (1-T)^(numgens ring M);
     exponents H / first)

cohomology(ZZ,CoherentSheaf) := (i,G,opts) -> (
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

document { (cohomology, ZZ, CoherentSheaf),
     TT "HH^i(F)", " -- for a coherent sheaf F on a projective variety X, computes
     the direct sum over natural numbers n, of the cohomology groups of F(n).",
     BR,
     NOINDENT,
     TT "HH^i(F, Degree=>e)", " -- same as above, but n ranges over the integers
     at least as large as e.",
     PARA,
     EXAMPLE {
	  "R = QQ[a,b,c,d]/(a^4+b^4+c^4+d^4);",
	  "X = Proj R",
	  "HH^1(cotangentSheaf X)",
	  "hilbertFunction(0,oo)"
	  },
     SEEALSO "CoherentSheaf"
     }

structureSheaf := method()		  -- private
structureSheaf(Variety) := (X) -> sheaf((ring X)^1, X)

OO = new ScriptedFunctor from { subscript => structureSheaf }
document { quote OO,
     TT "OO_X", " -- produce the structure sheaf on a variety ", TT "X", ".",
     SEEALSO "CoherentSheaf"
     }

--PP = new ScriptedFunctor from {
--     superscript => (
--	  i -> R -> (
--	       x := quote x;
--	       Proj (R[ x_0 .. x_i ])
--	       )
--	  )
--     }

cotangentSheaf = method()
cotangentSheaf ProjectiveVariety := (X) -> (
     if X.?cotangentSheaf then X.cotangentSheaf else X.cotangentSheaf = (
	  R := ring X;
	  F := presentation R;
	  sheaf(prune homology(vars ring F ** R,jacobian F ** R), X)
	  )
     )
document { (cotangentSheaf, ProjectiveVariety),
     TT "cotangentSheaf X", " -- calculate the cotangent sheaf of a variety ", TT "X", ".",
     PARA,
     SEEALSO {"CoherentSheaf", (cotangentSheaf, ZZ, ProjectiveVariety)}
     }
document { quote cotangentSheaf,
     TT "cotangentSheaf", " -- a function used for producing the contangent sheaf
     of a variety, or exterior powers of it.",
     PARA,
     MENU {
	  TO (cotangentSheaf, ProjectiveVariety),
	  TO (cotangentSheaf, ZZ, ProjectiveVariety),
	  },
     SEEALSO "CoherentSheaf"
     }

cotangentSheaf(ZZ,ProjectiveVariety) := (i,X) -> (
     if X#?(cotangentSheaf,i)
     then X#(cotangentSheaf,i) 
     else X#(cotangentSheaf,i) = exteriorPower(i,cotangentSheaf X))
document { (cotangentSheaf, ZZ, ProjectiveVariety),
     TT "cotangentSheaf(p,X)", " -- calculate the ", TT "p", "-th exterior power of
     the cotangent sheaf of a variety ", TT "X", ".",
     PARA,
     SEEALSO {"CoherentSheaf", (cotangentSheaf, ProjectiveVariety)}
     }

TEST ///
     R = ZZ/101[a,b,c,d]/(a^4+b^4+c^4+d^4)
     X = Proj R
     result = table(3,3,(p,q) -> timing ((p,q) => hilbertFunction(0, HH^q(cotangentSheaf(p,X)))))
     assert( {{1, 0, 1}, {0, 20, 0}, {1, 0, 1}} === applyTable(result,last@@last) )
     print new MatrixExpression from result
     ///
