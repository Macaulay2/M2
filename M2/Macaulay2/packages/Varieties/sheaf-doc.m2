document {
     Key => CoherentSheaf,
     Headline => "the class of all coherent sheaves"
     }

document {
     Key => sheaf,
     Headline => "make a coherent sheaf"
     }

document {
     Key => (sheaf, Variety, Module),
     Headline => "make a coherent sheaf",
     Usage => "sheaf(X,M)",
     Inputs => {"X","M"},
     Outputs => {{ "the coherent sheaf on the variety ", TT "X", " corresponding to the module ", TT "M" }},
     PARA{
     	  "If ", TT "X", " is the affine variety ", TT "Spec R", ", then ", TT "M", " should be an ", TT "R", "-module.  If ", TT "X", " is
     	  the projective variety ", TT "Proj R", ", then ", TT "M", " should be a homogeneous ", TT "R", "-module."
	  }
     }

document {
     Key => (sheaf, Variety, Ring),
     Headline => "make a coherent sheaf of rings",
     TT "sheaf(X,R)", " -- produce the coherent sheaf on the variety ", TT "X", " corresponding
     to the ring ", TT "R", ".  The variety ", TT "X", " must be ", TT "Spec R", " or ", TT "Proj R", ".",
     EXAMPLE lines ///
     R = QQ[x,y,z]
     X = Proj R
     Y = Spec R
     sheaf(X,R)
     sheaf(Y,R)
     ///}

document {
     Key => (sheaf, Variety),
     Headline => "make a coherent sheaf",
     Usage => "sheaf X",
     Inputs => {"X"},
     Outputs => {{ "the structure sheaf of rings on the variety ", TT "X" }},
     EXAMPLE lines ///
     R = QQ[x,y,z]
     X = Proj R
     Y = Spec R
     sheaf X
     sheaf Y
     ///
     }

document {
     Key => {(sheaf, Module),(symbol ~, Module)},
     Headline => "make a coherent sheaf",
     Usage => "sheaf M\nM~",
     Inputs => {"M" => "homogeneous" },
     Outputs => {{ "the coherent sheaf on a projective variety ", TT "X", " corresponding to ", TT "M" }},
     EXAMPLE lines ///
     R = QQ[x,y,z];
     X = Proj R
     M = R^{1,2,3}
     sheaf M
     M~
     ///
     }

document {
     Key => {(sheaf, Ring),(symbol ~, Ring)},
     Headline => "make a coherent sheaf of rings",
     Usage => "sheaf R\nR~",
     Inputs => {"R"},
     Outputs => {{"the coherent sheaf on a projective variety ", TT "X", " corresponding to ", TT "M"}},
     EXAMPLE lines ///
     R = QQ[x,y,z];
     X = Proj R
     sheaf R
     R~
     ///
     }

document {
     Key => {(Proj, Ring), Proj},
     Headline => "make a projective variety",
     Usage => "Proj R",
     Inputs => {"R"},
     Outputs => {{ "the projective variety (or scheme) formed from the graded ring ", TT "R" }},
     EXAMPLE lines ///
     R = QQ[x,y];
     Proj R
     ///
     }


document {
     Key => (module, CoherentSheaf),
     Headline => "get the module defining a coherent sheaf",
     Usage => "module F",
     Inputs => {"F"},
     Outputs => {{"the module from which the coherent sheaf ", TT "F", " was defined"}},
     EXAMPLE lines ///
     X = Proj(QQ[x,y,z])
     F = OO_X(3)
     module F
     degrees oo
     ///,
     SeeAlso => { OO, degrees, Proj }
     }

document {
     Key => (symbol ++, CoherentSheaf, CoherentSheaf),
     Headline => "direct sum of coherent sheaves",
     Usage => "F ++ G",
     Inputs => {"F","G"},
     Outputs => {{"the direct sum of ", TT "F", " and ", TT "G"}},
     EXAMPLE lines ///
     X = Proj(QQ[x,y,z])
     OO_X(3) ++ OO_X(4)
     module oo
     ///
     }

document {
     Key => (symbol **, CoherentSheaf, CoherentSheaf),
     Headline => "tensor produce of coherent sheaves",
     Usage => "F ** G",
     Inputs => {"F","G"},
     Outputs => {{"the tensor product of ", TT "F", " and ", TT "G"}},
     EXAMPLE lines ///
     X = Proj(QQ[x,y,z])
     OO_X(-3) ++ OO_X(4)
     oo ** oo
     ///
     }

document {
     Key => {(symbol SPACE, CoherentSheaf, ZZ), (symbol SPACE, SheafOfRings, ZZ)},
     Headline => "canonical twist of a coherent sheaf",
     Usage => "F(n)",
     Inputs => {"F" => {"or ", ofClass SheafOfRings, ", on a projective variety"}, "n"},
     Outputs => { CoherentSheaf => "the twist of F on a projective variety by the n-th power of the hyperplane line bundle." },
     EXAMPLE lines ///
     X = Proj(QQ[x,y,z])
     F = OO_X
     G = F(3)
     module G
     degrees oo
     ///
     }

document {
     Key => {(symbol /, CoherentSheaf, CoherentSheaf), (symbol /, CoherentSheaf, Ideal)},
     Headline => "quotient of coherent sheaves",
     Usage => "F / G",
     Inputs => { "F", "G" => {"or ", ofClass Ideal} },
     Outputs => { CoherentSheaf => {"the quotient sheaf ", TT "F/G"} },
     "We compute the cohomology of two sheaves supported on an elliptic curve.",
     EXAMPLE lines ///
     X = Proj(QQ[x,y,z])
     I = ideal(y^2*z-x*(x-z)*(x-11*z))
     N = (sheaf module I)/(sheaf module I^2)
     G = OO_X^1/I
     HH^1(G)
     HH^1(N)
     ///,
     SeeAlso => {Proj, Spec, sheaf, (cohomology,ZZ,CoherentSheaf), OO}
     }

document {
     Key => (exteriorPower, ZZ, CoherentSheaf),
     Usage => "exteriorPower(i,F)",
     Inputs => {"i","F"},
     Outputs => {{ "the ", TT "i", "-th exterior power of ", TT "F"}}
     }

