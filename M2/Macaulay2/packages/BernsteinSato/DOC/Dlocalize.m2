
document {
     Key => {[Dlocalize,Strategy],[DlocalizeAll,Strategy],[DlocalizeMap,Strategy],
	 Oaku,OTW,OTWcyclic},
     Headline=>"strategy for computing a localization of a D-module",
     UL{
	  {BOLD "Oaku", " -- use Oaku's algorithm"},
	  {BOLD "OTW", " -- use Oaku-Takayama-Walther's algorithm"},
	  {BOLD "OTWcyclic", " -- use Oaku-Takayama-Walther's algorithm for a cyclic module"}
	  }
     }

document {
     Key => {Dlocalize, (Dlocalize,Ideal,RingElement), (Dlocalize,Module,RingElement)},
     Headline => "localization of a D-module",

     Usage => "Dlocalize(M,f), Dlocalize(I,f)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "f" => RingElement => "a polynomial",
	  },
     Outputs => {
	  Module => {"the localized module ", TEX "M_f = M[f^{-1}]", " as a D-module"}
	  },
     "One of the nice things about D-modules is that if a finitely
     generated D-module is specializable along ", EM "f", ", then it's localization 
     with respect to ", EM "f", " is also finitely generated.  For instance,
     this is true for all holonomic D-modules.",
     
     PARA{},
     "There are two different algorithms for localization implemented.  
     The first appears in the
     paper 'A localization algorithm for D-modules' by Oaku-Takayama-Walther
     (1999).  The second is due to Oaku and appears in the paper
     'Algorithmic computation of local cohomology modules and the
     cohomological dimension of algebraic varieties' by Walther(1999)",

     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
     	M = W^1/(ideal(x*Dx+1, Dy))
     	f = x^2-y^3
     	Mf = Dlocalize(M, f)
	///,
     SeeAlso => {"DlocalizeAll", "DlocalizeMap", "AnnFs", "Dintegration"}
     }

document {
     Key => {DlocalizeMap, (DlocalizeMap,Ideal,RingElement), (DlocalizeMap,Module,RingElement)},
     Headline => "localization map from a D-module to its localization",
     Usage => "DlocalizeMap(M,f), DlocalizeMap(I,f)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "f" => RingElement => "a polynomial",
	  },
     Outputs => {
	  Matrix => {"which represents the natural map from ", TEX "M", " to ", TEX "M_f = M[f^{-1}]"}
	  },
     "A supplementary function for ", TO "Dlocalize", 
     " that computes the localization map.",
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
     	M = W^1/(ideal(x*Dx+1, Dy))
     	f = x^2-y^3
     	DlocalizeMap(M, f)
	///,
     SeeAlso => {"Dlocalize", "AnnFs", "Dintegration"}
     }


document {
     Key => {DlocalizeAll, (DlocalizeAll,Ideal,RingElement), (DlocalizeAll,Module,RingElement)},
     Headline => "localization of a D-module (extended version)",
     Usage => "DlocalizeAll(M,f), DlocalizeAll(I,f)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "f" => RingElement => "a polynomial",
	  },
     Outputs => {
	  HashTable => {"which contains the localized module", TEX "M_f = M[f^{-1}]", 
	       " and some additional information"}
	  },
     "An extension of ", TO "Dlocalize", 
     " that in addition computes the localization map, 
     the b-function, and the power ", TEX "s", " of the generator ", TEX "f^s", ".",
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
     	M = W^1/(ideal(x*Dx+1, Dy))
     	f = x^2-y^3
     	DlocalizeAll(M, f)
	///,
     SeeAlso => {"Dlocalize", "AnnFs", "Dintegration"}
     }

     TT "DlocalizeAll (M, f)", " -- 
     compute the localization of M with respect to f and
     some auxiliary information",
     BR{},
     TT "DlocalizeAll (M, f)", " -- 
     compute the localization of D/I with respect to f and some
     auxiliary information",
     PARA{},


document {
     Key => LocModule,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => GeneratorPower,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => LocMap,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => annFS,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => IntegrateBfunction,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
document {
     Key => Bfunction,
     Headline => "a key created by Dlocalize",
     "See ", TO "Dlocalize", "."
     }
