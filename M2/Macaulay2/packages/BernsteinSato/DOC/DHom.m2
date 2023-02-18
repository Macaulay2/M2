document {
     Key => [DHom,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}
document {
     Key => {DHom, (DHom,Module,Module), (DHom,Module,Module,List), (DHom,Ideal,Ideal)},
     Headline=>"D-homomorphisms between holonomic D-modules",
     Usage => "DHom(M,N), DHom(M,N,w), DHom(I,J)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "N" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "J" => Ideal => {"which represents the module ", EM "N = D/J"},
	  "w" => List => "a positive weight vector"  
	  },
     Outputs => { 
	  HashTable => {" a basis of D-homomorphisms between holonomic D-modules ", 
	       EM "M", " and ", EM "N" }
     	  },
     "The set of D-homomorphisms between two holonomic modules ", EM "M",
     " and ", EM "N",
     " is a finite-dimensional vector space over the ground field.
     Since a homomorphism is defined by where it sends a set of generators,
     the output of this command is a list of matrices whose columns
     correspond to the images of the generators of ", EM "M", ".  Here
     the generators of ", EM "M", " are determined from its presentation
     by generators and relations.",
     PARA {
	  "The procedure calls ", TO "Drestriction", ", which uses ", 
	  EM "w", " if specified."
	  },
     PARA {
	  "The algorithm used appears in the paper 'Computing homomorphisms
	  between holonomic D-modules' by Tsai-Walther(2000).
	  The method is to combine isomorphisms of Bjork and Kashiwara with
	  the restriction algorithm."},
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     M = W^1/ideal(D-1)
	     N = W^1/ideal((D-1)^2)
	     DHom(M,N)
	     ///,
     Caveat => {"Input modules ", EM "M", ", ", EM "N", ", ", 
	  EM "D/I", " and ", EM "D/J", " should be holonomic."},
     SeeAlso => {"DExt", "Drestriction"}
     }

document {
     Key => [DExt,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}
document {
     Key => [DExt,Special]
     }
document {
     Key => Special,
     SeeAlso => "DExt"
     }
document {
     Key => None,
     Headline => "an option for DExt=>Special",
     SeeAlso => "DExt"
     }
document {
     Key => [DExt,Output]}
document {
     Key => Output
     }
document {
     Key => [DExt,Info]
     }
document {
     Key => Info
     }
document {
     Key => {DExt, (DExt, Module, Module), (DExt, Module, Module, List)},
     Headline => "Ext groups between holonomic modules",
     Usage => "DExt(M,N), DExt(M,N,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "N" => Module => {"over the Weyl algebra ", EM "D"},
	  "w" => List => "a positive weight vector"  
	  },
     Outputs => { 
	  HashTable => {" the ", 
	       TT "Ext"," groups between holonomic D-modules", EM "M", 
	       " and ", EM "N" }
     	  },
     "The ", TEX "Ext", " groups between D-modules ", EM "M"," and ", EM "N",
     " are the derived functors of ", TEX "Hom", ", and are finite-dimensional
     vector spaces over the ground field when ", EM "M", " and ", EM "N", " are holonomic.",
     PARA {
	  "The procedure calls ", TO "Drestriction", ", which uses ", 
	  EM "w", " if specified."
	  },
     PARA {
	  "The algorithm used appears in the paper 'Polynomial and
	  rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
	  The method is to combine isomorphisms of Bjork and Kashiwara with
	  the restriction algorithm."},
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     M = W^1/ideal(x*(D-1))
	     N = W^1/ideal((D-1)^2)
	     DExt(M,N)
	     ///,
     Caveat =>{
	  "Input modules M, N should be holonomic.",
	  "Does not yet compute explicit representations of Ext groups
	  such as Yoneda representation."
	  },
     SeeAlso => {"DHom", "Drestriction"}
     }

document {
     Key => {Ddual, (Ddual,Module), (Ddual,Ideal)},
     Headline => "holonomic dual of a D-module",
     Usage => "Ddual M, Ddual I",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"}
	  },
     Outputs => {
	  Module => {"the holonomic dual of ", EM "M"}
	  },
     "If M is a holonomic left D-module, then ", 
     BOLD "Ext", SUP "n", SUB "D", "(", EM "M,D", ")", 
     " is a holonomic right D-module.  The holonomic dual is defined to be the left
     module associated to ",
     BOLD "Ext", SUP "n", SUB "D", "(", EM "M,D", ")", 
     ".  The dual is obtained by
     computing a free resolution of ", EM "M", ", dualizing, and applying
     the standard transposition to the ", EM "n", "-th homology.",
     EXAMPLE lines ///
             I = AppellF1({1,0,-3,2})
	     Ddual I
	     ///,
     Caveat =>{"The input module ", EM "M", " should be holonomic.  The user should
	  check this manually with the script ", TT "Ddim", "."},
     SeeAlso => {"Ddim", "Dtransposition"}
     }

document {
     Key => [polynomialExt,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}
document {
     Key => {polynomialExt, (polynomialExt,Module), (polynomialExt,ZZ,Ideal), (polynomialExt,ZZ,Module), (polynomialExt,Ideal)},
     Headline => "Ext groups between a holonomic module and a polynomial ring",
     Usage => "polynomialExt M, polynomialExt I; RatExt(i,M), RatExt(i,I)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
     	  { ofClass HashTable, " or ", ofClass Module, ", the ", 
	       TEX "Ext^i"," group(s) between holonomic ", EM "M", 
	       " and the polynomial ring" }
     	  },
     "The ", TT "Ext", " groups between a D-module ", EM "M", 
     " and the polynomial ring are the derived functors of ", TT "Hom", 
     ", and are finite-dimensional vector spaces over the ground field when ",
     EM "M", " is holonomic.",
     PARA {
	  "The algorithm used appears in the paper 'Polynomial and
	  rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
	  The method is to combine isomorphisms of Bjork and Kashiwara with
	  the restriction algorithm."},
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     M = W^1/ideal(x^2*D^2)
	     polynomialExt(M)
	     ///,
     Caveat =>{"Does not yet compute explicit representations of
	  Ext groups such as Yoneda representation."},
     SeeAlso => {"PolySols", "RatExt", "DExt", "Dintegration"}
     }

document {
     Key => [RatExt,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}

document {
     Key => {RatExt, (RatExt,Module), (RatExt,ZZ,Ideal,RingElement), (RatExt,ZZ,Ideal), 
	  (RatExt,Ideal,RingElement), (RatExt,Ideal),(RatExt,ZZ,Module,RingElement), 
	  (RatExt,ZZ,Module), (RatExt,Module,RingElement)},
     Headline => "Ext(holonomic D-module, polynomial ring localized at the singular locus)",
     Usage => "RatExt M, RatExt I; RatExt(M,f), RatExt(I,f);
               RatExt(i,M), RatExt(i,I); RatExt(i,M,f), RatExt(i,I,f)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "f" => RingElement => "a polynomial",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
     	  { ofClass HashTable, " or ", ofClass Module, ", the ", 
	       TEX "Ext^i"," group(s) between holonomic ", EM "M", 
	       " and the polynomial ring localized at the singular locus of ", EM "M", 
	       " (or at ", EM "f", " if specified)" } 
     	  },
     "The Ext groups between M and N
     are the derived functors of Hom, and are finite-dimensional
     vector spaces over the ground field when M and N are holonomic.",
     PARA {
	  "The algorithm used appears in the paper 'Polynomial and
	  rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
	  The method is to combine isomorphisms of Bjork and Kashiwara with
	  the restriction algorithm."},
     EXAMPLE lines ///
	     W = QQ[x, D, WeylAlgebra=>{x=>D}]
	     M = W^1/ideal(x*D+5)
	     RatExt M
	     ///,
     Caveat =>{"Input modules M or D/I should be holonomic."},
     SeeAlso => {"Dresolution", "Dintegration"}
     }

doc ///
  Key
    PolySols
    (PolySols,Module)
    (PolySols,Ideal,List)
    (PolySols,Module,List)
    (PolySols,Ideal)
  Headline
    polynomial solutions of a holonomic system
  Usage
    PolySols I
    PolySols M
    PolySols(I,w)
    PolySols(M,w)
  Inputs
    M:Module
      over the Weyl algebra $D$
    I:Ideal
      holonomic ideal in the Weyl algebra $D$
    w:List
      a weight vector
  Outputs
    :List
      a basis of the polynomial solutions of $I$
      (or of $D$-homomorphisms between $M$ and the polynomial ring)
      using $w$ for Groebner deformations. 
      If no $w$ is given, then it is taken to be the all ones vector.
  Description
    Text
      The polynomial solutions of a holonomic system form a
      finite-dimensional vector space.
      There are two algorithms implemented to get these solutions.
      The first algorithm is based on Gr\"obner deformations and
      works for ideals $I$ of PDE's - see the paper {\em Polynomial
      and rational solutions of a holonomic system} by
      Oaku, Takayama and Tsai (2000).  The second algorithm is based
      on homological algebra - see the paper {\em Computing
      homomorphisms between holonomic D-modules} by Tsai and Walther (2000).
    Example
      makeWA(QQ[x])
      I = ideal(dx^2, (x-1)*dx-1)
      PolySols I
  SeeAlso
    RatSols
    Dintegration
///

document { Key => Alg }
document {
     Key => [PolySols, Alg],
     Headline => "algorithm for finding polynomial solutions",
     UL {
	  {BOLD "GD", " -- uses Groebner deformations"},
	  {BOLD "Duality", " -- uses homological duality"}
	  }
     }
document {
     Key => GD,
     Headline => "an option for PolySols=>Alg",
     SeeAlso => "PolySols"
     }
document {
     Key => Duality,
     Headline => "an option for PolySols=>Alg",
     SeeAlso => "PolySols"
     }

doc ///
  Key
    RatSols
    (RatSols,Ideal,List,List)
    (RatSols,Ideal,RingElement,List)
    (RatSols,Ideal,List)
    (RatSols,Ideal,RingElement)
    (RatSols,Ideal)
  Headline
    rational solutions of a holonomic system
  Usage
    RatSols I
    RatSols(I,f)
    RatSols(I,f,w)
    RatSols(I,ff)
    RatSols(I,ff,w)
  Inputs
    I:Ideal
      holonomic ideal in the Weyl algebra @EM "D"@
    f:RingElement
      a polynomial
    ff:List
      a list of polynomials
    w:List
      a weight vector
  Outputs
    :List
      a basis of the rational solutions of @EM "I"@ with poles along @EM "f"@
      or along the polynomials in @TT "ff"@ using @EM "w"@
      for Groebner deformations
  Description
    Text
      The rational solutions of a holonomic system form a
      finite-dimensional vector space.
      The only possibilities for the poles of a rational solution
      are the codimension one components of the singular locus.
      An algorithm to compute rational solutions
      is based on Gr\"obner deformations and
      works for ideals $I$ of PDE's - see the paper {\em Polynomial
      and rational solutions of a holonomic system} by
      Oaku, Takayama and Tsai (2000).
    Example
      makeWA(QQ[x])
      I = ideal((x+1)*dx+5)
      RatSols I
  Caveat
    The most efficient method to find rational solutions of a system of differential
    equations is
    to find the singular locus, then try to find its irreducible
    factors.  With these, call RatSols(I, ff, w), where w
    should be generic enough so that the PolySols routine
    will not complain of a non-generic weight vector.
  SeeAlso
    PolySols
    RatExt
    DHom
///
