--- status: TODO
--- author(s): 
--- notes: 

document {
     Key => wedgeProduct,
     Headline => "the exterior multiplication map",
     TT "wedgeProduct(p,q,M)", " -- returns the matrix which represents the
     multiplication map from ", TT "exteriorPower(p,M) ** exteriorPower(q,M)", "
     to ", TT "exteriorPower(p+q,M)", ".",
     PARA{},
     "Here ", TT "M", " is free module."
     }

document { 
     Key => {(wedgeProduct,ZZ,ZZ,Module),wedgeProduct},
     Headline => "the exterior multiplication map",
     Usage => "wedgeProduct(p,q,F)",
     Inputs => {
	  "p", "q", "F" => "a free module"
	  },
     Outputs => {
	  Matrix => {"representing the multiplication map
	  from ", TT "exteriorPower(p,M) ** exteriorPower(q,M)", "
     	  to ", TT "exteriorPower(p+q,M)"}
	  },
     EXAMPLE lines ///
          F = QQ^4
          wedgeProduct(1,1,F)
	  ///,
     SeeAlso => {exteriorPower}
     }

