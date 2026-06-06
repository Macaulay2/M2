document {
    Key => {
	 eagonNorthcott,
	(eagonNorthcott, Matrix)
    },
    Headline => "Eagon-Northcott complex of a matrix of linear forms",
    Usage => "eagonNorthcott f",
    Inputs => { "f" => "a matrix of linear forms" },
    Outputs => { "C" => {"the Eagon-Northcott complex of ", TT "f"} },
    "The Eagon-Northcott complex is an explicit chain complex that gives a minimal projective
    resolution of the cokernel of the matrix maximal minors of a generic matrix of linear forms.",
    EXAMPLE lines ///
          needsPackage "OldChainComplexes"
     	  R = QQ[a..z]
	  f = genericMatrix(R,3,5)
	  M = coker gens minors_3 f
	  C = res M
	  D = eagonNorthcott f
	  H = prune HH D
	  assert( H_0 == M and H_1 == 0 and H_2 == 0 and H_3 == 0 )
    ///,
    "This function was written by Greg Smith."
    }
