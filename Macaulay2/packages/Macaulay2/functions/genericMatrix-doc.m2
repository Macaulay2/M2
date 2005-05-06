--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

document { 
     Key => {genericMatrix, (genericMatrix,Ring,RingElement,ZZ,ZZ), (genericMatrix,Ring,ZZ,ZZ)},
     Headline => "make a generic matrix of indeterminates",
     Usage => "genericMatrix(R,r,m,n)",
     Inputs => {
	  "R" => Ring => "",
	  "r" => RingElement => {"which is a variable in the ring ", TT "R"},
	  "m" => ZZ => "",
	  "n" => ZZ => ""
	  },
     Outputs => {
	  Matrix => {"with ", TT "m", " rows and ", TT "n", 
	       " columns whose entries are variables in the ring ", TT "R"} 
	  },
     EXAMPLE {
	  "R = ZZ[a..z];",
	  "genericMatrix(R,a,2,4)",
	  "genericMatrix(R,i,3,2)"
	  },
     PARA,
     "Omitting the ", TO2("RingElement","ring element"), " ", TT "r", 
     " is the same having ", TT "r", " be the first variable in ", TT "R", ".",
     EXAMPLE{
	  "genericMatrix(R,2,4)",
	  "genericMatrix(R,3,2)"	  
	  },
     SeeAlso => {genericSkewMatrix, genericSymmetricMatrix}
     }
end

