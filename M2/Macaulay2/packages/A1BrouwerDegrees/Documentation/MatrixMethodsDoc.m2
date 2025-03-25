document {
	Key => {diagonalizeViaCongruence, (diagonalizeViaCongruence, Matrix)},
	Headline => "diagonalizes a symmetric matrix via congruence",
	Usage => "diagonalizeViaCongruence M",
	Inputs => {
	    Matrix => "M" => {"a symmetric matrix over any field"}
	    },
	Outputs => {
	    Matrix => {"a diagonal matrix congruent to ", TT "M"}
	    },
	PARA {"Given a symmetric matrix ", TEX///$M$///, " over any field, this command gives a diagonal matrix congruent to ", TEX///$M$///,". Note that the order in which the diagonal terms appear is not specified."},
	EXAMPLE lines ///
		 M = matrix(GF(17), {{7,9},{9,6}});
		 diagonalizeViaCongruence M
	 	 ///,
	SeeAlso => {"getDiagonalClass", "getDiagonalEntries"}
     	}
