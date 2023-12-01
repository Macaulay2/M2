document{
    Key => GrothendieckWittClass,
    Headline => "a new type, intended to capture the isomorphism class of an element of the Grothendieck-Witt ring of a base field",
    PARA {"A ", TT "GrothendieckWittClass" ," object is a type of ", TO2(HashTable, "HashTable"), " encoding the isomorphism class of a non-degenerate symmetric bilinear form ", TEX///$V \times V \to k$///, " over a field ", TEX///$k$///, "."},
    PARA{"Given any basis ", TEX///$e_1,\ldots,e_n$///, " for ", TEX///$V$///, " as a ", TEX///$k$///, "-vector space, we can encode the symmetric bilinear form ", TEX///$\beta$///, " by how it acts on basis elements. That is, we can produce a matrix ", TEX///$\left(\beta(e_i,e_j)\right)_{i,j}$///, ". This is called a ", EM "Gram matrix", " for the symmetric bilinear form. A change of basis will produce a congruent Gram matrix, thus a matrix represents a symmetric bilinear form uniquely up to matrix congruence."},
	
	
    PARA{"A GrothendieckWittClass object can be built from a symmetric ", TO2(matrix, "matrix"), " over a field using the ", TO2(gwClass,"gwClass"), " method."},
    EXAMPLE lines///
    beta = gwClass(matrix(QQ,{{0,1},{1,0}}))
    class beta
    ///,
    PARA{"The underlying matrix representative of a form can be recovered via the ", TT "matrix", " command, and its underlying field can be recovered using ", TO2(baseField,"baseField"), "."},
    EXAMPLE lines///
    beta.matrix
    baseField(beta)
    ///,
    PARA{"For computational purposes, it is often desirable to diagonalize a Gram matrix. Any symmetric bilinear form admits a diagonal Gram matrix representative by ", EM "Sylvester's law of inertia", ", and this is implemented via the ", TO2(diagonalClass, "diagonalClass"), " method."},
    EXAMPLE lines///
    diagonalClass(beta)
    ///,
    PARA{"Once a form has been diagonalized, it is recorded in the cache for ", TT "GrothendieckWittClass", " and can therefore be quickly recovered."},
    EXAMPLE lines///
    beta.cache.diagonalClass
    ///,
    SeeAlso => {"gwClass","diagonalClass","baseField"},
    }

document {
    Key => {gwClass, (gwClass, Matrix), (matrix, GrothendieckWittClass), (isWellDefined, Matrix)},
	Headline => "the Grothendieck Witt class of a symmetric matrix",
	Usage => "gwClass(M)",
	Inputs => {
	    Matrix => "M" => {"a symmetric matrix defined over an arbitrary field"}
	    },
	Outputs => {
	    GrothendieckWittClass => { "the isomorphism class of a symmetric bilinear form represented by ", TEX/// $M$///}
	    },
	PARA {"Given a symmetric matrix, ", TEX///$M$///, ", this command outputs an object of type ", TT "GrothendieckWittClass", ". ",
                "This output has the representing matrix, ", TEX///$M$///, ", and the base field of the matrix stored in its CacheTable."},
	EXAMPLE lines ///
		 M := matrix(QQ,{{0,0,1},{0,1,0},{1,0,0}});
		 beta = gwClass(M)
	 	 ///,
	PARA{"The matrix representing a ", TT "GrothendieckWittClass", " element can be recovered using the ", TT "matrix", " command:"},
	EXAMPLE lines ///
	    	beta.matrix
		///,
        PARA{"The base field which the form ", TEX///$\beta$///, " is implicitly defined over can be recovered with the ", TO2(baseField,"baseField"), " method."},
	EXAMPLE lines ///
	    	baseField beta
		///,
		
	SeeAlso => {"baseField","GrothendieckWittClass"}
        }

document {
    Key => {baseField, (baseField, GrothendieckWittClass)},
	Headline => "the base field of a Grothendieck Witt class",
	Usage => "baseField(beta)",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"the isomorphism class of a symmetric bilinear form"}
	    },
	Outputs => {
	    Ring => { "the base field of the Grothendieck-Witt class ", TT "beta" }
	    },
	PARA {"Given the isomorphism class of a symmetric bilinear form, ", TT "beta", 
                ", this command outputs the base field of the form."},
	EXAMPLE lines ///
		 beta = gwClass(matrix(QQ,{{0,2},{2,0}}));
		 baseField beta
	 	 ///,
    SeeAlso => {"GrothendieckWittClass"}
        }


document {
    Key => {gwAdd, (gwAdd, GrothendieckWittClass, GrothendieckWittClass)},
    Headline => "the direct sum of two Grothendieck-Witt classes",     
    Usage => "gwAdd(beta, gamma)",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix ", TT "M"},
	    GrothendieckWittClass => "gamma" => {"the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix ", TT "N"},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the isomorphism class of the direct sum of the bilinear forms represented by the matrices ", TT "M", " and ", TT "N"},
	    },
	PARA {"This computes the direct sum of the Grothendieck-Witt classes ",TT "beta"," and ",TT "gamma","."},
	EXAMPLE lines ///
		 M = matrix(QQ,{{1,0},{0,1}});
		 N = matrix(QQ, {{1, 2}, {2, 5}});
		 beta = gwClass(M);
		 gamma = gwClass(N);
    	    	 gwAdd(beta, gamma)
	 	 ///,
    SeeAlso => {"GrothendieckWittClass", "gwClass", "gwMultiply"}
}

document {
    Key => {gwMultiply, (gwMultiply, GrothendieckWittClass, GrothendieckWittClass)},
    Headline => "the tensor product of two Grothendieck-Witt classes",     
	Usage => "gwMultiply(beta, gamma)",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix ", TT "M"},
	    GrothendieckWittClass => "gamma" => {"the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix ", TT "N"},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the isomorphism class of the tensor product of the bilinear forms represented by the matrices ", TT "M", " and ", TT "N"},
	    },
	PARA {"This computes the tensor product of the Grothendieck-Witt classes ",TT "beta"," and ",TT "gamma","."},
	EXAMPLE lines ///
    	    	 M = matrix(QQ,{{1,0},{0,1}});
		 N = matrix(QQ, {{1, 2}, {2, 5}});
		 beta = gwClass(M);
		 gamma = gwClass(N);
    	    	 gwMultiply(beta, gamma)
	 	 ///,
    SeeAlso => {"GrothendieckWittClass", "gwClass", "gwAdd"}
}

