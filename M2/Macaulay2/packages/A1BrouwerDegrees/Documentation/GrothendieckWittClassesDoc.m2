document{
    Key => {GrothendieckWittClass, (net, GrothendieckWittClass), (texMath, GrothendieckWittClass)},
    Headline => "a new type, intended to capture the isomorphism class of an element of the Grothendieck-Witt ring of a base field",
    PARA {"A ", TT "GrothendieckWittClass" ," object is a type of ", TO2(HashTable, "HashTable"), " encoding the isomorphism class of a non-degenerate symmetric bilinear form ", TEX///$V \times V \to k$///, " over a field ", TEX///$k$///, "."},
    PARA{"Given any basis ", TEX///$e_1,\ldots,e_n$///, " for ", TEX///$V$///, " as a ", TEX///$k$///, "-vector space, we can encode the symmetric bilinear form ", TEX///$\beta$///, " by how it acts on basis elements. That is, we can produce a matrix ", TEX///$\left(\beta(e_i,e_j)\right)_{i,j}$///, ". This is called a ", EM "Gram matrix", " for the symmetric bilinear form. A change of basis produces a congruent Gram matrix, so thus a matrix represents a symmetric bilinear form uniquely up to matrix congruence."},
	
    PARA{"A GrothendieckWittClass object can be built from a symmetric ", TO2(matrix, "matrix"), " over a field using the ", TO2(makeGWClass,"makeGWClass"), " method."},
    EXAMPLE lines///
    beta = makeGWClass matrix(QQ, {{0,1},{1,0}})
    class beta
    ///,
    PARA{"The underlying matrix representative of a form can be recovered via the ", TO2(getMatrix, "getMatrix"), " command or the ", TT "matrix", " command, and its underlying field can be recovered using ", TO2(getBaseField,"getBaseField"), "."},
    EXAMPLE lines///
    getMatrix beta
    getBaseField beta
    ///,
    PARA{"For computational purposes, it is often desirable to diagonalize a Gram matrix. Any symmetric bilinear form admits a diagonal Gram matrix representative, and this is implemented via the ", TO2(getDiagonalClass, "getDiagonalClass"), " method."},
    EXAMPLE lines///
    getDiagonalClass beta
    ///,
    PARA{"Once a form has been diagonalized, it is recorded in the cache for ", TT "GrothendieckWittClass", " and can therefore be quickly recovered."},
    EXAMPLE lines///
    beta.cache.getDiagonalClass
    ///,
    PARA{"We additionally have the following methods which can be applied to Grothendieck-Witt classes:"},
    UL{
	{TO2(getRank,"getRank"),": returns the rank of a form,"},
	{TO2(getSignature,"getSignature"),": returns the signature of a form over the real numbers or rational numbers,"},
	{TO2(getIntegralDiscriminant,"getIntegralDiscriminant"),": returns an integral representative for the discriminant of a form over the rational numbers,"},
	{TO2(getHasseWittInvariant,"getHasseWittInvariant"),": returns the Hasse-Witt invariant for a form over the rational numbers at a particular prime,"},
	{TO2(getAnisotropicDimension,"getAnisotropicDimension"),": returns the anisotropic dimension of a form,"},
	{TO2(getAnisotropicPart,"getAnisotropicPart"),": returns the anisotropic part of a form,"},
	{TO2(getSumDecomposition,"getSumDecomposition"),": returns a simplified diagonal representative of a form,"},
	{TO2(getSumDecompositionString,"getSumDecompositionString"),": returns a string to quickly read a form,"},
	},
    PARA{"and Boolean methods for Grothendieck-Witt classes:"},
    UL{
	{TO2(isIsotropic,"isIsotropic"),": returns whether the form is isotropic,"},
	{TO2(isAnisotropic,"isAnisotropic"),": returns whether the form is anisotropic."},
	},
    PARA{"Forms can be created via the following methods:"},
    UL{
	{TO2(makeDiagonalForm,"makeDiagonalForm"),": creates a diagonal form over a field out of an element or list of field elements,"},
	{TO2(makeHyperbolicForm,"makeHyperbolicForm"),": creates a hyperbolic form over a field,"},
	{TO2(makePfisterForm,"makePfisterForm"),": creates a Pfister form over a field out of an element or list of field elements."},
	},
    SeeAlso => {"makeGWClass", "getBaseField", "getMatrix", "getDiagonalClass"},
    }

document {
    Key => {makeGWClass, (makeGWClass, Matrix), (isWellDefinedGW, Matrix)},
	Headline => "the Grothendieck-Witt class of a symmetric matrix",
	Usage => "makeGWClass M",
	Inputs => {
	    Matrix => "M" => {"a non-singular symmetric matrix defined over an arbitrary field of characteristic not 2"}
	    },
	Outputs => {
	    GrothendieckWittClass => {"the isomorphism class of the non-degenerate symmetric bilinear form represented by ", TEX/// $M$///}
	    },
	PARA {"Given a symmetric matrix, ", TEX///$M$///, ", this command outputs an object of type ", TT "GrothendieckWittClass", ". ",
                "This output has the representing matrix, ", TEX///$M$///, ", and the base field of the matrix stored in its ",TO2(CacheTable,"CacheTable.")},
	EXAMPLE lines ///
		 M := matrix(QQ, {{0,0,1},{0,1,0},{1,0,0}});
		 beta = makeGWClass M
	 	 ///,
		
	SeeAlso => {"GrothendieckWittClass", "getMatrix", "getBaseField"}
        }

document {
    Key => {getMatrix, (getMatrix, GrothendieckWittClass)},
	Headline => "the underlying matrix of a Grothendieck-Witt class",
	Usage => "getMatrix beta",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"the isomorphism class of a non-degenerate symmetric bilinear form over a field of characteristic not 2"}
	    },
	Outputs => {
	    Ring => {"the underlying matrix of the Grothendieck-Witt class ", TT "beta"}
	    },
	PARA {"Given the isomorphism class of a symmetric bilinear form, ", TT "beta", ", this command outputs the underlying matrix of the form."},
	EXAMPLE lines ///
		 beta = makeGWClass matrix(QQ, {{0,2},{2,0}});
		 getMatrix beta
	 	 ///,
    SeeAlso => {"GrothendieckWittClass", "makeGWClass"}
        }

document {
    Key => {getBaseField, (getBaseField, GrothendieckWittClass)},
	Headline => "the base field of a Grothendieck-Witt class",
	Usage => "getBaseField beta",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"the isomorphism class of a non-degenerate symmetric bilinear form over a field of characteristic not 2"}
	    },
	Outputs => {
	    Ring => {"the base field of the Grothendieck-Witt class ", TT "beta"}
	    },
	PARA {"Given the isomorphism class of a symmetric bilinear form, ", TT "beta", ", this command outputs the base field of the form."},
	EXAMPLE lines ///
		 beta = makeGWClass matrix(QQ, {{0,2},{2,0}});
		 getBaseField beta
	 	 ///,
    SeeAlso => {"GrothendieckWittClass"}
        }


document {
    Key => {addGW, (addGW, GrothendieckWittClass, GrothendieckWittClass)},
    Headline => "the direct sum of two Grothendieck-Witt classes",     
    Usage => "addGW(beta, gamma)",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix ", TT "M"},
	    GrothendieckWittClass => "gamma" => {"the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix ", TT "N"},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the isomorphism class of the direct sum of the bilinear forms represented by the matrices ", TT "M", " and ", TT "N"},
	    },
	PARA {"This computes the direct sum of the Grothendieck-Witt classes ",TT "beta"," and ",TT "gamma","."},
	EXAMPLE lines ///
		 M = matrix(QQ, {{1,0},{0,1}});
		 N = matrix(QQ, {{1,2},{2,5}});
		 beta = makeGWClass M;
		 gamma = makeGWClass N;
    	    	 addGW(beta, gamma)
	 	 ///,
    SeeAlso => {"GrothendieckWittClass", "makeGWClass", "multiplyGW"}
}

document {
    Key => {multiplyGW, (multiplyGW, GrothendieckWittClass, GrothendieckWittClass)},
    Headline => "the tensor product of two Grothendieck-Witt classes",     
	Usage => "multiplyGW(beta, gamma)",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix ", TT "M"},
	    GrothendieckWittClass => "gamma" => {"the isomorphism class of a non-degenerate symmetric bilinear form represented by a matrix ", TT "N"},
	    }, 
	Outputs => { 
	    GrothendieckWittClass => {"the isomorphism class of the tensor product ",TEX///$M\otimes N$///," of the bilinear forms represented by the matrices ", TT "M", " and ", TT "N"},
	    },
	PARA {"This computes the tensor product ",TEX///$\beta\otimes\gamma$///," of the Grothendieck-Witt classes ",TT "beta"," and ",TT "gamma","."},
	EXAMPLE lines ///
    	    	 M = matrix(QQ, {{1,0},{0,1}});
		 N = matrix(QQ, {{1,2},{2,5}});
		 beta = makeGWClass M;
		 gamma = makeGWClass N;
    	    	 multiplyGW(beta, gamma)
	 	 ///,
    SeeAlso => {"GrothendieckWittClass", "makeGWClass", "addGW"}
}

