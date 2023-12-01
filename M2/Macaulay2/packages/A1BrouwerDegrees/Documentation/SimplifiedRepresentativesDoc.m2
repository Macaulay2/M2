document {
    Key => {diagonalClass, (diagonalClass, GrothendieckWittClass)},
	Headline => "produces a diagonalized form for any Grothendieck-Witt class, with simplified terms on the diagonal",
	Usage => "diagonalClass(beta)",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"over a field ", TEX///$k$///,", where ", TEX///$k$///, " is the rationals, reals, complex numbers, or a finite field of characteristic not 2"}
	    },
	Outputs => {
	    GrothendieckWittClass => {"a form isomorphic to ", TEX///$\beta$///, " with a diagonal Gram matrix"}
	    },
	PARA {"Given a symmetric bilinear form, this method calls the ", TO2(congruenceDiagonalize,"congruenceDiagonalize"), " command in order to produce a diagonal symmetric bilinear form isomorphic to ", TEX///$\beta$///, ", with reduced square classes appearing as the diagonal entries."},
	EXAMPLE lines ///
	M = matrix(QQ,{{9,1,7,4},{1,10,3,2},{7,3,6,7},{4,2,7,5}});
	beta = gwClass(M);
	diagonalClass(beta)
	///,
	PARA{"Note that the  ", TO2(GrothendieckWittClass, "GrothendieckWittClass"), " type caches diagonal versions of a form once they've been computed. We can recover this quickly in the following way."},
	EXAMPLE lines///
	beta.cache.diagonalClass
	///,
	SeeAlso => {"congruenceDiagonalize"}
	}    


document {
    Key => {diagonalEntries, (diagonalEntries, GrothendieckWittClass)},
	Headline => "extracts a list of diagonal entries for a GrothendieckWittClass",
	Usage => "diagonalEntries(beta)",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"over a field ", TEX///$k$///,", where ", TEX///$k$///, " is the rationals, reals, complex numbers, or a finite field of characteristic not 2"}
	    },
	Outputs => {
	    List => "L" => {" of elements ", TEX///$a_i\in k$///,", where ", TEX///$i = 1,\dots,n $///,", such that ", TEX///$\beta \cong \langle a_1,\ldots,a_n\rangle$///}
	    },
	PARA {"Given a diagonal form, ", TT "diagonalEntries", " will extract the elements along the diagonal."},
	EXAMPLE lines ///
	beta = gwClass(matrix(QQ,{{3,0,0},{0,2,0},{0,0,7}}))
	diagonalEntries beta
        ///,
	PARA{"If the form is not given with a diagonal representative, this method will first diagonalize it."},
	EXAMPLE lines///
	gamma = gwClass(matrix(RR,{{0,0,1},{0,1,0},{1,0,0}}))
	diagonalEntries gamma
	///,
	SeeAlso => {"diagonalClass", "congruenceDiagonalize"}
	}

