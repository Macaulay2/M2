document {
    Key => {getDiagonalClass, (getDiagonalClass, GrothendieckWittClass)},
	Headline => "produces a diagonalized form for any Grothendieck-Witt class, with simplified terms on the diagonal",
	Usage => "getDiagonalClass beta",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"over ", TEX///$\mathbb{Q}$///,", ",TEX///$ \mathbb{R}$///,", ",TEX///$\mathbb{C}$///, ", or a finite field of characteristic not 2"}
	    },
	Outputs => {
	    GrothendieckWittClass => {"a form isomorphic to ", TEX///$\beta$///, " with a diagonal Gram matrix"}
	    },
	PARA {"Given a symmetric bilinear form, this method uses the ", TO2(diagonalizeViaCongruence,"diagonalizeViaCongruence"), " command in order to produce a diagonal symmetric bilinear form isomorphic to ", TEX///$\beta$///, ", with reduced square classes appearing as the diagonal entries."},
	EXAMPLE lines ///
	M = matrix(QQ, {{9,1,7,4},{1,10,3,2},{7,3,6,7},{4,2,7,5}});
	beta = makeGWClass M;
	getDiagonalClass beta
	///,
	PARA{"Note that the  ", TO2(GrothendieckWittClass, "GrothendieckWittClass"), " type caches diagonal versions of a form once they've been computed. We can recover this quickly in the following way."},
	EXAMPLE lines///
	beta.cache.getDiagonalClass
	///,
	SeeAlso => {"diagonalizeViaCongruence", "getDiagonalEntries"}
	}    


document {
    Key => {getDiagonalEntries, (getDiagonalEntries, GrothendieckWittClass)},
	Headline => "extracts a list of diagonal entries for a GrothendieckWittClass",
	Usage => "getDiagonalEntries beta",
	Inputs => {
	    GrothendieckWittClass => "beta" => {"over a field of characteristic not 2"}
	    },
	Outputs => {
	    List => "L" => {" of elements ", TEX///$a_i\in k$///,", where ", TEX///$i = 1,\dots,n $///,", such that ", TEX///$\beta \cong \langle a_1,\ldots,a_n\rangle$///}
	    },
	PARA {"Given a diagonal form, ", TT "getDiagonalEntries", " will extract the elements along the diagonal."},
	EXAMPLE lines ///
	beta = makeGWClass matrix(QQ, {{3,0,0},{0,2,0},{0,0,7}})
	getDiagonalEntries beta
        ///,
	PARA{"If the form is not given with a diagonal representative, this method will first diagonalize it."},
	EXAMPLE lines///
	gamma = makeGWClass matrix(RR, {{0,0,1},{0,1,0},{1,0,0}})
	getDiagonalEntries gamma
	///,
	SeeAlso => {"getDiagonalClass", "diagonalizeViaCongruence"}
	}

