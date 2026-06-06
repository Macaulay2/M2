document {
    Key => {
	 selectInSubring,
	(selectInSubring, ZZ, Matrix)
    },
    Headline => "select columns in a subring",
    Usage => "selectInSubring(i,m)",
    Inputs => {
	"i" => ZZ,
	"m" => Matrix
    },
    Outputs => {
	Matrix => {"with the same target and ring as ", TT "m", ", consisting of those columns
	    of ", TT "m", " which lie in the subring where the first 
	    ", TT "i", " blocks of the monomial order are zero"}
	},
    "For example, consider the following block (or product) order.",
    EXAMPLE lines ///
        R = QQ[x,y,a..d,t,MonomialOrder=>{2,4,1}];
	m = matrix{{x*a-d^2, a^3-1, x-a^100, a*b*d+t*c^3, t^3-t^2-t+1}}
	selectInSubring(1,m)
        selectInSubring(2,m)
    ///,
    PARA{},
    "The lexicographic order is considered as one block, as in the following example.",
    EXAMPLE lines ///
        S = QQ[a..d,MonomialOrder=>Lex];
	m = matrix{{a^2-b, b^2-c, c^2-d, d^2-1}}
        selectInSubring(1,m)
    ///,
    PARA{},
    "If you wish to be able to pick out the elements not involving a, or a and b, etc,
    then create a block monomial order.",
    EXAMPLE lines ///
        S = QQ[a..d,MonomialOrder=>{4:1}];
	m = matrix{{a^2-b, b^2-c, c^2-d, d^2-1}}
	selectInSubring(1,m)
	selectInSubring(2,m)
	selectInSubring(3,m)	  
    ///,
    Caveat => {
	"This routine doesn't do what one would expect for graded orders
	such as ", TT "GLex", ".  There, the first part of the monomial 
	order is the degree, which is usually not zero."
    },
    SeeAlso => {
	"monomial orderings",
	leadTerm,
	"Elimination::eliminate",
    },
}
