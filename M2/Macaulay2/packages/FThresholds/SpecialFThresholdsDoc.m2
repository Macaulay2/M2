doc ///
     Key
     	binomialFPT
     Headline
        computes the F-pure threshold of a binomial polynomial
     Usage
     	 binomialFPT(f)
     Inputs 
		f:RingElement
     Outputs
         :QQ
     Description
	Text
	    Returns the F-pure threshold of a binomial in a polynomial ring.  This is based on the work of Daniel Hernandez.
///

doc ///
     Key
     	diagonalFPT
     Headline
        computes the F-pure threshold of a diagonal polynomial
     Usage
     	 diagonalFPT(f)
     Inputs 
		f:RingElement
     Outputs
         :QQ
     Description
	Text
	    Returns the F-pure threshold of a diagonal hypersurface in a polynomial ring.  This is based on the work of Daniel Hernandez.
///


doc ///
     Key
     	binaryFormFPT
         (binaryFormFPT,RingElement)
         (binaryFormFPT,List,List)
     Headline
         computes the F-pure threshold of a form in two variables
     Usage
     	  binaryFormFPT(G), binaryFormFPT(factors,multiplicities)
     Inputs 
	factors:List
	    which contains the linear factors of a form G in two variables 
	multiplicities:List
	    which contains the multiplicities of those linear factors in G
	G:RingElement
	    a form in two variables
     Outputs
        :QQ
     Description
	Text
	    binaryFormFPT computes the F-pure threshold of a homogeneous polynomial G
	    	in two variables. 
	Example
	    R = ZZ/3[x,y];
	    G = x^5*y^2-x^3*y^4+x*y^6-y^7;
	Text    
	    The method used requires factoring G into linear forms in some extension of the base field. If the user knows such a factorization beforehand, the alternate call binaryFormFPT(factors,multiplicities) can be used for improved performance.
	Example
	    R = ZZ/5[x,y];
	Text    
	    This is based on the work of Daniel Hernandez and Pedro Teixeira.
///


doc ///
     Key
     	isBinomial 
     Headline
        Checks whether a polynomial is binomial.
     Usage
     	 isBinomial(f)
     Inputs 
		f:RingElement
     Outputs
         :Boolean
     Description
	Text
	    Returns true if f is a binomial, otherwise returns false.
///

doc ///
     Key
     	isDiagonal 
     Headline
        Checks whether a polynomial is diagonal.
     Usage
     	 isDiagonal(f)
     Inputs 
		f:RingElement
     Outputs
         :Boolean
     Description
	Text
	    Returns true if f is a diagonal, otherwise returns false.  Recall f is called diagonal if it is of the form x_1^(a_1)+...+x_n^(a_n) up to renumbering of the variables.
///

