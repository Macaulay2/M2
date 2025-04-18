document {
    Key => {
	divideByVariable,
	(divideByVariable, Matrix, RingElement),
	(divideByVariable, Matrix, RingElement, ZZ)
    },
    Headline => "divide all columns by a (power of a) variable",
    TT "divideByVariable(m,v)", " -- divide each column of the matrix 'm' by 
    as high a power of the variable 'v' as possible.",
    BR{},
    TT "divideByVariable(m,v,d)", " -- divide each column of the matrix 'm' by 
    as high a power of the variable 'v' as possible, but divide by no more than v^d.",
    PARA{},
    EXAMPLE {
	"R = ZZ/101[a..d]",
	"m = matrix{{a*b, a^2*c}, {a*b^2, a^4*d}}",
	"divideByVariable(m,a)",
	"divideByVariable(m,a,1)",
    },
    Caveat => "You can only divide by a variable, not a monomial,
    and you have little control on what power will be divided.  This routine is mostly
    used by the saturation commands as a fast internal way of dividing.",
    PARA{},
    "We may eliminate this routine.",
}
