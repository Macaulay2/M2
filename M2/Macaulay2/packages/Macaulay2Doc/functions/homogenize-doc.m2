document {
    Key => {
	 homogenize,
	(homogenize, Ideal,  RingElement),
	--(homogenize, Ideal, RingElement, List), ??
	(homogenize, Matrix, RingElement),
	(homogenize, Matrix, RingElement, List),
	(homogenize, Module, RingElement),
	(homogenize, Module, RingElement, List),
	(homogenize, Vector, RingElement),
	(homogenize, Vector, RingElement, List),
	(homogenize, RingElement, RingElement),
	(homogenize, RingElement, RingElement, List),
    },
    Headline => "homogenize with respect to a variable",
    TT "homogenize(m,v)", " -- homogenize the ring element, vector,
    matrix, or module ", TT "m", " using the variable ", TT "v", " in the ring of ", TT "m", ".",
    BR{},     
    TT "homogenize(m,v,w)", " -- homogenize ", TT "m", " using the variable ", TT "v", ",
    so that the result is homogeneous with respect to the given list ", TT "w", " of
    integers provided as weights for the variables.",
    PARA{},
    EXAMPLE {
	"R = ZZ/101[x,y,z,Degrees => {1,2,3}]",
	"f = 1 + y + z^2",
	"homogenize(f,x)",
	"homogenize(f,x,{1,0,-1})",
    },
    PARA{},
    "The weights that may be used are limited (roughly) to the range -2^30 .. 2^30.",
    PARA{},
    Caveat => {
	"If the homogenization overflows the monomial, this is not
	reported as an error."
    }
}
