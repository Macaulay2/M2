document {
    Key => {
	 modulo,
	(modulo, Matrix, Matrix),
	(modulo, Matrix, Nothing),
	(modulo, Nothing, Matrix)
    },
    Headline => "find the pre-image (pullback) of image of a map (low level version)",
    Usage => "modulo(f,g)",
    Inputs => { "f", "g" },
    Outputs => {
	Matrix => { " whose image is the pre-image (pullback) of the image of ", TT "g", " under ", TT "f" }
    },
    PARA {
	"The maps ", TT "f", " and ", TT "g", " must have the same target, and their sources and targets must be free.
	If ", TT "f", " is ", TO "null", ", then it is taken to be the identity.  If ", TT "g", " is ", TO "null", ", it is taken to be zero."
    },
    PARA {"This function is mainly for internal use."},
    EXAMPLE lines ///
	R = QQ[x,y,z]
	f = matrix {{x,y}}
	g = matrix {{y,z}}
	modulo(f,g)
	kernel( inducedMap(coker g, target g) * f )
    ///,
}
