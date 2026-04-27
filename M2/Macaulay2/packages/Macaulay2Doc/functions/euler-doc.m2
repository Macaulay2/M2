document {
    Key => (euler, ProjectiveHilbertPolynomial),
    Headline => "constant term of the Hilbert polynomial",
    Usage => "euler P",
    Inputs => {"P"},
    Outputs => {ZZ =>" the constant term of the Hilbert polynomial"},
    "The command returns ", TT "P(0)", " the constant term of P. 
    This is also the Euler characteristic of the sheaf of rings of a projective 
    variety with Hilbert polynomial ", TT "P", ".",
    PARA{},
    EXAMPLE {	   
	"R = QQ[x_0..x_3]",
	"C = Proj(R/monomialCurveIdeal(R, {1,3,4}));",
	"P = hilbertPolynomial C",
	"euler P"
    },
    SeeAlso => {hilbertPolynomial, eulers, genus},
}
