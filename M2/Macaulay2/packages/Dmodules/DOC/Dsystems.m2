document {
     Key => [gkz,Vars] }
document {
     Key => [AppellF1,Vars] }
document {
     Key => Vars }

document {
     Key => {gkz, --(gkz, Matrix), 
	  (gkz, Matrix, List)},
     Headline => "GKZ A-hypergeometric ideal",
     Usage => "gkz(A,b)",
     Inputs => {
	  "A" => Matrix,
	  "b" => List 
	  },
     Outputs => {
     	  Ideal => "which represents the Gel'fand-Kapranov-Zelevinsky hypergeometric system
     associated to the matrix A and the parameter vector b"
	  },
     "The GKZ hypergeometric system of PDE's associated to a (d x n)
     integer matrix A consists of the toric ideal I_A in the polynomial
     subring C[d_1,...,d_n] and Euler relations given by the entries
     of the vector (A theta - b), where theta is the vector
     (theta_1,...,theta_n)^t, and theta_i = x_i d_i.
     See the book 'Groebner deformations of hypergeometric differential 
     equations' by Saito-Sturmfels-Takayama (1999) for more details.",
     EXAMPLE lines ///
	A = matrix{{1,1,1},{0,1,2}}
     	b = {3,4}
     	I = gkz (A,b)
	///,
     Caveat =>{"gkz always returns a different ring and will use variables
	  x_1,...,x_n, D_1,...D_n."},
     SeeAlso => {"AppellF1"}
     },

document {
     Key => {(AppellF1, List), AppellF1},
     Headline => "Appell F1 system of PDE's",
     Usage => "AppellF1 {a0,a1,a2,a3}",
     Inputs => {
	  "{a0,a1,a2,a3}"
	  },
     Outputs => {
	  Ideal => "which represents Appell F1 system of PDE's associated to the
     	  parameters a0, a1, a2, and a3."
	  },
     EXAMPLE lines ///
	w = {1,4/5,-2,3/2}
     	I = AppellF1 w
	///,
     Caveat =>{"AppellF1 always returns a different ring and will
	  use variables x and y. Input should be a List of 4
	  numbers."},
     SeeAlso => {"gkz"}
     }

document {
     Key => {(PolyAnn, RingElement), PolyAnn},
     Headline => "annihilator of a polynomial in Weyl algebra",
     Usage => "PolyAnn f",
     Inputs => {
     	  "f" => RingElement => "polynomial"
	  },
     Outputs => {
     	  Ideal => {"the annihilating (left) ideal of ", EM "f", "in the Weyl algebra"}
	  },
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
     	f = x^2-y^3
     	I = PolyAnn f
	///,
     Caveat =>{"The input f should be an element of a Weyl algebra,
	  and not an element of a commutative polynomial ring.
	  However, f should only involve commutative variables."},
     SeeAlso => {"RatAnn"}
     }

document {
     Key => {RatAnn, (RatAnn, RingElement, RingElement), (RatAnn, RingElement)},
     Headline => "annihilator of a rational function in Weyl algebra",
     Usage => "RatAnn f, RatAnn(g,f)",
     Inputs => {
	  "f" => RingElement => "polynomial",
	  "g" => RingElement => "polynomial"
	  },
     Outputs => {
     	  Ideal => "left ideal of the Weyl algebra"
	  },
     TT "RatAnn f", " computes the annihilator ideal in the Weyl algebra of the rational
     function 1/f",
     BR{},
     TT "RatAnn(g,f)", " computes the annihilator ideal in the Weyl algebra of the rational
     function g/f",
     EXAMPLE lines ///
	W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]
     	f = x^2-y^3
     	g = 2*x*y
     	I = RatAnn (g,f)
	///,
     Caveat =>{"The inputs f and g should be elements of a Weyl algebra,
	  and not elements of a commutative polynomial ring.
	  However, f and g should only use the commutative variables."},
     SeeAlso => {"PolyAnn"}
     }
