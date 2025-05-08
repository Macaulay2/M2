doc ///
  Key
    AnnFs
    (AnnFs, RingElement)
  Headline
    differential annihilator of a polynomial in a Weyl algebra
  Usage
    AnnFs(f)
  Inputs
    f:RingElement
      polynomial in a Weyl algebra $D$
  Outputs
     :Ideal
      the differential annihilator of $f$ in $D[s]$, for a new variable $s$. 
  Description
    Text
     This routine computes the ideal of the differential annihilator of a polynomial. This ideal is a left ideal of the ring $D[s]$.  More
     details can be found in 
     [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Chapter 5].  
     The computation in the case of the element $f$ is via  Algorithm 5.3.6.
    Example
      makeWA(QQ[x,y])
      f = x^2+y
      AnnFs f
  Caveat
     Must be over a ring of characteristic $0$.
///

document {
     Key => {(AnnIFs, Ideal,RingElement), AnnIFs}, 
     Headline => "the annihilating ideal of f^s for an arbitrary D-module", 
     Usage => "AnnIFs(I,f)",
     Inputs => {
	  "I" => {
	       "that represents a holonomic D-module ", 
	       EM {"A", SUB "n", "/I"}, 
	       " (the ideal is expected to be f-saturated; one may use ", TO WeylClosure," if it is not) "
	       },
	  "f" => {"a polynomial in a Weyl algebra ", EM {"A", SUB "n"},  
	       " (should contain no differential variables)"}
	  },      
     Outputs => {
	  Ideal => {"the annihilating ideal of ", TEX "A_n[f^{-1},s] f^s", " tensored with ",
	       TEX "A_n/I", " over the ring of polynomials" }
	  },
     EXAMPLE lines ///
	  W = QQ[x,dx, WeylAlgebra=>{x=>dx}]
	  AnnIFs (ideal dx, x^2)
	  ///, 
     Caveat => {"Caveats and known problems: The ring of f should not have any 
	  parameters: it should be a pure Weyl algebra. Similarly, 
	  this ring should not be a homogeneous Weyl algebra."
     	  },
     SeeAlso => {"AnnFs", "WeylAlgebra", "WeylClosure"}
     }  

doc ///
  Key
    diffRatFun
   (diffRatFun, List, RingElement)
   (diffRatFun, List, RingElement, RingElement, ZZ)
  Headline
    derivative of a rational function in a Weyl algebra
  Usage
    diffRatFun(m,f)
    diffRatFun(m,g,f,a)
  Inputs
    f:RingElement
      polynomial in a Weyl algebra $D$ in $n$ variables or rational function in the fraction
      field of a polynomial ring in $n$ variables
    g:RingElement
      polynomial in a Weyl algebra $D$ in $n$ variables
    m:List 
      of nonnegative integers $m = \{m_1,...,m_n\}$
    a:ZZ
      an integer
  Outputs
     :RingElement
      the result of applying the product of the $(dx_i)^{m_i}$ to $f$ 
     :List
     -- (RingElement,RingElement,ZZ)
       the result of applying the product of the $(dx_i)^{m_i}$ to $g/f^a$, written as 
       (numerator,denominator,power of denominator)
  Description
    Text
     Let $D$ be a Weyl algebra in the variables $x_1,..x_n$ and partials $dx_1,..,dx_n$.  Let $f$ 
     be either a polynomial or rational function in the $x_i$ and $m = (m_1,..,m_n)$ a list 
     of nonnegative integers.  The function $f$ may be given as an element of a polynomial ring in the $x_i$
     or of the fraction field of that polynomial ring or of $D$.  This method applies the product of the 
     $dx_i^{m_i}$ to $f$. In the case of the input $(m,g,f,a)$, where $f \neq 0$ and $g$ are both 
     polynomials and $a$ is a nonnegative integer, it applies the product of the $dx_i^{m_i}$ to $g/f^a$ 
     and returns the resulting derivative as (numerator,denominator,power of denominator), not necessarily 
     in lowest terms.
    Example 
     QQ[x,y,z]
     m = {1,1,0}
     f = x^2*y+z^5
     diffRatFun(m,f)
    Example 
     makeWA(QQ[x,y,z])
     m = {1,1,0}
     f = x^2*y+z^5
     diffRatFun(m,f)
    Example
     frac(QQ[x,y])
     m = {1,2}
     f = x/y
     diffRatFun(m,f)
    Example
     makeWA(QQ[x,y,z])
     m = {1,2,1}
     g = z
     f = x*y
     a = 3
     diffRatFun(m,g,f,a)
  Caveat
    Must be over a ring of characteristic $0$.  
///

doc ///
  Key
    polynomialAnnihilator
   (polynomialAnnihilator, RingElement)
  Headline
    annihilator of a polynomial in the Weyl algebra
  Usage
    polynomialAnnihilator f
  Inputs
    f:RingElement
      polynomial
  Outputs
    :Ideal
      the annihilating (left) ideal of @{EM "f"}@ in the Weyl algebra
  Description
    Example
      makeWA(QQ[x,y])
      f = x^2-y^3
      I = polynomialAnnihilator f
  Caveat
    The input f should be an element of a Weyl algebra, and not an element of a commutative polynomial ring.
    However, f should only involve commutative variables.
  SeeAlso
    rationalFunctionAnnihilator
///

doc ///
  Key
    rationalFunctionAnnihilator
    (rationalFunctionAnnihilator, RingElement, RingElement)
    (rationalFunctionAnnihilator, RingElement)
  Headline
    annihilator of a rational function in Weyl algebra
  Usage
    rationalFunctionAnnihilator f
    rationalFunctionAnnihilator(g,f)
  Inputs
    f:RingElement
      polynomial
    g:RingElement
      polynomial
  Outputs
    :Ideal
      left ideal of the Weyl algebra
  Description
    Text
      @{TT "rationalFunctionAnnihilator f"}@ computes the annihilator ideal in the Weyl algebra 
      of th, e rational function $1/f$.
      @BR{}@
      @{TT "rationalFunctionAnnihilator(g,f)"}@ computes the annihilator ideal in the 
      Weyl algebra of the rational function $g/f$.
   Example
      makeWA(QQ[x,y])
      f = x^2-y^3
      g = 2*x*y
      I = rationalFunctionAnnihilator (g,f)
  Caveat
    The inputs f and g should be elements of a Weyl algebra, and not elements of a commutative polynomial ring.
    However, f and g should only use the commutative variables.
  SeeAlso
    polynomialAnnihilator
///
