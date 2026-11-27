--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented {
	  (coefficient, RingElement, RingElement),
	  (coefficient, MonoidElement, RingElement)
	  }

doc ///
  Key
    coefficient
  Headline
    coefficient of a monomial
  Usage
    coefficient(m,f)
  Inputs
    m:RingElement
      a monomial
    f:RingElement
      in the same ring @TT "R"@ as @TT "m"@
  Outputs
    :RingElement
      the coefficient of the monomial @TT "m"@ in @TT "f"@ as an element of the
      coefficient ring of @TT "R"@
  Description
    Example
      R = GF(25,Variable=>a)[x,y,z];
      f = ((a+1)*x+a*y+a^2*z)^2
      coefficient(y^2,f)
    Text
      The returned value is an element of the coefficient ring, even in the
      case when that ring is another polynomial ring.
    Example
      S = R[r,s,t];
      coefficient(r,a*x*(r+a*s))
  SeeAlso
    coefficients
    monomials
    coefficientRing
///
