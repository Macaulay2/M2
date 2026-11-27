--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented { (numerator,QQ) }

doc ///
  Key
    numerator
    (numerator,Divide)
  Headline
    numerator of a fraction
  Usage
    numerator x
  Inputs
    x:
      a fraction
  Outputs
    :
      the numerator of @TT "x"@
  Description
    Example
      numerator (4/6)
    Example
      R = frac(ZZ[x,y]);
      numerator((x+2*y-3)/(x-y))
    Text
      @TT "numerator"@ also works with Hilbert series.
    Example
      R = QQ[a..d]/(a^2,b^2,c^3);
      hf = hilbertSeries R
      numerator hf
    Text
      For a Laurent polynomial in a ring with inverses of variables, it gives
      the result after clearing all the denominators in each of the terms by
      multiplying by a suitable monomial.
    Example
      R = QQ[x,y,z,Inverses => true, MonomialOrder => Lex]
      numerator (x*y^-1+y*z^-2+1+y^-1*z^-1)
  SeeAlso
    denominator
    "fraction fields"
    hilbertSeries
///
