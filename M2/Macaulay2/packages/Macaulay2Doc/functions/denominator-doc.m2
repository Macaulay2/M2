--- status: DRAFT
--- author(s): MES
--- notes: 

undocumented { (denominator,QQ) }

doc ///
  Key
    denominator
    (denominator,Divide)
  Headline
    denominator of a fraction
  Usage
    denominator x
  Inputs
    x:
      a fraction
  Outputs
    :
      the denominator of @TT "x"@
  Description
    Example
      denominator (4/6)
    Example
      R = frac(ZZ[x,y]);
      denominator((x+2*y-3)/(x-y))
    Text
      @TT "denominator"@ also works with Hilbert series.
    Example
      R = QQ[a..d]/(a^2,b^2,c^3);
      hf = hilbertSeries R
      denominator hf
    Text
      For a Laurent polynomial in a ring with inverses of variables, it gives
      the monomial needed to clear all the denominators in each of the terms.
    Example
      R = QQ[x,y,z,Inverses => true, MonomialOrder => Lex]
      denominator (x*y^-1+y*z^-2+1+y^-1*z^-1)
  SeeAlso
    numerator
    "fraction fields"
    hilbertSeries
///

