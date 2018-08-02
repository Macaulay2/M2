-- Author: Lily Silverstein
-- Rewritten August 2018

doc ///
 Key
  leadCoefficient
  (leadCoefficient, RingElement)
 Headline
  the coefficient of the leading term
 Usage
  leadCoefficient f
 Inputs
  f:RingElement
   a polynomial
 Outputs
  :RingElement
   an element of the @TO coefficientRing@ of the ring of {\tt f}
 Description
  Text
   The leading term is with respect to the monomial ordering in the
   ring. See @TO "monomial orderings"@ for details.
  Example
   R = ZZ/5[a,b,c]; f = a*b^2 + 3*b*c^3 + 2;
   leadCoefficient f
   S = ZZ/5[a,b,c, MonomialOrder => Lex]; g = a*b^2 +3*b*c^3 + 2;
   leadCoefficient g
 SeeAlso
  coefficients
  coefficientRing
  leadComponent
  leadMonomial
  leadTerm
  someTerms
  terms
///
