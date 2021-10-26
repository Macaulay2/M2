-- Author: Lily Silverstein
-- Rewritten August 2018

doc ///
 Key
  leadMonomial
  (leadMonomial, RingElement)
 Headline
  the leading monomial of a ring element
 Usage
  leadMonomial f
 Inputs
  f:RingElement
   a polynomial
 Outputs
  :RingElement
   the monomial, with no coefficient, of the leading term of {\tt f}
 Description
  Text
   The leading term is with respect to the monomial ordering in the
   ring. See @TO "monomial orderings"@ for details.
  Example
   R = ZZ/5[a,b,c]; f = a*b^2 + 3*b*c^3 + 2;
   leadMonomial f
   S = ZZ/5[a,b,c, MonomialOrder => Lex]; g = a*b^2 +3*b*c^3 + 2;
   leadMonomial g
 SeeAlso
  leadComponent
  leadMonomial
  leadTerm
  monomials
  someTerms
  terms
///
