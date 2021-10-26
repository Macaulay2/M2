--- status: rewritten September 2018
--- author(s): Lily Silverstein

doc///
 Key
  parts
  (parts, RingElement)
 Headline
  display terms of a polynomial degree by degree
 Usage
  parts f
 Inputs
  f:RingElement
 Outputs
  :Expression
   with the terms of {\tt f} parenthesized degree by degree. The degrees are given in increasing order
 Description
  Text
   By default, {\tt Macaulay2} displays a polynomial @TO RingElement@ by
   putting its terms in decreasing order, 
   with respect to the @TO MonomialOrder@ of the ambient ring.
   On the other hand, {\tt parts} will display the terms in order of
   increasing degree, regardless of the term order of the ring. 
   Parentheses group together all terms of the same degree.
  Example
   R = QQ[x,y];
   f = (x + y + 1)^2
   parts f
   R = QQ[x,y, MonomialOrder => Lex];
   f = (x + y + 1)^2
   parts f
  Text
   The output is an @TO Expression@ of a special class, @TO Parenthesize@.
   Accessing the individual parenthesized parts of this expression is
   difficult, so for this purpose it may be better to use @TO part@.
  Example
   part(2, f)
   part(0, 1, f)
 SeeAlso
  degree
  monomials
  part
  select
  terms
  someTerms
  "graded and multigraded polynomial rings"
  "manipulating polynomials"
///
  

