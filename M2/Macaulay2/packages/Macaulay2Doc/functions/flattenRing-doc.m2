-- -*- coding: utf-8 -*-
--- status: rewritten July 2018
--- author(s): Lily Silverstein
--- notes: 

doc ///
 Key
  flattenRing
  (flattenRing, Ring)
  (flattenRing, Ideal) 
  (flattenRing, GaloisField)
  (flattenRing, QuotientRing)
  (flattenRing, PolynomialRing)
 Headline
  write a ring as a (quotient of a) polynomial ring
 Usage
  flattenRing R
  flattenRing I
  flattenRing G
 Inputs
  R:Ring
   OR
  I:Ideal
   OR
  G:GaloisField
 Outputs
  S:Ring
   a ring isomorphic to {\tt R}, but represented as a (quotient of a) polynomial ring, or
  J:Ideal
   the ideal of {\tt S} corresponding to (isomorphic to) {\tt I}
  F:RingMap
   the isomorphism from {\tt R} to {\tt S}
  G:RingMap
   the isomorphism from {\tt S} to {\tt R} 
 Description
  Text
   Use {\tt flattenRing} to get an isomorphic representation
   of a ring over a different coefficient ring. 
   
   For instance the following ring $R$ is defined to have
   indeterminates $y,z$, and coefficients in the quotient
   ring $\mathbb{Z}[x]/(x^2-3)$.
  Example
   A = ZZ[x] / (x^2-3);  R = A[y,z] / (x*y^2-z^2, y^3);
   describe R
  Text
   To work with the isomorphic ring $S = \mathbb{Z}[x,y,z]/(x^2-3,x*y^2-z^2,y^3)$
   instead, use {\tt flattenRing}.
  Example 
   flattenRing R
  Text
   The default output is the sequence $(S, F)$, where 
   $S$ is the flattened ring, and $F$ is the @TO RingMap@ from $R$ to $S$.
   Here we demonstrate how to store and use the map $F$.
  Example
   I = ideal(y^2 - 3*x*z)
   (S, F) = flattenRing R;
   F(I)
  Text
   Flattening an ideal instead of a quotient ring can save a lot of time spent computing the
   Gröbner basis of the resulting ideal, if the flattened quotient is not needed. Notice that
   the ring map in the following example is {\em not} the same as the previous example; the
   following ring map is from $R$ to $\mathbb{Z}[x,y,z]$, while the previous map was from $R$ 
   to $\mathbb{Z}[x,y,z]/(x^2-3,x*y^2-z^2,y^3)$.
  Example
   A = ZZ[x] / (x^2-3);  R = A[y,z] / (x*y^2-z^2, y^3);
   I = ideal(y^2 - 3*x*z);
   (J, F) = flattenRing I
  Text
   {\tt flattenRing} can also be used to represent @TO GaloisField@s
   as quotients of polynomial rings. If $K$ is the Galois field of
   order $p^n$, {\tt flattenRing(K)} will represent $K$ as a quotient
   of $\mathbb{Z}_p[a]$.
  Example
   K = GF(5^3)
   flattenRing K
  Text
   Or, if a different variable was specified in the construction
   of $K$, {\tt flattenRing K} will follow suit.
  Example
   L = GF(7, 5, Variable => z);
   flattenRing L
  Text
   Use {\tt Result => 1} to return the ring only. 
   The default behavior is equivalent to {\tt Return => 2}. 
   Use {\tt Result => 3} to return the ring, the isomorphism $F:R\to S$,
   and the inverse map $F^{-1}:R\to S$. 
  Example
   flattenRing(R, Result => 1)
   flattenRing(R, Result => 2)   
   flattenRing(R, Result => 3)
  Text
   These are the only numeric options for {\tt Result}. However,
   we can also specify the output(s) desired with a sequence of 
   length 1, 2, or 3. The elements of the sequence correspond to the 
   {\tt Result => 1}, {\tt Result => 2}, and {\tt Result => 3} output
   sequences, respectively. Any element of the sequence can be
   suppressed by asking for {\tt Nothing} instead. For example,
   to return the map $F$ only, we ask for the sequence of length 2,
   $(S, F)$, but suppress $S$:
  Example
   flattenRing(R, Result => (Nothing, RingMap)) 
  Text
   Note that {\tt Result => RingMap} is NOT a valid way to return $F$
   only, because an output of length 1 can only have a ring or an ideal.

   Similarly, to return $S$ and $F^{-1}$ only:
  Example
   flattenRing(R, Result => (Ring, Nothing, RingMap)) 
  Text
   Omitting an element of the sequence is equivalent to asking for
   it to be returned.
  Example
   flattenRing(R, Result => (Nothing, )) 
   flattenRing(R, Result => ( , Nothing, ) )
  Text
   When running {\tt flattenRing(Ideal)}, there are additional choices for
   the {\tt Result} option: the first element of the sequence, which is
   always the ring in the {\tt flattenRing(Ring)} case, can now be either
   the new ring or the corresponding new ideal.
   The default is to return the ideal, including when using numeric
   options for {\tt Result} or omitting an object type in the sequence.
  Example
   flattenRing(I, Result => 1)
   flattenRing(I, Result => 3)
   flattenRing(R, Result => ( , Nothing, ) )
   flattenRing(I, Result => (Ring, Nothing, RingMap)) 
   flattenRing(I, Result => (Ring, RingMap)) 
   flattenRing(I, Result => Ideal)
  Text
   By default {\tt flattenRing} attempts to define a new ring over
   either {\tt ZZ} or a base field.
  
   In the following example, the coefficient ring of the result 
   is the fraction field $K$.
  Example
   K = frac(ZZ[a])
   B = K[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
   (D, F) = flattenRing B
   describe D	  
  Text
   Once a ring has been declared to be a field with @TO toField@
   then it will be used as the coefficient ring.
  Example
   A = QQ[a]/(a^2-3);
   L = toField A
   B = L[x,y,z]/(a*x^2-y^2-z^2, y^3, z^3)
   (D, F) = flattenRing(B[s,t])
   describe D	  
  Text
   Use the {\tt CoefficientRing} option to specify a different base
   field or ring.
  Example
   (D, F) = flattenRing(B[s,t], CoefficientRing => QQ)
   describe D
  Text
   Here is a more complicated example.
  Example
   use L
   C1 = L[s,t];
   C2 = C1/(a*s-t^2);
   C3 = C2[p_0..p_4]/(a*s*p_0)[q]/(q^2-a*p_1);
   (D, F) = flattenRing(C3, CoefficientRing=>C2)
   describe D
   (D, F) = flattenRing(C3, CoefficientRing=>QQ)
   describe D
  Text
   Multiple calls to {\tt flattenRing} may result in defining
   multiple rings that are regarded as different
   objects by {\tt Macaulay2}. (See @TO "rings"@ or 
       @TO "working with multiple rings"@ for more information.)
  Example
   flattenRing(B[s,t]) === flattenRing(B[s,t])
 SeeAlso
  coefficientRing
  describe
  GF
  lift
  presentation
  promote
  toField
  trim
  "finite fields"
  "rings"
  "working with multiple rings"
 Subnodes
   [flattenRing, CoefficientRing]
   [flattenRing, Result]
///

doc ///
 Key
  [flattenRing, CoefficientRing] 
 Headline
  specify the coefficient ring of the flattened ring
 Description
  Text
   Use this option to change the default behavior of @TO flattenRing@,
   by setting the option to the desired @TO coefficientRing@ of the output
   ring.
  Example
   k = toField (QQ[x]/(x^2+x+1));
   R = k[y]/(x-y+2);
   (S, f) = flattenRing(R); describe S
   (S2, f2) = flattenRing(R, CoefficientRing => QQ); describe S2
  Text
   See @TO flattenRing@ documentation for more examples and details.
 SeeAlso
  flattenRing
///

doc ///
 Key  
  [flattenRing, Result]
 Headline
  specify which output(s) to return
 Description
  Text
   The @TO flattenRing@ documentation goes into much greater detail about the
   {\tt Result} option. This node has some examples only.
  Example
   k = toField (QQ[x]/(x^2+x+1));
   R = k[y]/(x-y+2);
   flattenRing(R, Result => 1)
   flattenRing(R, Result => 2)   
   flattenRing(R, Result => 3)
   flattenRing(R, Result => (Nothing, RingMap)) 
   flattenRing(R, Result => (Ring, Nothing, RingMap)) 
   flattenRing(R, Result => (Nothing, )) 
   flattenRing(R, Result => ( , Nothing, ) )
   I = ideal(x*y+y^2-5);
   flattenRing(I, Result => 1)
   flattenRing(I, Result => 3)
   flattenRing(I, Result => (Ring, Nothing, RingMap)) 
   flattenRing(I, Result => (Ideal, Nothing, RingMap))    
   flattenRing(I, Result => (Ring, RingMap)) 
   flattenRing(I, Result => Ideal)
 SeeAlso
  flattenRing
///
