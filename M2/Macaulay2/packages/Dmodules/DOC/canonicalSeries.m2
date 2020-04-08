
///
   Consequences
    Item
   Description
    Text
      Code
      Pre
    Example
      CannedExample
   Subnodes
   Caveat
   SeeAlso
///

doc ///
   Key
     genToDistractionGens
     (genToDistractionGens, RingElement, Ring)
   Headline
     the image in the thetaRing of a torus-fixed element in a Weyl algebra
   Usage
     genToDistractionGen(f,S)
   Inputs
     f:RingElement 
       in a Weyl algebra D
     S:Ring 
       that is a stand in for the theta ring inside D
   Outputs
     :List  
       in S that is the result of applying [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Lemma 2.3.1] to f.
   Description
    Text
      Code
      Pre
    Example
      2+2
///

doc ///
   Key
     thetaIdeal
     (thetaIdeal, Ideal, Ring)
   Headline
     the image in the thetaRing of a torus-fixed ideal in a Weyl algebra
   Usage
     thetaIdeal(I,S)
   Inputs
     I:
       Ideal in a WeylAlgebra that is torus-fixed
   Outputs 
     :Ideal 
       that results from intersecting I with the thetaRing of D, as in 
       [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Lemma 2.3.1] to I.
   Description
    Text
      Code
      Pre
    Example
      2+2
///

doc ///
   Key
     cssExpts
     (cssExpts, Ideal, List)
   Headline
     the exponents of the canonical series solutions of I in the direction of a weight vector
   Usage
     cssExpts(I,w)
   Inputs
     I:
       holonomic ideal in a Weyl algebra D
     w:
       List of (generic) weights for I, of length half the number of variables in D
   Outputs
     :List 
       of exponents of the exponents of the canonical series solutions of I 
       in the direction of (-w,w), as in 
       [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Theorem 2.3.11].
   Description
    Text
      Code
      Pre
    Example
      2+2
///

doc ///
   Key
     cssExptsMult
     (cssExptsMult, Ideal, List)
   Headline
     the exponents (and multiplicities) of the canonical series solutions of I in the direction of a weight vector
   Usage
     cssExptsMult(I,w)
   Inputs
     I:
       holonomic ideal in a Weyl algebra D
     w:
       List of (generic) weights for I, of length half the number of variables in D
   Outputs
     :List 
       of exponents of the starting exponents of the canonical series solutions of I 
       in the direction of (-w,w), as in 
       [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@, Theorem 2.3.11], 
       together with their multiplicities. 
   Description
    Text
      Code
      Pre
    Example
      2+2
///

doc ///
   Key
     isTorusFixed
     (isTorusFixed, Ideal)
   Headline
     checks if an ideal in a Weyl algebra is torus-fixed
   Usage
     isTorusFixed I
   Inputs
     I:Ideal
       in a WeylAlgebra
   Outputs
     :Boolean 
       true if I is torus-fixed, false if not
   Description
    Text
       [@HREF("https://mathscinet.ams.org/mathscinet/pdf/1734566.pdf","SST")@], 
       just before Lemma 2.3.1.
///

