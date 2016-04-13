doc ///
   Key
     nonminimalResolution
     (nonminimalResolution, Ideal)
     (nonminimalResolution, Matrix)
     (nonminimalResolution, Module)
   Headline
     compute a non-minimal graded free resolution
   Usage
     C = nonminimalResolution I
   Inputs
     I:Ideal
       or @ofClass Matrix@ or @ofClass Module@, in a polynomial ring
       or skew commuting polynomial ring, over a finite prime field
   Outputs
     C:ChainComplex
   Description
    Text
      Given a singly-graded module, this function
      computes a non-minimal free resolution.  If the 
      input is an ideal $I \subset S$, it computes a non-minimal
      resolution of $S^1/I$.
      
      The key benefit of this function is that it allows
      a much faster method for computing the 
      betti numbers of the {\bf minimal} free resolution.
      
    Example
      I = Grassmannian(1,6, CoefficientRing => ZZ/101);
      S = ring I      
      elapsedTime C = nonminimalResolution I
      elapsedTime C1 = res ideal(I_*)
      betti C1 == betti C
    Text
      For a non-minimal resolution, @TO betti@ gives the minimal Betti
      numbers, while @TO "nonminimalBetti"@ gives the actual ranks
      of the complex.
    Example
      betti C
      nonminimalBetti C
    Text
      If the resolution is not large, this function can be slower than
      the usual function @TO resolution@.  But for larger examples, 
      if one is only interested in the betti numbers, this function
      can be hundreds or thousands of times faster.      
   Caveat
     Released in M2 1.9, still experimental.  Only works over finite prime fields. Uses quite alot of memory.
   SeeAlso
     betti
     nonminimalBetti
     resolution
///
