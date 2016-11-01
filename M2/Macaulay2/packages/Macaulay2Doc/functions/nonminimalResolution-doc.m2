doc ///
   Key
     FastNonminimal
     [res,FastNonminimal]
   Headline
     compute a non-minimal graded free resolution
   Usage
     C = res(I, FastNonminimal => true, ...)
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
      If that is your only interest (i.e. you don't need the complex itself), 
      instead use @TO minimalBetti@.
    Example
      I = Grassmannian(1,6, CoefficientRing => ZZ/101);
      S = ring I      
      elapsedTime C = res(I, FastNonminimal => true)
      elapsedTime C1 = res ideal(I_*)
      betti(C, Minimize => true) == betti C1
    Text
      For a non-minimal resolution, @TO betti@ gives the actual Betti
      numbers, and using the @TO [betti,Minimize]@ option gives the ranks
      in a minimal resolution (which is itself not computed).
    Example
      betti C
      betti(C, Minimize => true)
    Text

      As mentioned above, if you are just interested in the minimal betti numbers of the ideal or
      module, then use @TO minimalBetti@, as it avoids construction of the
      non-minimal free resolution.
    Example
      minimalBetti I
    Text

      If the resolution is not large, this function can be slower than
      the usual function @TO resolution@.  But for larger examples, 
      if one is only interested in the betti numbers, this function
      can be hundreds or thousands of times faster.      
   Caveat
     Released in M2 1.9, still experimental.  Only works over finite prime fields. Uses quite alot of memory.
   SeeAlso
     minimalBetti
     betti
     [betti,Minimize]
     resolution
///
