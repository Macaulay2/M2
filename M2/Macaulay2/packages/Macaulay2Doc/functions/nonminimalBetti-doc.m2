doc ///
   Key
     [betti,Minimize]
   Headline
     minimal betti numbers of a non-minimal free resolution
   Usage
     betti(C, Minimize => true)
   Inputs
     C:ChainComplex
       computed using @TO FastNonminimal@ (and therefore a
           non-minimal free resolution of an ideal or
           module in a polynomial ring
           or skew commuting polynomial ring, over a finite prime field)
   Outputs
     :BettiTally
   Description
    Text
      Given a chain complex computed using {\tt res(I, FastNonminimal => true)} (@TO FastNonminimal@),
      returns the minimal graded Betti numbers of this complex.

      To get the actual betti numbers of the non-minimal resolution, use @TO betti@.

      If you simply want the minimal betti numbers of a module or ideal {\tt I},
      use @TO "minimalBetti"@.
    Example
      I = Grassmannian(1,6, CoefficientRing => ZZ/101);
      S = ring I      
      elapsedTime C = res(I, FastNonminimal => true)
    Text
      For a non-minimal resolution, @TO betti@ gives the minimal Betti
      numbers, while @TO "nonminimalBetti"@ gives the actual ranks
      of the complex.
    Example
      betti C
      betti(C, Minimize=>true)
    Text
      This command is useful if the non-minimal free resolution has already been computed.
      However, as mentioned above, if one wants the minimal betti numbers of an ideal or module,
      it is recommended to use the function @TO "minimalBetti"@ as that avoids much computation,
      and allows the use of length and degree limits.
   Caveat
     Released in M2 1.9, still experimental.  Only works over finite prime field.
     If the complex is the resolution of a non-homogeneous or multi-homogeneous object,
     then this function will result in an error.
   SeeAlso
     minimalBetti
     betti
     resolution
     FastNonminimal
///
