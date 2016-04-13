doc ///
   Key
     nonminimalBetti
     (nonminimalBetti, ChainComplex)
   Headline
     non-minimal betti numbers of a non-minimal free resolution
   Usage
     nonminimalBetti C
   Inputs
     C:ChainComplex
       computed using @TO nonminimalResolution@ (and therefore a
           non-minimal free resolution of an ideal or
           module in a polynomial ring
           or skew commuting polynomial ring, over a finite prime field
   Outputs
     :BettiTally
   Description
    Text
      Given a chain complex computed using  @TO nonminimalResolution@,
      returns the nonminimal graded Betti numbers of this complex.

      To get the minimal betti numbers, use @TO betti@.  This 
      function is available as internal information, or in the case
      when it is desirable to work with the
      non-minimal resolution as a complex.
      
      It can be used to get an idea of what might need to be
      computed, in case the minimal betti numbers are needed.
    Example
      I = Grassmannian(1,6, CoefficientRing => ZZ/101);
      S = ring I      
      elapsedTime C = nonminimalResolution I
    Text
      For a non-minimal resolution, @TO betti@ gives the minimal Betti
      numbers, while @TO "nonminimalBetti"@ gives the actual ranks
      of the complex.
    Example
      betti C
      nonminimalBetti C
   Caveat
     Released in M2 1.9, still experimental.  Only works over finite prime field.
   SeeAlso
     betti
     nonminimalResolution
     resolution
///
