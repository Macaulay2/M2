--- status: Draft
--- author(s): Decker 
--- notes: 

doc ///
Node
  Key
    radical
   (radical, Ideal)
   [radical, Strategy]
   [radical, Unmixed]
   [radical, CompleteIntersection]
  Headline
    the radical of an ideal
  Usage
    radical I
  Inputs
    I:Ideal
    Unmixed=>Boolean
      whether it is known that the ideal {\tt I} is unmixed.
      The ideal $I$ is said to be unmixed if all associated primes of $R/I$ have the same dimension.
      In this case the algorithm tends to be much faster.
    Strategy=>Symbol
      the strategy to use, either @TT "Decompose"@ or @TT "Unmixed"@
    CompleteIntersection=>Ideal
      an ideal @TT "J"@ of the same height as @TT "I"@ whose generators form a maximal regular sequence contained
      in @TT "I"@. Providing this option as a hint allows a separate, often faster, algorithm to be used to compute
      the radical. This option should only be used if @TT "J"@ is nice in some way. For example, if @TT "J"@ is
      randomly generated, but @TT "I"@ is relatively sparse, then this will most likely run slower than just giving
      the @TO "Unmixed"@ option.
  Outputs
    :Ideal
      the radical of @TT "I"@
  Description
    Code
     "If I is an ideal in an affine ring (i.e. a quotient of a polynomial 
     ring over a field), and if the characteristic of this field is
     large enough (see below), then this routine yields the radical of
     the ideal I.",
     PARA{},
     "The method used is the Eisenbud-Huneke-Vasconcelos algorithm.
     See their paper in Inventiones Mathematicae, 1993, for more details on the
     algorithm.",
     PARA{},
     "The algorithms used generally require that the characteristic of the
     ground field is larger than the degree of each primary component.  In 
     practice, this means that if the characteristic is something like 32003,
     rather than, for example, 5, the methods used will produce the radical of ", TT "I", ".  Of
     course, you may do the computation over ", TO "QQ", ", but it will often run much
     slower.  In general, this routine still needs to be tuned for speed.",
     PARA{},
     "Computes the radical of ", TT "I", " using the Eisenbud-Huneke-Vasconcelos algorithm.
     If ", TT "I", " is  ", ofClass MonomialIdeal, ", a faster \"combinatorial\" algorithm is used.",
     EXAMPLE {
	  "R=QQ[x,y]",
	  "I=ideal((x^2+1)^2*y, y+1)",
	  "radical I"
	  },
     PARA{},
     "For another example, see ", TO "component example", ".",
  Caveat
    The current implementation requires that the characteristic of the ground field is either zero
    or a "large" prime (unless @TT "I"@ is @ofClass MonomialIdeal@).
  SeeAlso
    "MinimalPrimes :: minimalPrimes"
    topComponents
    removeLowestDimension
    "Colon :: saturate"
    "Colon :: Ideal : Ideal"
///
