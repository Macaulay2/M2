-- TODO: (symbol ^, Ring, BettiTally)

doc ///
Node
  Key
   (betti, GradedModule)
   (betti, ChainComplex)
  Headline
    Betti diagram showing the of degrees in a graded module or chain complex
  Usage
      betti C
  Inputs
      C:{GradedModule,ChainComplex}
      Weights=>List
      Minimize=>Boolean
  Outputs
      :BettiTally
        showing the degrees of the generators of the modules in @TT "C"@
  Description
      Text
        The diagram can be used to determine the degrees of the entries in the matrices of the differentials
        in a chain complex (which is a type of graded module) provided they are homogeneous maps of degree 0.
      Example
        R = ZZ/101[a..h]
        p = genericMatrix(R,a,2,4)
        q = generators gb p
        C = resolution cokernel leadTerm q
        betti C
      Text
        Column @TT "j"@ of the top row of the diagram gives the rank of the free module @TT "C_j"@.
        The entry in column @TT "j"@ in the row labelled @TT "i"@ is the number of basis elements of
        (weighted) degree @TT "i+j"@ in the free module @TT " C_j"@. When the chain complex is the
        resolution of a module the entries are the total and the graded Betti numbers of the module.
  Subnodes
    [betti, Minimize]
    (symbol ^, Ring, BettiTally)

Node
   Key
     [betti, Minimize]
   Headline
     minimal betti numbers of a non-minimal free resolution
   Usage
     betti(C, Minimize => true)
   Inputs
     C:ChainComplex
       computed using @TO FastNonminimal@ (and therefore a non-minimal free resolution of an ideal or
       module in a polynomial ring or skew commuting polynomial ring, over a finite prime field)
   Outputs
     :BettiTally
   Description
    Text
      Given a chain complex computed using {\tt res(I, FastNonminimal => true)} (@TO FastNonminimal@),
      returns the minimal graded Betti numbers of this complex.
    Example
      I = Grassmannian(1,6, CoefficientRing => ZZ/101);
      S = ring I
      elapsedTime C = res(I, FastNonminimal => true)
    Text
      For a non-minimal resolution, @TO betti@ gives the actual ranks of the complex.
      If the option @TT "Minimize => true"@ is given, the minimal Betti numbers are returned.
    Example
      betti C
      betti(C, Minimize => true)
    Text
      This command is useful if the non-minimal free resolution has already been computed. However,
      to get the minimal betti numbers of an ideal or module, it is recommended to use the function
      @TO "minimalBetti"@ as that avoids much computation and allows the use of length and degree limits.
   Caveat
     Released in M2 1.9, still experimental.  Only works over finite prime field.
     If the complex is the resolution of a non-homogeneous or multi-homogeneous object,
     then this function will result in an error.
   SeeAlso
     minimalBetti
     betti
     resolution
     [resolution, FastNonminimal]

Node
  Key
    (symbol ^, Ring, BettiTally)
  Headline
    construct a chain complex with prescribed Betti table
  Usage
    R^t
  Inputs
    R:Ring
    t:BettiTally
  Outputs
    C:ChainComplex -- whose Betti table matches {\tt t}
  Description
    Text
      Given a ring $R$, a chain complex with zero maps over $R$ that has a prescribed Betti table can be
      constructed. Negative entries are ignored and rational entries produce an error. Multigraded rings
      work only if the Betti tally contains degrees of the correct degree length.
    Example
      R = QQ[x,y]
      t = new BettiTally from { (0,{0},0) => 1, (1,{1},1) => 2, (2,{3},3) => 3, (2,{4},4) => 4 }
      C = R^t
      betti C
      C.dd
  Contributors
    Hans-Christian von Bothmer implemented this feature.
  SeeAlso
    betti
    BettiTally
///
