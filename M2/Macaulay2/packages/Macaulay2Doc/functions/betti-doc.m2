--- status: DRAFT
--- author(s): caviglia, kummini
--- notes: functions below are all defined in betti.m2

doc ///
Node
  Key
    BettiTally
    (pdim, BettiTally)
    (codim, BettiTally)
    (degree, BettiTally)
    (poincare, BettiTally)
    (regularity, BettiTally)
    (hilbertSeries, ZZ, BettiTally)
    (hilbertPolynomial, ZZ, BettiTally)
    (lift, BettiTally, ZZ)
    (dual, BettiTally)
    (symbol *, QQ, BettiTally)
    (symbol *, ZZ, BettiTally)
    (symbol **, BettiTally, BettiTally)
    (symbol ++, BettiTally, BettiTally)
    (symbol SPACE, BettiTally, Array)
    (symbol SPACE, BettiTally, ZZ)
  Headline
    the class of all Betti tallies
  Description
    Text
      A Betti tally is a special type of @TO "Tally"@ that is printed as a display of graded Betti numbers.
      The class was created so the function @TO "betti"@ could return something that both prints nicely and
      from which information can be extracted.  The keys are triples @TT "(i,d,h)"@ encoding:
    Tree
      : @TT "i"@, the column labels, representing the homological degree;
      : @TT "d"@, a list of integers giving a multidegree; and
      : @TT "h"@, the row labels, representing the dot product of a weight covector and @TT "d"@.
    Text
      Only @TT "i"@ and @TT "h"@ are used in printing, and the weight covector can be modified by specifying
      the @TO [betti, Weights]@ option to @TO (betti, BettiTally)@.
    Example
      t = new BettiTally from { (0,{0},0) => 1, (1,{1},1) => 2, (2,{3},3) => 3, (2,{4},4) => 4 }
      betti(t, Weights => {2})
      peek oo
    Text
      For convenience, the operations of direct sum (@TO "++"@), tensor product (@TO "**"@), @TO codim@,
      @TO degree@, @TO dual@, @TO pdim@, @TO poincare@, @TO regularity@, and degree shifting (numbers in
      brackets or parentheses), have been implemented for Betti tallies.  These operations mimic the
      corresponding operations on chain complexes.
    Example
      t(5)
      t[-5]
      dual oo
      t ++ oo
      t ** t
      pdim t
      codim t
      degree t
      poincare t
      regularity t
    Text
      If the Betti tally represents the Betti numbers of a resolution of a module $M$ on a polynomial ring
      $R = K[x_0,...,x_n]$, then while the data does not uniquely determine $M$, it suffices to compute
      the @TO2 {hilbertPolynomial, "Hilbert polynomial"}@ and @TO2 {hilbertSeries, "Hilbert series"}@ of $M$.
    Example
      n = 3
      hilbertSeries(n, t)
      hilbertPolynomial(n, t)
    Text
      A Betti tally can be multiplied by an integer or by a rational number, and the values can be lifted
      to integers, when possible.
    Example
      (1/2) * t
      2 * oo
      lift(oo,ZZ)
  Subnodes
    MultigradedBettiTally

Node
  Key
    betti
   (betti, Ideal)
   (betti, Matrix)
   (betti, Module)
   (betti, GroebnerBasis)
  Headline
    display or modify a Betti diagram
  Usage
    betti M
  Description
    Text
      The function @TT "betti"@ creates and displays the Betti diagram of mathematical objects that
      can be presented using graded free modules and graded maps between them, such as ideals, modules,
      and chain complexes. The returned @TO BettiTally@ encapsulates the data from the entries of the
      displayed Betti diagram, in case they are needed in a program.
    Example
      S = QQ[x,y,z,w];
      I = monomialCurveIdeal(S, {1,2,3})
      t = betti res I
      peek oo
      t#(1, {2}, 2)
    Text
      The keys are triples @TT "(i,d,h)"@ encoding:
    Tree
      : @TT "i"@, the column labels, representing the homological degree;
      : @TT "d"@, a list of integers giving a multidegree; and
      : @TT "h"@, the row labels, representing the dot product of a weight covector and @TT "d"@.
    Text
      Only @TT "i"@ and @TT "h"@ are used in printing, and the weight covector can be modified by specifying
      the @TO [betti, Weights]@ option. The @TO2 {"heft vectors", "heft vector"}@ of the ring of the input
      object is the default choice for the weight covector.
    Example
      R = QQ[a,b,c, Degrees => {-1,-2,-3}];
      heft R
      betti koszul vars R
    Text
      If the ring has no heft vector, then the weights vector is taken to be all zero.
      If the option @TO [betti, Weights]@ is provided, the length of the given weight vector should
      be the same as the @TO2 {degreeLength, "degree length"}@ of the ring of the input object.
    Example
      betti(oo, Weights => {1})
    Text
      If the ring is multigraded, the function @TO (multigraded, BettiTally)@ may be used to extract
      information from all degree components of the Betti diagram at once.
    Example
      R = QQ[a,b,c,d, Degrees => {{1,0},{2,1},{0,1},{-2,1}}];
      heft R
      B = betti res coker vars R
      betti(B, Weights => {1,0})
      betti(B, Weights => {0,1})
      multigraded B
  Synopsis
    Heading
      Betti table of a Gröbner basis
    Usage
      betti G
    Inputs
      G:GroebnerBasis
      Weights=>List
    Outputs
      :BettiTally
        a diagram showing the degrees of the generators of the source
        and target modules of the matrix of generators of @TT "G"@
    Description
      Example
        S = ZZ/10007[x,y];
        G = gb ideal(x^3+y^3, x*y^4);
        gens G
        betti G
  Synopsis
    Heading
      Betti diagram showing the degrees of the target and source of a map
    Usage
      betti f
    Inputs
      f:Matrix
      Weights=>List
    Outputs
      :BettiTally
        a diagram showing the degrees of the generators of the source and target modules of @TT "f"@
    Description
      Text
        The diagram ignores the degree of the map itself.
      Example
        S = ZZ/10007[x,y];
        betti matrix {{x^3, x*y^2}, {y*x, y^2}}
  Synopsis
    Heading
      Betti diagram showing the degrees of generators and relations of a homogeneous module
    Usage
      betti M
    Inputs
      M:Module
      Weights=>List
    Outputs
      :BettiTally
        showing the zero-th, first graded, and total Betti numbers of $M$.
    Description
      Text
        Note that the Betti numbers are not minimized.
      Example
        S = ZZ/10007[x,y];
        betti coker matrix{{x^3, x*y^2}, {y*x^2, y^3}}
        betti coker map(S^{0,-1}, , matrix{{x^2, y}, {y^3, x^2}})
      Text
        Also see @TO "Varieties::betti(CoherentSheaf)"@.
  Synopsis
    Heading
      Betti diagram showing the degrees of generators of a homogeneous ideal
    Usage
      betti I
    Inputs
      I:Ideal
      Weights=>List
    Outputs
      :BettiTally
        showing the degrees of the generators and relations of the quotient of the ambient ring by $I$
    Description
      Text
        Note that the Betti numbers are not minimized.
      Example
        S = ZZ/10007[x,y];
        I = ideal(x,x^2,y^3);
        betti I
        betti comodule I
  SeeAlso
    minimalBetti
    multigraded
    regularity
    pdim
  Subnodes
    (multigraded, BettiTally)
    (betti, BettiTally)
    "OldChainComplexes :: betti(...,Minimize=>...)"
    minimalBetti

Node
  Key
    (betti, BettiTally)
    [betti, Weights]
  Headline
    view and set the weight vector of a Betti diagram
  Usage
    betti(t, Weights => w)
  Inputs
    t:BettiTally
    Weights=>List
      with the same length as the multidegrees in @TT "t"@, used as the weight vector @TT "w"@
  Outputs
    :BettiTally
      with the same homological degrees, multidegrees, and ranks. If a weight vector @TT "w"@
      is provided, the total degree weights in the resulting Betti tally will be recomputed by
      taking the dot products of @TT "w"@ with the multidegrees in the tally.
  Description
    Example
      R = ZZ/101[a..d, Degrees => {2:{1,0}, 2:{0,1}}];
      I = ideal random(R^1, R^{2:{-2,-2}, 2:{-3,-3}});
      t = betti res I
      peek t
    Text
      The following three displays show the first degree, the second degree, and the total degree, respectively.
    Example
      betti(t, Weights => {1,0})
      betti(t, Weights => {0,1})
      betti(t, Weights => {1,1})
      peek oo
  SeeAlso
    "heft vectors"
    MultigradedBettiTally
    multigraded
///

-----------

doc ///
Node
  Key
    MultigradedBettiTally
    (symbol SPACE, MultigradedBettiTally, Sequence)
  Headline
    the class of all multigraded Betti tallies
  Description
    Text
      A multigraded Betti tally is a special type of @TO BettiTally@ that is printed as a diagram of the
      multigraded Betti numbers. The class was created so that the method @TO (multigraded, BettiTally)@
      could return something that both prints nicely and from which information could be extracted. The
      content of a multigraded Betti tally is identical to the Betti tally from which it was constructed.
    Example
      B = new BettiTally from {(0, {0, 0}, 0) => 1, (1, {0, 2}, 2) => 1, (1, {1, 1}, 2) => 2, (1, {2, 0}, 2) => 1, (2, {1, 2}, 3) => 2, (2, {2, 1}, 3) => 2, (3, {2, 2}, 4) => 1}
      B = multigraded B
      peek B
    Text
      By default the data is presented as a table of polynomials where each column corresponds to a given
      homological degree appearing as the top entry and each monomial in the other entries represents the
      multidegree of a given summand.

      When @TO "compactMatrixForm"@ is set to @TT "false"@, the entries represent the multidegree of summands
      ordered by the total weight. The number of summands corresponding to a given multidegree appears to the
      left of the multidegree.
    Example
      compactMatrixForm = false
      B
    Text
      For convenience, various operations on @TT "BettiTally"@ such as direct sum (@TO "++"@), tensor product
      (@TO "**"@), @TO "pdim"@ and degree shifting (numbers in brackets or parentheses) are extended to work
      with multigraded Betti tables. These operations mimic the corresponding operations on chain complexes.
    Example
      compactMatrixForm = true
      B(-1,-1)
      B[1]
      B[1] ++ B
      B ** B
      compactMatrixForm = false
      B ** B
  Contributors
    This feature was implemented by Mahrud Sayrafi based on earlier work by Gregory G. Smith.
  SeeAlso
    BettiTally
    (multigraded, BettiTally)
    "compactMatrixForm"

Node
  Key
     multigraded
    (multigraded, BettiTally)
  Headline
    convert a Betti tally into a multigraded Betti tally
  Usage
    multigraded t
  Inputs
    t:BettiTally
  Outputs
    :MultigradedBettiTally
      different from the input only in the printed diagram
  Description
    Text
      A multigraded Betti tally is a special type of @TO "BettiTally"@ that both prints nicely and
      from which multigraded Betti numbers could be easily extracted.
    Example
      R = ZZ/101[a..d, Degrees => {2:{1,0},2:{0,1}}];
      I = ideal random(R^1, R^{2:{-2,-2},2:{-3,-3}});
      t = betti res I
      B = multigraded t
    Text
      By changing the weights, we can reorder the columns of the diagram. The following three displays show
      the first degree, the second degree, and the total degree, respectively.
    Example
      betti(B, Weights => {1,0})
      betti(B, Weights => {0,1})
      betti(B, Weights => {1,1})
  SeeAlso
    MultigradedBettiTally
    (betti, BettiTally)
///
