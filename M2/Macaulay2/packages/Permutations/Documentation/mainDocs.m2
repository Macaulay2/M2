-- Permutation
-- (permutation, VisibleList)
doc ///
  Key
    Permutation
    permutation
    (permutation, VisibleList)
  Headline
    the Permutation type
  Description
    Text
      Permutations are constructed from lists. To create a permutation, 
      use the {\tt permutation} method.
    Example
      p = permutation {3,1,2,4,5}
    Text
      Permutations must be constructed from lists consisting of only the integers
      $1 \dots n$. If a list contains any other elements or does not consist of 
      the entire range, then an error is thrown.
///

-- ascendingRuns
doc ///
  Key
    ascendingRuns
    (ascendingRuns, Permutation)
  Headline
    computes the ascending runs of a permutation
  Usage
    ascendingRuns w
  Inputs
    w:Permutation
  Outputs
    allRuns:List
  Description
    Text
      An {\em ascending run} is a maximal subsequence of successive ascents.
    Example
      p = permutation {3,1,2,5,4}
      ascendingRuns p
  SeeAlso
    ascents
    descendingRuns
    descents
    exceedances
    saliances
    records
///

-- ascents
doc ///
  Key
    ascents
    (ascents, Permutation)
  Headline
    computes the ascents of a permutation
  Usage
    ascents w
  Inputs
    w:Permutation
  Outputs
    :List
  Description
    Text
      A permutation $p=(p_1 \, \dots \, p_n)$ has an {\em ascent} at $i$ (for $i < n$)
      if $p(i) < p(i+1)$.
    Example
      p = permutation {3,1,2,5,4}
      ascents p
  SeeAlso
    ascendingRuns
    descendingRuns
    descents
    exceedances
    saliances
    records
///

-- cycleDecomposition
doc ///
  Key
    cycleDecomposition
    (cycleDecomposition, Permutation)
  Headline
    computes the decomposition of a permutation as a product of disjoint cycles
  Usage
    cycleDecomposition w
  Inputs
    w:Permutation
  Outputs
    :List
  Description
    Text
      Every permutation can be decomposed into a product of disjoint cycles.
      We follow the convention to write the decomposition in its "canonical" or 
      "standard" form. So
      
      1. each cycle is listed with its largest element first, and @BR{}@
      2. the cycles are listed in increasing order.
    Example
      p = permutation {3,1,2,5,4}
      cycleDecomposition p
  SeeAlso
    cycleType
///

-- cycleType
doc ///
  Key
    cycleType
    (cycleType, Permutation)
  Headline
    computes the cycle type of a permutation
  Usage
    cycleType w
  Inputs
    w:Permutation
  Outputs
    :Sequence
  Description
    Text
      A permutation's {\em cycle type} is the sequence consisting of the lengths of the 
      cycles in its cycle decomposition, listed in weakly decreasing order.
    Example
      p = permutation {3,1,2,5,4}
      cycleType p
  SeeAlso
    cycleDecomposition
///

-- descendingRuns
doc ///
  Key
    descendingRuns
    (descendingRuns, Permutation)
  Headline
    computes the descending runs of a permutation
  Usage
    descendingRuns w
  Inputs
    w:Permutation
  Outputs
    allRuns:List
  Description
    Text
      A {\em descending run} is a maximal subsequence of successive descents.
    Example
      p = permutation {3,1,2,5,4}
      descendingRuns p
  SeeAlso
    ascendingRuns
    ascents
    descents
    exceedances
    saliances
    records
///

-- descents
doc ///
  Key
    descents
    (descents, Permutation)
  Headline
    computes the descents of a permutation
  Usage
    descents w
  Inputs
    w:Permutation
  Outputs
    :List
  Description
    Text
      A permutation $p=(p_1 \, \dots \, p_n)$ has a {\em descent} at $i$ (for $i < n$)
      if $p(i) > p(i+1)$.
    Example
      p = permutation {3,1,2,5,4}
      descents p
  SeeAlso
    ascendingRuns
    ascents
    descendingRuns
    exceedances
    saliances
    records
///

-- exceedances
doc ///
  Key
    exceedances
    (exceedances, Permutation)
    [exceedances, Weak]
  Headline
    computes the exceedances of a permutation
  Usage
    exceedances w
    exceedances(w, Weak)
  Inputs
    w:Permutation
  Outputs
    :List
  Description
    Text
      A permutation $p=(p_1 \, \dots \, p_n)$ has an {\em exceedance} at $i$ if 
      $p(i) > i$.
    Example
      p = permutation {3,1,2,5,4}
      exceedances p
    Text
      This is called a {\em weak exceedance} if the inequality is not 
      strict, i.e., $p(i) \geq i$.
    Example
      p = permutation {3,1,2,5,4}
      exceedances(p, Weak=>true)
  SeeAlso
    ascendingRuns
    ascents
    descendingRuns
    descents
    saliances
    records
///

-- extend(w,n)
doc ///
  Key
    extend
    (extend, Permutation, ZZ)
  Headline
    rewrites a permutation as a permutation on more symbols
  Usage
    extend(w,n)
  Inputs
    w:Permutation
    n:ZZ
  Outputs
    :Permutation
  Description
    Text
      A permutation on $n$ symbols is an element of the symmetric group on $n$ 
      symbols, $\mathfrak{S}_n$. However, it can also be regarded as a permutation
      on $N > n$ symbols as well by fixing the $(n+1)$st through $N$th symbols.
    Example
      p = permutation {3,1,2,5,4}
      extend(p, 7)
  SeeAlso
    trim
///

-- extend(w,v)
-- TODO: how to write documentation for tuple output
doc ///
  Key
    (extend, Permutation, Permutation)
  Headline
    rewrites two permutations to be permutations on the same number of symbols
  Usage
    e = extend(w,v)
  Inputs
    w:Permutation
    v:Permutation
  Outputs
    e:Sequence
  Description
    Text
      For ease, we can also extend two permutations so that they are regarded as 
      permutations on the same number of symbols. More precisely, if we have 
      permutations $p=(p_1, \dots, p_n)$ and $q=(q_1, \dots, q_m)$, then 
      {\tt extend(p,q)} will return both $p$ and $q$ as permutations on
      $\text{max}(m,n)$ symbols.
    Example
      p = permutation {3,1,2,5,4}
      q = permutation {2,3,1,4,5,7,6}
      extend(p,q)
  SeeAlso
    (extend, Permutation, ZZ)
    trim
///

-- fixedPoints
doc ///
  Key
    fixedPoints
    (fixedPoints, Permutation)
  Headline
    computes the fixed points of a permutation
  Usage
    fixedPoints w
  Inputs
    w:Permutation
  Outputs
    :List
  Description
    Text
      A permutation $p$ has a {\em fixed point} at $i$ if $p(i) = i$.
      {\tt fixedPoints} computes all of the fixed points of a permutation.
    Example
      p = permutation {2,1,3,5,4}
      fixedPoints p
  SeeAlso
    isDerangement
///

-- foataBijection
doc ///
  Key
    foataBijection
    (foataBijection, Permutation)
  Headline
    computes the image of a permutation under the Foata bijection
  Usage
    foataBijection w
  Inputs
    w:Permutation
  Outputs
    :Permutation
  Description
    Text
      Foata's fundamental bijection is a bijection between a permutation's standard 
      cycle decomposition and another permutation read the same (in one-line notation)
      as the decomposition with its parentheses removed. For example, if $p = (3 \, 2 \, 1)(5 \, 4)$
      (written in cycle notation), then its corresponding permutation (written in one-line 
      notation) is $\hat{p} = (3 \, 2 \, 1 \, 5 \, 4)$.
    Example
      p = permutation {3,1,2,5,4}
      foataBijection p
///

-- inverse(w)
doc ///
  Key
    (inverse, Permutation)
  Headline
    computes the inverse of a permutation
  Usage
    inverse w
  Inputs
    w:Permutation
  Outputs
    :Permutation
  Description
    Example
      p = permutation {3,1,2,5,4}
      fixedPoints p
///

-- inversions
doc ///
  Key
    inversions
    (inversions, Permutation)
  Headline
    computes the inversions of a permutation
  Usage
    inversions w
  Inputs
    w:Permutation
  Outputs
    :List
  Description
    Text
      A permutation $p$ has an {\em inversion} $(i,j)$ if $i < j$ and $p(i) > p(j)$.
      {\tt inversions} computes all of the inversions of a permutation.
    Example
      p = permutation {3,1,2,5,4}
      inversions p
  SeeAlso
    length
///

-- isDerangement
doc ///
  Key
    isDerangement
    (isDerangement, Permutation)
  Headline
    whether a permutation is a derangement
  Usage
    fixedPoints w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is a {\em derangement} if it has no fixed points.
    Example
      p = permutation {3,1,2,5,4}
      isDerangement p
  SeeAlso
    fixedPoints
///

-- isEven
doc ///
  Key
    isEven
    (isEven, Permutation)
  Headline
    whether a permutation is even
  Usage
    isEven w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is {\em even} if it can be written as a product of an
      even number of transpositions. Equivalently, a permutation is even if its
      @TO sign@ is $1$.
    Example
      p = permutation {3,1,2,5,4}
      isEven p
  SeeAlso
    isOdd
    (sign, Permutation)
///

-- isOdd
doc ///
  Key
    isOdd
    (isOdd, Permutation)
  Headline
    whether a permutation is odd
  Usage
    isOdd w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is {\em odd} if it can be written as a product of an
      odd number of transpositions. Equivalently, a permutation is odd if its
      @TO sign@ is $-1$.
    Example
      p = permutation {3,1,2,5,4}
      isOdd p
  SeeAlso
    isEven
    (sign, Permutation)
///

-- isWellDefined
doc ///
  Key
    (isWellDefined, Permutation)
  Headline
    checks if a list is a valid permutation
  Usage
    isWellDefined w
    isWellDefined(w)
  Inputs
    w:Permutation
  Outputs
     :Boolean
  Description
    Text
      @TT "isWellDefined p"@ determines if @TT "p"@ is a valid permutation.
      Permutations must be constructed from lists consisting of only the integers
      $1 \textemdash n$. If a list contains any other elements or does not consist of the entire
      range, then an error is thrown.
    Example
      isWellDefined permutation {1, 2, 3}
      isWellDefined permutation {0, 1, 2}
      isWellDefined permutation {1, 1, 2}
///

-- length
doc ///
  Key
    (length, Permutation)
  Headline
    computes the length of a permutation
  Usage
    length w
  Inputs
    w:Permutation
  Outputs
    :ZZ
  Description
    Text
      The {\em length} of a permutation is the size of its inversion set.
    Example
      p = permutation {3,1,2,5,4}
      length p
  SeeAlso
    inversions
///

-- (matrix, Permutation)
doc ///
  Key
    (matrix, Permutation)
  Headline
    computes the matrix representation of a permutation
  Usage
    matrix w
  Inputs
    w:Permutation
  Outputs
    :Matrix
  Description
    Text
      Every permutation $p$ on $n$ symbols can be written as an $n \times n$ 
      matrix. Its matrix representation has a $1$ in entry $(i,j)$ if $p(i) = j$
      and $0$ otherwise.
    Example
      p = permutation {3,1,2,5,4}
      matrix p
  SeeAlso
    (symbol *, Permutation, Matrix)
    (symbol *, Matrix, Permutation)
///

-- ord
doc ///
  Key
    ord
    (ord, Permutation)
  Headline
    computes the order of a permutation
  Usage
    ord w
  Inputs
    w:Permutation
  Outputs
    :ZZ
  Description
    Text
      The {\em order} of a permutation $p$ is the smallest positive integer 
      $n$ such that $p^n$ is the identity permutation, i.e., its order in the 
      symmetric group.
    Example
      p = permutation {3,1,2,5,4}
      ord p
///

-- randomPermutation
doc ///
  Key
    randomPermutation
    (randomPermutation, ZZ)
  Headline
    generates a random permutation
  Usage
    randomPermutation n
  Inputs
    n:ZZ
  Outputs
    :Permutation
  Description
    Text
      Generates a random permutation on $1..n$.
    Example
      randomPermutation 5
///

-- records
doc ///
  Key
    records
    (records, Permutation)
  Headline
    computes the saliances of a permutation
  Usage
    records w
  Inputs
    w:Permutation
  Outputs
    :List
  Description
    Text
      A permutation $p$ has a {\em record} at $i$ if $p(j) < p(i)$ for all $j < i$.
    Example
      p = permutation {3,1,2,5,4}
      saliances p
  SeeAlso
    ascendingRuns
    ascents
    descendingRuns
    descents
    exceedances
    saliances
///

-- reducedWords
doc ///
  Key
    reducedWords
    (reducedWords, Permutation)
  Headline
    computes the reduced words of a permutation
  Usage
    reducedWords w
  Inputs
    w:Permutation
  Outputs
    :List
  Description
    Text
      A permutation $p$ can be expressed as a product of transpositions.
      A {\em reduced word} is a minimal-length expression of a permutation as a
      product of transpositions. The {\tt reducedWords} method computes all of 
      the reduced words of a permutation. A word is represented by a list of 
      integers, where $i$ denotes the transposition $(i,i+1)$.
    Example
      p = permutation {4,3,2,1}
      words = reducedWords p
    Text
      We can check that a reduced word multiplies out to the original permutation.
      Note that we need to multiply from right to left, so we must reverse the 
      list.
    Example
      word = words#0
      product reverse apply(word, i -> transposition i)
  SeeAlso
    length
    transposition
///

-- saliances
doc ///
  Key
    saliances
    (saliances, Permutation)
  Headline
    computes the saliances of a permutation
  Usage
    saliances w
  Inputs
    w:Permutation
  Outputs
    :List
  Description
    Text
      A permutation $p$ has a {\em saliance} at $i$ if $p(j) < p(i)$ for all $j > i$.
    Example
      p = permutation {3,1,2,5,4}
      saliances p
  SeeAlso
    ascendingRuns
    ascents
    descendingRuns
    descents
    exceedances
    records
///

-- (sign, Permutation)
doc ///
  Key
    (sign, Permutation)
  Headline
    computes the sign of a permutation
  Usage
    sign w
  Inputs
    w:Permutation
  Outputs
    :ZZ
  Description
    Text
      Every permutation can be written as a product of transpositions. One definition for the {\em sign}
      of a permutation $p$ is $1$ if it can be written as a product of an even number of transpositions
      and is $-1$ if it can be written as an odd number of transpositions.
    Example
      p = permutation {3,1,2,5,4}
      sign p
  SeeAlso
    cycleDecomposition
    cycleType
    isEven
    isOdd
///

-- strongBruhatOrder
doc ///
  Key
    strongBruhatOrder
    (strongBruhatOrder, Permutation, Permutation)
  Headline
    compares two permutations in the (strong) Bruhat order
  Usage
    strongBruhatOrder(w,v)
  Inputs
    w:Permutation
    v:Permutation
  Outputs
    :Boolean
  Description
    Text
      The {\em (strong) Bruhat order} is a partial order on the symmetric group
      $\mathfrak{S}_n$. See [BB05] for more details on the strong Bruhat order.
    Example
      p = permutation {3,5,1,2,4}
      q = permutation {4,5,1,2,3}
      strongBruhatOrder(p, q)
  References
    @UL{
      {
        "[BB05] Anders Björner and Francesco Brenti, ",
        HREF("https://link.springer.com/book/10.1007/3-540-27596-7", EM "Combinatorics of Coxeter groups"), 
        ", Graduate Texts in Mathematics, 231, Springer, New York, 2005"
      }
    }@
  SeeAlso
    weakBruhatOrder
    symmetricGroupPoset
///

-- symmetricGroupPoset
doc ///
  Key
    symmetricGroupPoset
    (symmetricGroupPoset, ZZ, Function)
  Headline
    constructs the poset of the symmetric group under a partial order
  Usage
    symmetricGroupPoset(n,f)
  Inputs
    w:Permutation
    f:Function
  Outputs
    :Boolean
  Description
    Text
      Using the @TO "Posets"@ package, we can construct the poset on the
      symmetric group $\mathfrak{S}_n$ under a partial order. See
      [BB05] for more details on Bruhat order.
    Example
      symmetricGroupPoset(4, strongBruhatOrder)
    Text
      With a little bit of finagling, we can also construct the poset using 
      the non-default {\em left} weak Bruhat order.
    Example
      partialOrder = (w,v) -> weakBruhatOrder(w,v, Side=>"left")
      symmetricGroupPoset(4, partialOrder)
  SeeAlso
    weakBruhatOrder
    strongBruhatOrder
///

doc ///
  Key
    transposition
    (transposition, ZZ)
    (transposition, ZZ, ZZ)
  Headline
    constructs a transposition
  Usage
    transposition i
  Inputs
    i:ZZ
    j:ZZ
  Outputs
    :Permutation
  Description
    Text
      This method constructs the transposition $(i,i+1)$.
    Example
      transposition 3
    Text
      When two arguments are passed, this constructs the transposition $(i,j)$.
    Example
      transposition(3,5)
///

-- trim
doc ///
  Key
    trim
    (trim, Permutation)
  Headline
    rewrites a permutation in its smallest representation
  Usage
    trim w
  Inputs
    w:Permutation
  Outputs
    :Permutation
  Description
    Text
      {\tt trim} rewrites a permutation $p$ as a permutation in $S_n$,
      where $n$ is the smallest integer such that $p$ is in $\mathfrak{S}_n$.
      In other words, it returns a permutation where any extraneous fixed points
      are removed. 
    Example
      p = permutation {3,1,2,5,4,6,7}
      trim p
  SeeAlso
    extend
///

-- weakBruhatOrder
doc ///
  Key
    weakBruhatOrder
    (weakBruhatOrder, Permutation, Permutation)
    [weakBruhatOrder, Side]
    Side
  Headline
    compares two permutations in the weak Bruhat order
  Usage
    weakBruhatOrder(w,v)
  Inputs
    w:Permutation
    v:Permutation
    Side:String
      an optional argument to specify the side of the Bruhat order
  Outputs
    :Boolean
  Description
    Text
      The (right) weak Bruhat order is a partial order on the symmetric group 
      $\mathfrak{S}_n$. For two permutations $p$ and $q$, $w \leq_R v$ if and 
      only if $\ell(w) + \ell(v^{-1} w) = \ell(v)$, where $\ell$ denotes the
      @TO (length, Permutation)@ of a permutation and $\leq_R$ is the right 
      weak Bruhat order. See [BB05] for more details on the weak Bruhat order.
    Example
      p = permutation {1,3,2}
      q = permutation {2,1,3}
      r = permutation {3,1,2}
      weakBruhatOrder(p, r)
      weakBruhatOrder(q, r)
    Text
      By default, {\tt weakBruhatOrder} computes the {\em right}
      weak Bruhat order, but the optional argument {tt Side} can be used to 
      compute, for example, the {\em left} weak Bruhat order.
      The current options for {\tt Side} are {\tt "left"} and {\tt "right"}.
    Example
      p = permutation {1,3,2}
      r = permutation {3,1,2}
      weakBruhatOrder(inverse p, inverse r, Side=>"left")
  References
    @UL{
      {
        "[BB05] Anders Björner and Francesco Brenti, ",
        HREF("https://link.springer.com/book/10.1007/3-540-27596-7", EM "Combinatorics of Coxeter groups"), 
        ", Graduate Texts in Mathematics, 231, Springer, New York, 2005"
      }
    }@
  SeeAlso
    (length, Permutation)
    strongBruhatOrder
    symmetricGroupPoset
///