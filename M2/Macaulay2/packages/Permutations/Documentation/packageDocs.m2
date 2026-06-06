-- Package home page
doc ///
  Key
    "Permutations"
  Headline
    a package which implements permutations
  Description
    Text
      This package defines the @TO Permutation@ type. An overview of the package's
      can be found in the @TO "Permutations Guide"@.
///

-- Permutations Guide
doc ///
  Key
    "Permutations Guide"
  Headline
    a detailed overview of permutations in Macaulay2
  Description
    Text
      This page gives an overview of the use of permutations.

      The sections of the overview are, in order:
    Text
      @OL {
        TO "Creating permutations",
        TO "Basic operations",
        TO "Group actions",
        TO "Cyclic decomposition",
        TO "Ascents, descents, runs, exceedances, and records",
        TO "Pattern avoidance",
        TO "Bruhat order",
        TO "Miscellaneous",
      }@
///

-- Creating permutations
doc ///
  Key
    "Creating permutations"
  Headline
    an overview of creating permutations
  Description
    Text
      Permutations are constructed from lists. To create a {\bf permutation}, 
      use the @TO permutation@ method:
    Example
      p = permutation {3,1,2,4,5}
    Text
      Permutations must be constructed from lists consisting of only the integers
      $1 \textemdash n$. If a list contains any other elements or does not consist of the entire
      range, then an error is thrown.
      The method @TO (matrix, Permutation)@ can be used to get the matrix representation of the
      permutation. In this representation, for a permutation $p$, if $p(i)=j$, then
      then the $(i,j)$th entry is $1$.
    Example
      p = permutation {3,1,2,4,5}
      matrix p
    Text
      This is especially useful for considering the action of permutations on matrices,
      see {\bf Group actions}.
///

-- Basic operations
doc ///
  Key
    "Basic operations"
  Headline
    an overview of basic operations with permutations
  Description
    Text
      Two permutations $p$ and $q$ are equal if they are equal as functions, i.e., 
      if $p(i) = q(i)$ for all $i$.
    Example
      p = permutation {3,1,2}
      q = permutation {3,1,2,4,5}
      p == q
    Text
      We can also multiply (or compose) two permutations. We follow the convention 
      of $(p*q)(i) = p(q(i))$.
    Example
      p = permutation {3,1,2,5,4}
      q = permutation {2,1,3,4,5}
      p*q
    Text
      This also let's us compute powers of permutations.
    Example
      p = permutation {3,1,2,5,4}
      p^2
      p^6
      p^0
      p^(-1)
      p^(-2)
///

-- Group actions
doc ///
  Key
    "Group actions"
  Headline
    an overview of permutations acting on objects in Macaulay2
  Description
    Text
      A permutation on $n$ symbols acts on lists of size $n$ by permuting its contents 
      according to the permutation.
    Example
      p = permutation {3,1,2,5,4};
      L = {1,2,3,4,5};
      p*L
    Text
      A permutation $p$ on $n$ symbols can also be regarded as a permutation on $N$ 
      symbols for any $N \geq n$ by regarding all of the indices $n+1, n+2, \dots$
      as fixed by $p$, i.e., $p(i)=i$ for all $i > n$. This is also reflected in the 
      group action.
    Example
      p = permutation {3,1,2,5,4};
      L = {1,2,3,4,5,6,7,8};
      p*L
    Text
      In a similar manner, permutations on $n$ symbols also act on the space of 
      $m \times n$ matrices by permuting the rows (or columns). Another way to 
      view this action is via the multiplication on the left by the matrix representation
      of the permutation (if acting on the rows).
    Example
      p = permutation {3,1,2};
      M = id_(ZZ^3);
      p*M
    Text
      Just as in the case of lists, the size of the matrix can be larger than $n$.
      Example
      p = permutation {3,1,2};
      M = id_(ZZ^5);
      p*M
    Text
      The matrix does not need to be square. As long as the number of rows is 
      greater than or equal largest non-fixed point of the permutation, the action
      is valid.
    Example
      p = permutation {3,1,2};
      M = id_(ZZ^3) || id_(ZZ^3);
      N = matrix {{1,0},{0,1},{1,1}};
      p*M
      p*N
    Text
      We can also act on the columns of the matrix in a similar way.
    Example
      p = permutation {3,1,2};
      M = id_(ZZ^3) || id_(ZZ^3);
      N = matrix {{1,0,0,0},{0,1,0,0},{0,0,1,0}};
      M*p
      N*p
///

-- Cyclic decomposition
doc ///
  Key
    "Cyclic decomposition"
  Headline
    an overview of cyclic decompositions of permutations
  Description
    Text
      Every permutation can be decomposed as a product of disjoint cycles.
      This can be computed with @TO cycleDecomposition@.
    Example
      p = permutation {3,1,2,5,4}
      cycleDecomposition p
    Text
      We follow the convention to write the decomposition in its "canonical" or 
      "standard" form. So

      1. each cycle is listed with its largest element first, and @BR{}@
      2. the cycles are listed in increasing order.

      A permutation's {\em cycle type} is the sequence consisting of the lengths of the 
      cycles in its cycle decomposition, listed in weakly decreasing order.
    Example
      cycleType p
    Text
      @SUBSECTION "Foata's fundamental bijection"@
    Text
      Foata's fundamental bijection is a bijection between a permutation's standard 
      cycle decomposition and another permutation read the same (in one-line notation)
      as the decomposition with its parentheses removed. For example, if $p = (3 \, 2 \, 1)(5 \, 4)$
      (written in cycle notation), then its corresponding permutation (written in one-line 
      notation) is $\hat{p} = (3 \, 2 \, 1 \, 5 \, 4)$.
    Example
      p = permutation {3,1,2,5,4};
      foataBijection p
    Text
      @SUBSECTION "Decomposition into transpositions"@
    Text
      A permutation can also be decomposed as a product of transpositions.
      When this is done minimally (with respect to the number of transpositions 
      used), such decompositions are called {\em reduced words}. We can compute 
      these with @TO reducedWords@.
    Example
      p = permutation {3,1,2,5,4};
      reducedWords p
    Text
      We can verify that these reduced words multiply to the original permutation.
    Example
      all(reducedWords p, word -> product reverse apply(word, transposition) == p)
///

-- Ascents, descents, runs, exceedances, and records
doc ///
  Key
    "Ascents, descents, runs, exceedances, and records"
  Headline
    a detailed overview of permutations in Macaulay2
  Description
    Text
      A permutation $p=(p_1 \, \dots \, p_n)$ has an {\em ascent} at $i$ (for $i < n$)
      if $p(i) < p(i+1)$. Similarly, it has a {\em descent} at $i$ (for $i < n$) if 
      $p(i) > p(i+1)$. We can compute the set of ascents and the set of descents using
      @TO ascents@ and @TO descents@, respectively.
    Example
      p = permutation {3,1,2,5,4};
      ascents p
      descents p
    Text
      An {\em ascending run} is a maximal subsequence of successive ascents.
      Similarly, a {\em descending run} is a maximal subsequence of successive 
      descents.
    Example
      p = permutation {3,1,2,5,4};
      ascendingRuns p
      descendingRuns p
    Text
      A permutation $p=(p_1 \, \dots \, p_n)$ has an {\em exceedance} at $i$ if 
      $p(i) > i$; this is called a {\em weak exceedance} if the inequality is not 
      strict, i.e., $p(i) \geq i$.
    Example
      p = permutation {3,1,2,5,4};
      exceedances p
    Text
      A permutation $p$ has a {\em saliance} at $i$ if $p(j) < p(i)$ for all $j > i$.
    Example
      p = permutation {3,1,2,5,4};
      saliances p
    Text
      A permutation $p$ has a {\em record} at $i$ if $p(j) < p(i)$ for all $j < i$.
    Example
      p = permutation {3,1,2,5,4};
      records p
///

-- Pattern avoidance
doc ///
  Key
    "Pattern avoidance"
  Headline
    an overview of pattern avoidance for permutations in Macaulay2
  Description
    Text
      We can check if a permutation avoids a pattern.
      For example, a permutation $p$ is $2143$-avoiding if there do not exist 
      indices $i < j < k < l$ such that $w_j < w_i < w_l < w_k$.
    Example
      p = permutation {3,1,2,5,4};
      avoidsPattern(p, {2,1,4,3})
    Text
      Some patterns are common enough that those pattern-avoiding permutations
      are given a name, such as {\em vexillary} for those that are $2143$-avoiding.
    Example
      p = permutation {3,1,2,5,4};
      isVexillary p
    Text
      We can also check if a permutation simultaneously avoids a list of patterns.
    Example
      p = permutation {3,1,2,5,4};
      avoidsPatterns(p, {{2,1,4,3},{1,4,3,2}})
    Text
      Just as before, some lists of patterns are common enough to be given names.
      See the documentation for @TO isCartwrightSturmfels@, @TO isCDG@, and 
      @TO isSeparable@ for such lists of patterns.
    Example
      p = permutation {3,1,2,5,4};
      isCartwrightSturmfels p
      isCDG p
///

-- Bruhat order
doc ///
  Key
    "Bruhat order"
  Headline
    an overview of the Bruhat order for permutations
  Description
    Text
      The {\em (strong) Bruhat order} is a partial order on the symmetric group
      $\mathfrak{S}_n$. See [BB05] for more details on Bruhat order.
    Example
      p = permutation {3,5,1,2,4}
      q = permutation {4,5,1,2,3}
      strongBruhatOrder(p, q)
    Text
      The (right) weak Bruhat order is a partial order on the symmetric group 
      $\mathfrak{S}_n$. For two permutations $p$ and $q$, $w \leq_R v$ if and 
      only if $\ell(w) + \ell(v^{-1} w) = \ell(v)$, where $\ell$ denotes the
      @TO (length, Permutation)@ of a permutation and $\leq_R$ is the right 
      weak Bruhat order.
    Example
      p = permutation {1,3,2}
      q = permutation {3,1,2}
      weakBruhatOrder(p, q)
    Text
      The optional argument @TO Side@ can be used to specify which weak Bruhat
      order to use. The current options are "left" and "right" (default).
    Example
      weakBruhatOrder(p, q, Side=>"left")
    Text
      We can use these orders to construct the poset of $\mathfrak{S}_n$ using
      the @TO "Posets"@ package. The @TO symmetricGroupPoset@ method constructs
      the poset of $\mathfrak{S}_n$ with the Bruhat order for convenience.
    Example
      P = symmetricGroupPoset(4, strongBruhatOrder)
      Q = symmetricGroupPoset(4, weakBruhatOrder)
    Text
      This allows us to verify, for example, that the weak Bruhat order is 
      rank-symmetric and Sperner [GG20].
    Example
      rankPoly = rankGeneratingFunction Q
      coeffs = apply(flatten entries (coefficients rankPoly)#1, k -> sub(k, ZZ))
      d = (degree rankPoly)#0
      all(apply(#coeffs // 2, i -> coeffs#i == coeffs#(d-i)))
      isSperner P
  References
    @UL{
      {
        "[BB05] Anders Björner and Francesco Brenti, ",
        HREF("https://link.springer.com/book/10.1007/3-540-27596-7", EM "Combinatorics of Coxeter groups"), 
        ", Graduate Texts in Mathematics, 231, Springer, New York, 2005"
      },
      {
        "[GG20] Christian Gaetz and Yibo Gao, ",
        EM "The weak Bruhat order on the symmetric group is Sperner",
        ", Sém. Lothar. Combin. ",
        EM "82B",
        " (2020), Art. 35, 8 pp."
      }
    }@
///

-- Miscellaneous
doc ///
  Key
    "Miscellaneous"
  Headline
    a detailed overview of permutations in Macaulay2
  Description
    Text
      We can compute the {\em inverse} of a permutation.
    Example
      p = permutation {3,1,2,5,4};
      inverse p
    Text
      The {\em order} of a permutation $p$ is its order in the symmetric group $\mathfrak{S}_n$, i.e.,
      the smallest positive integer $k$ such that $p^k$ is the identity permutation.
    Example
      p = permutation {3,1,2,5,4};
      ord p
    Text
      Every permutation can be written as a product of transpositions. One definition for the {\em sign}
      of a permutation $p$ is $1$ if it can be written as a product of an even number of transpositions
      and is $-1$ if it can be written as an odd number of transpositions. If $\text{sign}(p) = 1$,
      the permutation is called {\em even} and if $\text{sign}(p) = -1 $, it is called {\em pdd}.
    Example
      p = permutation {3,1,2,5,4};
      sign p
      isEven p
      isOdd p
    Text
      A permutation $p$ is a {\em derangement} if $p(i) \neq i$ for all $i$.
      We can determine if $p$ is a derangement as well its fixed points.
    Example
      p = permutation {3,1,2,5,4};
      isDerangement p
      fixedPoints p
    Text
      A permutation $p$ has an inversion $(i,j)$ if $i < j$ and $p(i) > p(j)$.
      We can compute all the inversions of a permutation.
    Example
      p = permutation {3,1,2,5,4};
      inversions p
///