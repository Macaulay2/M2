-- Sean Grate

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

doc ///
 Key
  "Permutations Guide"
 Headline
  a detailed overview of permutations in Macaulay2
 Description
  Text
   This page gives an overview of the use of permutations.

   The sections of the overview are, in order:
   
   {\bf Creating permutations.} @BR{}@
   {\bf Indexing.} @BR{}@
   {\bf Basic operations.} @BR{}@
   {\bf Group actions.} @BR{}@
   {\bf Cyclic Decompositions.} @BR{}@
   {\bf Ascents, descents, runs, exceedances, and records.} @BR{}@
   {\bf Pattern avoidance.} @BR{}@
   {\bf Foata's fundamental bijection.} @BR{}@
   {\bf Miscellaneous.}
   
   Links to individual documentation pages for the functions 
   described in this article are collected in an alphabetized list 
   at the very end of the page.
   
   
   {\bf Creating permutations.}
   
   Permutations are constructed from lists. To create a {\bf permutation}, 
   use the {\tt permutation} method:
  Example
   p = permutation {3,1,2,4,5}
  Text
   Permutations must be constructed from lists consisting of only the integers
   $1 \textemdash n$. If a list contains any other elements or does not consist of the entire
   range, then an error is thrown.
   The method @TO matrix@ can be used to get the matrix representation of the
   permutation. In this representation, for a permutation $p$, if $p(i)=j$, then
   then the $(i,j)$th entry is $1$.
  Example
   p = permutation {3,1,2,4,5}
   matrix p
  Text
   This is especially useful for considering the action of permutations on matrices,
   see {\bf Group actions}.
  
  --
  Text

   {\bf Basic operations.}

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

  --
  Text

   {\bf Group actions.}

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

  --
  Text

   {\bf Cycle decompositions.}

   Every permutation can be decomposed as a product of disjoint cycles.
   This can be computed with {\tt cycleDecomposition}.
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

  --
  Text

   {\bf Ascents, descents, runs, exceedances, saliances, and records.}

   A permutation $p=(p_1 \, \dots \, p_n)$ has an {\em ascent} at $i$ (for $i < n$)
   if $p(i) < p(i+1)$. Similarly, it has a {\em descent} at $i$ (for $i < n$) if 
   $p(i) > p(i+1)$. We can compute the set of ascents and the set of descents using
   {\tt ascents} and {\tt descents}, respectively.
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

  --
  Text

   {\bf Pattern avoidance.}

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
   See the documentation for @TO isCartwrightSturmfels@ and @TO isCDG@ for such 
   lists of patterns.
  Example
   p = permutation {3,1,2,5,4};
   isCartwrightSturmfels p
   isCDG p

  --
  Text

   {\bf Foata's fundamental bijection.}

   Foata's fundamental bijection is a bijection between a permutation's standard 
   cycle decomposition and another permutation read the same (in one-line notation)
   as the decomposition with its parentheses removed. For example, if $p = (3 \, 2 \, 1)(5 \, 4)$
   (written in cycle notation), then its corresponding permutation (written in one-line 
   notation) is $\hat{p} = (3 \, 2 \, 1 \, 5 \, 4)$.
  Example
   p = permutation {3,1,2,5,4};
   foataBijection p

  --
  Text

   {\bf Miscellaneous.}

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
 SeeAlso
  (symbol _, Permutation, ZZ)
  (symbol _, Permutation, List)
  (symbol _, Permutation, Sequence)
  (symbol _, VisibleList, Permutation)
  (symbol ==, Permutation, Permutation)
  (symbol *, Permutation, VisibleList)
  (symbol *, Permutation, Matrix)
  (symbol *, Matrix, Permutation)
  (symbol _, Matrix, Permutation)
  (symbol ^, Matrix, Permutation)
  (symbol *, Permutation, Permutation)
  (symbol ^, Permutation, ZZ)
  ascendingRuns
  ascents
  avoidsPattern
  avoidsPatterns
  cycleDecomposition
  cycleType
  descendingRuns
  descents
  exceedances
  extend
  fixedPoints
  foataBijection
  inverse
  inversions
  isCartwrightSturmfels
  isCDG
  isDerangement
  isEven
  isOdd
  isWellDefined
  isVexillary
  ord
  records
  trim
  saliances
  sign
  (matrix, Permutation)
///

-- Permutation
-- (permutation, VisibleList)
doc ///
  Key
    Permutation
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

-- (symbol _, Permutation, ZZ)
doc ///
  Key
    (symbol _, Permutation, ZZ)
  Headline
    selects an element from the permutation when regarded as a list
  Usage
    w_n
  Inputs
    w:Permutation
    n:ZZ
  Outputs
    :ZZ
  Description
    Text
      Selects an element from the permutation when it is regarded as a list.
      The index should be 1-indexed.

      Given a permutation $p$ and index $i$, this is the same as $p(i)$.
    Example
      p = permutation {3,1,2,5,4}
      p_1
///

-- (symbol _, Permutation, List)
doc ///
  Key
    (symbol _, Permutation, List)
  Headline
    selects a subset of the permutation when regarded as a list
  Usage
    w_l
  Inputs
    w:Permutation
    l:List
  Outputs
    :List
  Description
    Text
      Selects a subset of the permutation when it is regarded as a list.
      It is important to note that the output may not be a valid
      permutation.

      The indices should be 0-indexed.
    Example
      p = permutation {3,1,2,5,4}
      p_{1,2}
  SeeAlso
    (symbol _, Permutation, Sequence)
    (symbol _, Permutation, ZZ)
///

-- (symbol _, Permutation, Sequence)
doc ///
  Key
    (symbol _, Permutation, Sequence)
  Headline
    selects a subset of the permutation when regarded as a list
  Usage
    w_s
  Inputs
    w:Permutation
    s:Sequence
  Outputs
    :List
  Description
    Text
      Selects a subset of the permutation when it is regarded as a list.
      It is important to note that the output may not be a valid
      permutation.

      The indices should be 0-indexed.
    Example
      p = permutation {3,1,2,5,4}
      p_(1,2)
  SeeAlso
    (symbol _, Permutation, List)
    (symbol _, Permutation, ZZ)
///

-- (symbol ==, Permutation, Permutation)
doc ///
  Key
    (symbol ==, Permutation, Permutation)
  Headline
    whether two permutations are the same
  Usage
    w == v
  Inputs
    w:Permutation
    v:Permutation
  Outputs
    :Boolean
  Description
    Text
      Two permutations $p$ and $q$ are equal if $p(i) = q(i)$ for all $i$.
    Example
      p = permutation {3,1,2,5,4}
      q = permutation {3,1,2,5,4,6,7}
      p == q
    Text
      The permutations do not need to be the same length.
    Example
      p = permutation {3,1,2,4}
      q = permutation {3,1,2,4,5,6}
      p == q
///

-- (symbol *, Permutation, VisibleList)
doc ///
  Key
    (symbol *, Permutation, VisibleList)
  Headline
    computes the action of a permutation on a list
  Usage
    w * l
  Inputs
    w:Permutation
    l:VisibleList
  Outputs
    :VisibleList
  Description
    Text
      A permutation $p$ acts on the elements of list by permuting the elements
      of the list according to the permutation. More precisely, if 
      $L = \{ e_1, \dots, e_k \}$ is a list and $p=(p_1, \dots, p_n)$ is a 
      permutation, then the action is given by $p*L = \{ e_{p(1)}, \dots, e_{p(k)}}$.

      The permutation cannot permute more than {\tt #l} elements. 
    Example
      p = permutation {3,1,2,5,4,7,6}
      L = {3,1,2,5,4,6,7}
      p * L
    Text
      The permutation can be a permutation on less than {\tt #l} elements.
    Example
      p = permutation {3,1,2,4,5}
      L = {3,1,2,5,4,6,7}
      p * L
  SeeAlso
    (symbol _, VisibleList, Permutation)
///

-- (symbol _, VisibleList, Permutation)
doc ///
  Key
    (symbol _, VisibleList, Permutation)
  Headline
    computes the action of a permutation on a list
  Usage
    l_p
  Inputs
    l:VisibleList
    w:Permutation
  Outputs
    :VisibleList
  Description
    Text
      A permutation $p$ acts on the elements of list by permuting the elements
      of the list according to the permutation. More precisely, if 
      $L = \{ e_1, \dots, e_k \}$ is a list and $p=(p_1, \dots, p_n)$ is a 
      permutation, then the action is given by $p*L = \{ e_{p(1)}, \dots, e_{p(k)}}$.

      The permutation cannot permute more than {\tt #l} elements. 
    Example
      p = permutation {3,1,2,5,4,7,6}
      L = {3,1,2,5,4,6,7}
      L_p
    Text
      The permutation can be a permutation on less than {\tt #l} elements.
    Example
      p = permutation {3,1,2,4,5}
      L = {3,1,2,5,4,6,7}
      L_p
  SeeAlso
    (symbol *, Permutation, VisibleList)
///

-- (symbol *, Permutation, Matrix)
doc ///
  Key
    (symbol *, Permutation, Matrix)
  Headline
    computes the action of a permutation on the rows of a matrix
  Usage
    w * M
  Inputs
    w:Permutation
    M:Matrix
  Outputs
    :Matrix
  Description
    Text
      A permutation $p$ acts on the space of $n \times n$ matrices by permuting
      the rows of the matrix according to the permutation.

      The permutation cannot permute more than {\tt numRows M} elements. 
    Example
      p = permutation {3,1,2}
      A = matrix {{1,2,3},{4,5,6},{7,8,9}}
      p * A
    Text
      The permutation can be a permutation on less than {\tt numRows M} elements.
    Example
      p = permutation {2,1}
      A = matrix {{1,2},{3,4},{5,6}}
      p * A
  SeeAlso
    (symbol _, Matrix, Permutation)
///

-- (symbol _, Matrix, Permutation)
doc ///
  Key
    (symbol _, Matrix, Permutation)
  Headline
    computes the action of a permutation on the rows of a matrix
  Usage
    w * M
  Inputs
    M:Matrix
    w:Permutation
  Outputs
    :Matrix
  Description
    Text
      A permutation $p \in \mathfrak{S}_m$ acts on the space of $m \times n$ matrices by permuting
      the rows of the matrix according to the permutation.

      The permutation cannot permute more than {\tt numRows M} elements. 
    Example
      p = permutation {3,1,2}
      A = matrix {{1,2,3},{4,5,6},{7,8,9}}
      p * A
    Text
      The permutation can be a permutation on less than {\tt numRows M} elements.
    Example
      p = permutation {2,1}
      A = matrix {{1,2},{3,4},{5,6}}
      A_p
  SeeAlso
    (symbol *, Permutation, Matrix)
///

-- (symbol *, Matrix, Permutation)
doc ///
  Key
    (symbol *, Matrix, Permutation)
  Headline
    computes the action of a permutation on the columns of a matrix
  Usage
    M * w
  Inputs
    M:Matrix
    w:Permutation
  Outputs
    :Matrix
  Description
    Text
      A permutation $p \in \mathfrak{S}_n$ acts on the space of $m \times n$ matrices by permuting
      the columns of the matrix according to the permutation.

      The permutation cannot permute more than {\tt numColumns M} elements. 
    Example
      p = permutation {3,1,2}
      A = matrix {{1,2,3},{4,5,6},{7,8,9}}
      A * p
    Text
      The permutation can be a permutation on less than {\tt numColumns M} elements.
    Example
      p = permutation {2,1}
      A = matrix {{1,2,3},{4,5,6}}
      A * p
  SeeAlso
    (symbol ^, Matrix, Permutation)
///

-- (symbol ^, Matrix, Permutation)
doc ///
  Key
    (symbol ^, Matrix, Permutation)
  Headline
    computes the action of a permutation on a matrix
  Usage
    M^w
  Inputs
    M:Matrix
    w:Permutation
  Outputs
    :Matrix
  Description
    Text
      A permutation $p \in \mathfrak{S}_n$ acts on the space of $m \times n$ matrices by permuting
      the columns of the matrix according to the permutation.

      The permutation cannot permute more than {\tt numColumns M} elements. 
    Example
      p = permutation {3,1,2}
      A = matrix {{1,2,3},{4,5,6},{7,8,9}}
      A^p
    Text
      The permutation can be a permutation on less than {\tt numColumns M} elements.
    Example
      p = permutation {2,1}
      A = matrix {{1,2,3},{4,5,6}}
      A^p
  SeeAlso
    (symbol *, Matrix, Permutation)
///

-- (symbol *, Permutation, Permutation)
doc ///
  Key
    (symbol *, Permutation, Permutation)
  Headline
    computes the product of two permutations
  Usage
    w * v
  Inputs
    w:Permutation
    v:Permutation
  Outputs
    :Permutation
  Description
    Text
      The product of two permutations $p$ and $q$ is given by their composition,
      i.e., $ (p \circ q)(i) = p(q(i))$.
    Example
      p = permutation {3,1,2,5,4}
      q = permutation {2,1,3,4,5}
      p * q
    Text
      The two permutations do not need to be the same length.
    Example
      p = permutation {3,1,2,5,4}
      q = permutation {2,1,3}
      p * q
///

-- (symbol ^, Permutation, ZZ)
doc ///
  Key
    (symbol ^, Permutation, ZZ)
  Headline
    computes the power of a permutation
  Usage
    w^n
  Inputs
    w:Permutation
    n:ZZ
  Outputs
    :Permutation
  Description
    Text
      Computes the power of a permutation. The power can be any integer.
    Example
      p = permutation {3,1,2,5,4}
      p^2
      p^0
      p^(-3)
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

-- avoidsPattern
doc ///
  Key
    avoidsPattern
    (avoidsPattern, Permutation, List)
  Headline
    whether a permutation avoids a pattern
  Usage
    avoidsPattern(w, pattern)
  Inputs
    w:Permutation
    pattern:List
  Outputs
    isAvoiding:Boolean
  Description
    Text
      Pattern avoidance is more easily understood through an example.
      A permutation $p$ is $2143$-avoiding if there do not exist indices 
      $i < j < k < l$ such that $w_j < w_i < w_l < w_k$.
    Example
      p = permutation {3,1,2,5,4}
      avoidsPattern(p, {2,1,4,3})
  SeeAlso
    avoidsPatterns
    isCartwrightSturmfels
    isCDG
    isVexillary
///

-- avoidsPatterns
doc ///
  Key
    avoidsPatterns
    (avoidsPatterns, Permutation, List)
  Headline
    whether a permutation simultaneously avoids a list of patterns
  Usage
    avoidsPatterns(w, patterns)
  Inputs
    w:Permutation
    patterns:List
  Outputs
    :Boolean
  Description
    Text
      See @TO avoidsPattern@ for more information on pattern avoidance.
    Example
      p = permutation {3,1,2,5,4}
      avoidsPatterns(p, {{2,1,4,3}, {1,4,3,2}})
  SeeAlso
    avoidsPattern
    isCartwrightSturmfels
    isCDG
    isVexillary
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
-- TODO: how to document optional inputs?
doc ///
  Key
    exceedances
    (exceedances, Permutation)
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
    rewrites a permutation as a permution on more symbols
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

-- isCartwrightSturmfels
-- TODO: add list of patterns to description
doc ///
  Key
    isCartwrightSturmfels
    (isCartwrightSturmfels, Permutation)
  Headline
    whether a permutation is Cartwright-Sturmfels
  Usage
    isVexillary w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is {\em Cartwright-Sturmfels} if it avoids.
    Example
      p = permutation {3,1,2,5,4}
      isCartwrightSturmfels p
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCDG
    isVexillary
///

-- isCDG
-- TODO: add CDG spelled out to headline and description
-- TODO add list of patterns to description
doc ///
  Key
    isCDG
    (isCDG, Permutation)
  Headline
    whether a permutation is CDG.
  Usage
    isCDG w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is {\em CDG} if it avoids.
    Example
      p = permutation {3,1,2,5,4}
      isCDG p
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCartwrightSturmfels
    isVexillary
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
    sign
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
    sign
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

-- isVexillary
doc ///
  Key
    isVexillary
    (isVexillary, Permutation)
  Headline
    whether a permutation is vexillary
  Usage
    isVexillary w
  Inputs
    w:Permutation
  Outputs
    :Boolean
  Description
    Text
      A permutation $p$ is {\em vexillary} if it is $2143$-avoiding.
    Example
      p = permutation {3,1,2,5,4}
      isVexillary p
  SeeAlso
    avoidsPattern
    avoidsPatterns
    isCartwrightSturmfels
    isCDG
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

-- sign
doc ///
  Key
    sign
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
