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