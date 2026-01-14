
doc ///
Node
  Key
    Tableaux
  Headline
    a package for computing with Young tableaux
  Description
    Text
      This package provides the classes @TO YoungTableau@, @TO MutableYoungTableau@, and @TO Tabloid@. These classes
      can be used to construct (skew) tableaux, and entries in the boxes may be any class. See the constructor's
      page @TO youngTableau@ for some basics of this class.
///

doc ///
Key
    MutableYoungTableau
Headline
    a type of HashTable representing a Young tableau, with mutable entries
Description
  Text
    An object of type MutableYoungTableau is a hash table containing two shapes of type @TO Partition@,
    and a mutable list of box entries. The entries may have any type, except for @TO null@ objects.

    The inner and outer shapes, $\lambda$ and $\mu$ respectively, can be any sequence of integers. In particular,
    it accepts negative parts, compositions, rows where $\lambda_i < \mu_i$, and compositions where
    $\ell(\lambda)\neq\ell(\mu)$. 
  Example
    lam = new Partition from {4,3,2}
    mu = new Partition from {3,1}
    entryList = {1,2,3,3,9}
    T = mutableYoungTableau(lam,mu,entryList)
    T_(1,1) = 8
    T
SeeAlso
    mutableYoungTableau
///

doc ///
Key
    Tabloid
Headline
    a type of HashTable representing a Young tabloid
Description
  Text
    An object of type Tabloid is a subclass of type @TO YoungTableau@. A tabloid represents an equivalence class
    of Young tableaux, with entries in $\{1,\ldots,n\}$. Two tabloids are equal if they have the same shape,
    and each corresponding row contains the same boxes, up to some permutation. Hence, rows are drawn without
    dividing boxes.
  Example
    lam = new Partition from {4,3,2}
    mu = new Partition from {3,1}
    T = tabloid(lam,mu,{1,2,5,3,4})
    T' = tabloid(lam,mu,{1,5,2,3,4})
    T == T'
SeeAlso
    tabloid
///

doc ///
Key
    YoungTableau
    (net, YoungTableau)
Headline
    a type of HashTable representing a Young tableau
Description
  Text
    An object of type YoungTableau is a hash table containing two shapes of type @TO Partition@,
    and a list of box entries. The entries may have any type, except for @TO null@ objects.

    The inner and outer shapes, $\lambda$ and $\mu$ respectively, can be any sequence of integers. In particular,
    it accepts negative parts, compositions, rows where $\lambda_i < \mu_i$, and compositions where
    $\ell(\lambda)\neq\ell(\mu)$. 
  Example
    lam = new Partition from {4,3,2}
    mu = new Partition from {3,1}
    entryList = {1,2,3,3,9}
    T = youngTableau(lam,mu,entryList)
SeeAlso
    youngTableau
///

doc ///
Key
    allSemistandardTableaux
   (allSemistandardTableaux, Partition, Partition, ZZ)
   (allSemistandardTableaux, Partition, ZZ)
   (allSemistandardTableaux, Partition, Partition)
   (allSemistandardTableaux, Partition)
   (allSemistandardTableaux, List)
Headline
   list all semistandard Young tableaux of a given shape
Usage
   allSemistandardTableaux(lam,mu,N)
   allSemistandardTableaux L
Inputs
    lam:Partition
      the outer shape, $\lambda$.
    mu:Partition
      the inner shape, $\mu$.
    N:ZZ
      the maximum entry.
    L:List
      a list of tuples ($\lambda^{(i)}$,$\mu^{(i)}$,$N^{(i)}$).
Outputs
    b:Bag
      bagged list of all SSYT of given shape and entries in 1..N. If N is not provided, it is assumed
      to be the length of the shape.
Description
  Text
    The resulting bag can be thought of as the set $\mathrm{SSYT}(\lambda,\mu,N)$ of semistandard
    Young tableaux of shape $\lambda/\mu$ and entries in $1,2,\ldots,N$.
  Example
    theBag = allSemistandardTableaux(new Partition from {5,4,2}, new Partition from {3,1})
    for i from 0 to 4 do print theBag#i
  Text
    When the inner partition is $0$, the function @TO numSemistandardTableaux@ computes the number of SSYT via
    the hook-content formula.
  Example
    # allSemistandardTableaux(new Partition from {6,6,5,1,1})
    numSemistandardTableaux(new Partition from {6,6,5,1,1})
  Text
    A convenient way to compute the Cartesian product 
    $\mathrm{SSYT}(\lambda^{(1)},\mu^{(1)},N^{(1)})\times\mathrm{SSYT}(\lambda^{(2)},\mu^{(2)},N^{(2)})\times\cdots$, 
    is to use a list of the inputs
    $L=\{(\lambda^{(1)},\mu^{(1)},N^{(1)}),(\lambda^{(2)},\mu^{(2)},N^{(2)}),\ldots\}$. In this case,
    the output is a bagged list of tuples of tableaux.
  Example
    lam1 = new Partition from {3,1}
    mu1 = new Partition from {2}
    lam2 = new Partition from {1,1}
    theProduct = allSemistandardTableaux {(lam1,mu1), (lam2,3)}
    for i from 0 to 5 do print theProduct#i
SeeAlso
  numSemistandardTableaux
  allStandardTableaux
  allTabloids
///

doc ///
Key
    allStandardTableaux
   (allStandardTableaux, Partition)
Headline
   list all standard Young tableaux of a given shape
Usage
   allSemistandardTableaux(lam)
Inputs
    lam:Partition
      the shape, $\lambda$.
Outputs
    b:Bag
      bagged list of all SYT of given shape and entries in $\{1,\ldots,N\}$, where $N$ is the size of $\lambda$.
Description
  Example
    theBag = allStandardTableaux(new Partition from {4,3,1})
    for i from 0 to 4 do print theBag#i
  Text
    The function @TO numStandardTableaux@ computes the number of SYT.
  Example
    # theBag
    numStandardTableaux(new Partition from {4,3,1})
SeeAlso
  numStandardTableaux
  allSemistandardTableaux
  allTabloids
///

doc ///
Key
    allSubPartitions
   (allSubPartitions, Partition, Partition)
   (allSubPartitions, Partition)
Headline
   list all partitions whose Young diagrams are contained in another
Usage
   allSubPartitions(lam,mu)
Inputs
    lam:Partition
      the outer shape, $\lambda$.
    mu:Partition
      the inner shape, $\mu$. If not provided, then it is the $0$ partition.
Outputs
    b:Bag
      bagged list of all partitions $\nu$ such that $\mu\subseteq\nu\subseteq\lambda$.
Description
  Example
    theBag = allSubPartitions(new Partition from {2,2,1},new Partition from {1})
    for nu in theBag do print youngDiagram nu
SeeAlso
  allSemistandardTableaux
  allStandardTableaux
  allTabloids
///

doc ///
Key
    allTabloids
   (allTabloids, Partition, Partition)
   (allTabloids, Partition)
Headline
   list all tabloids of a given shape
Usage
   allSemistandardTableaux(lam,mu)
Inputs
    lam:Partition
      the outer shape, $\lambda$.
    mu:Partition
      the inner shape, $\mu$.
Outputs
    b:Bag
      bagged list of all tabloids of given shape and entries in $\{1,\ldots,N\}$, where $N$ is the size of
      the tabloid.
Description
  Example
    theBag = allTabloids(new Partition from {4,3,1})
    for i from 0 to 4 do print theBag#i
  Text
    The function @TO numTabloids@ computes the number of tabloids.
  Example
    # theBag
    numTabloids(new Partition from {4,3,1})
SeeAlso
  numTabloids
  allSemistandardTableaux
  allStandardTableaux
///

doc ///
Key
    applyEntries
   (applyEntries, YoungTableau, Function)
Headline
   apply a function to all entries in the tableau
Usage
   applyEntries(T,f)
Inputs
    T:YoungTableau
      a tableau.
    f:Function
      acts on the entries of T.
Outputs
    T':YoungTableau
      a tableau with the same shape as T, and entries n -> f n.
Description
  Example
    T = youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(0..8))
    applyEntries(T, theBox -> theBox^2)
SeeAlso
  applyPositions
///

doc ///
Key
    applyPositions
   (applyPositions, YoungTableau, Function)
Headline
   apply a function to all positions of boxes in a tableau
Usage
   applyPositions(T,f)
Inputs
    T:YoungTableau
      a skew Tableau.
    f:Function
      acts on the entries of T.
Outputs
    T':YoungTableau
      a tableau with the same shape as T, and entries (i,j) -> f (i,j).
Description
  Example
    T = youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(0..8))
    applyPositions(T, thePosition -> thePosition)
    applyPositions(T, thePosition -> thePosition#1 - thePosition#0)
    applyPositions(T, thePosition -> (T_thePosition)^2)
SeeAlso
  applyEntries
///

doc ///
Key
    boxContent
   (boxContent, ZZ, ZZ)
Headline
   compute the content of a box of a tableau
Usage
   boxContent(i,j)
Inputs
    i:ZZ
      the row index of a box.
    j:ZZ
      the column index of a box.
Outputs
    n:ZZ
      the hook length of box (i,j).
Description
  Text
    The content of box (i,j) is defined as $j-i$.
  Example
    T = youngTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    boxContent(1,1)
    applyPositions(T, thePosition -> boxContent thePosition)
///

doc ///
Key
    columnEntries
   (columnEntries, YoungTableau, ZZ)
   (columnEntries, ZZ, YoungTableau)
Headline
   get the entries in a column
Usage
   columnEntries(T,j)
   columnEntries(j,T)
Inputs
    T:YoungTableau
      a tableau.
    j:ZZ
      the index of a column.
Outputs
    l:List
      the entries of column $j$ of T.
Description
  Text
    This returns the entries of a column without extra @TO null@ entries, as opposed to T_i.
  Example
    T = youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(1..9))
    columnEntries(T,0)
    T_0
SeeAlso
  (symbol _, YoungTableau, ZZ)
///

doc ///
Key
    columnRange
   (columnRange, YoungTableau)
Headline
    the range of column indices of a tableau
Usage
   columnRange T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    seq:Sequence
      the range of column indices.
Description
  Text
    Although it is straightforward that the row indices are 0..(numRows T - 1), the analogous sequence does not
    work for columns since columns may have negative indices. Hence, columnRange provides an easy way to iterate
    over column indices. The lowest number is the smallest part of the tableau's shape, and the largest number
    is the largest part of its shape.
  Example
    T = youngTableau(new Partition from {8,7,6,6,1}, new Partition from {4,4,3,2,-2}, toList(1..17))
    columnRange T
    for j in columnRange T do print columnEntries(T,j)
SeeAlso
  (symbol _, YoungTableau, ZZ)
  columnEntries
  rowRange
  positionList
///

doc ///
Key
    columnStabilizer
   (columnStabilizer, YoungTableau)
Headline
    the column stabilizer of a tableau
Usage
   columnStabilizer T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    l:List
      the permutations in the column stabilizer.
Description
  Text
    A permutation acts on a tableau by permuting its entries. The column stabilizer of a tableau is the group
    of permutations that leave boxes in their original columns.
  Example
    T = youngTableau(new Partition from {2,2,1}, {1,4,2,5,3})
    columnStabilizer T
SeeAlso
  rowStabilizer
///

doc ///
Key
    toPartitionChain
   (toPartitionChain, YoungTableau)
Headline
    decompose a tableau into a chain of partitions
Usage
   toPartitionChain T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    s:Sequence
      a sequence of partitions.
Description
  Text
    A (semistandard) Young tableau of shape $\lambda/\mu$ determines (and is determined by) a sequence of
    partitions $(\lambda^{(0)},\ldots,\lambda^{(r)})$, where
    $\mu= \lambda^{(0)}\subseteq\lambda^{(1)}\subseteq\cdots\subseteq\lambda^{(r)}=\lambda$.
  Example
    lam = new Partition from {6,6,5,3,2,2,2,1}
    mu = new Partition from {3,1}
    entryList = {1,1,7,1,3,4,4,8,1,2,4,7,7,2,3,5,3,4,4,5,5,6,8}
    T = youngTableau(lam,mu,entryList)
    theDecomp = toPartitionChain T
  Text
    This chain has the property that each $\lambda^{(i)}/\lambda^{(i-1)}$ is a horizontal strip.
  Example
    drawInnerShape true -* no-capture-flag *-
    for i from 1 to #theDecomp-1 do (
        print(toString(toSequence trim theDecomp#i)|"/"|toString(toSequence trim theDecomp#(i-1))|":");
        print youngTableau(theDecomp#i,theDecomp#(i-1));
        )
///

doc ///
Key
    drawInnerShape
   (drawInnerShape, Boolean)
Headline
    option to draw the inner shape of a tableau
Usage
    drawInnerShape b
Inputs
    b:Boolean
      whether to draw the shape or not.
Outputs
    b:Boolean
      the inputted value.
Consequences
    Item
      If b is true, then all subsequent calls to net will draw the inner shape. If b is false, then the inner
      shape is not drawn.
Description
  Example
    T = youngTableau(new Partition from {5,4,1}, new Partition from {2,1,1})
    drawInnerShape true -* no-capture-flag *-
    T
    drawInnerShape false -* no-capture-flag *-
    T
SeeAlso
  youngTableau
///

doc ///
Key
    ferrersDiagram
   (ferrersDiagram, Partition, Partition)
   (ferrersDiagram, Partition)
   (ferrersDiagram, YoungTableau)
Headline
    a net of the Ferrers diagram
Usage
    ferrersDiagram(lam, mu)
    ferrersDiagram T
Inputs
    lam:Partition
      the outer shape $\lambda$.
    mu:Partition
      the inner shape $\mu$. If not given, if is assumed to be the $0$ partition.
    T:YoungTableau
      a skew tableau.
Outputs
    n:Net
      a representation of the Ferrers diagram of shape $\lambda/\mu$.
Description
  Example
    T = youngTableau(new Partition from {4,3,1}, new Partition from {2,1}, {1,2,3,4,5})
    ferrersDiagram T
SeeAlso
  youngTableau
  youngDiagram
///

doc ///
Key
    hookLength
   (hookLength, Sequence, YoungTableau)
   (hookLength, Partition)
Headline
   compute the hook length of a box of a tableau
Usage
   n = hookLength(thePosition,T)
   m = hookLength lam
Inputs
    thePosition:Sequence
      a pair (i,j) of integers.
    T:YoungTableau
      a tableau.
    lam:Partition
      a partition.
Outputs
    n:ZZ
      the hook length of box (i,j) in T.
    m:ZZ
      the hook length formula for lam.
Description
  Text
    The hook length of box (i,j) is defined as the number of boxes directly below, and directly to the right,
    of a box, including the box itself.
  Example
    T = youngTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    hookLength((1,1),T)
    applyPositions(T, thePosition -> hookLength(thePosition,T))
  Text
    The hook length formula computes the number of standard Young tableau of shape $\lambda$.
  Example
    lam = new Partition from {3,2,2}
    hookLength lam
SeeAlso
  numStandardTableaux
  allStandardTableaux
///

doc ///
Key
    horizontalNet
Headline
   multiple nets concatenated horizontally
Usage
   horizontalNet L
Inputs
    L:List
      a list or sequence.
Outputs
    n:Net
      a net of objects in L, stacked horizontally and justified at the top.
Description
  Text
    If L is a list of objects $A_1,A_2,\ldots,A_n$, then this concatentates their nets horizontally.
  Example
    T1 = youngTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    T2 = youngTableau(new Partition from {3,1}, new Partition from {1}, {"a","b","c"})
    horizontalNet {T1, T2}
SeeAlso
  verticalNet
///

doc ///
Key
    innerShape
   (innerShape, YoungTableau)
Headline
    the inner shape of a (skew) Young tableau
Usage
    mu = innerShape T
Inputs
    T:YoungTableau
      a Young tableau of shape $\lambda/\mu$.
Outputs
    mu:Partition
      the inner shape, $\mu$, of T.
Description
  Text
    If the tableau has (non-skew) shape $\lambda$, then this returns the empty partition.
  Example
    T = youngTableau(new Partition from {4,3,1,0}, new Partition from {3,1})
    innerShape T
SeeAlso
  outerShape
  shape
  skewShape
///

doc ///
Key
    isCorner
   (isCorner, Sequence, Partition)
   (isCorner, Sequence, YoungTableau)
Headline
    checks if a box is a corner of a tableau
Usage
    isCorner((rowIndex,colIndex), T)
Inputs
    rowIndex:ZZ
      the row index of a box.
    colIndex:ZZ
      the column index of a box.
    T:YoungTableau
      a Young tableau.
Outputs
    b:Boolean
      returns true if the box is a corner, and false otherwise.
Description
  Text
    The corners of a tableau are the boxes that are both the last box in a row, and the last box in a column.
  Example
    T = youngTableau(new Partition from {7,7,6,5,3,2,2,2,1}, new Partition from {3,1})
    isCorner((1,3),T)
    applyPositions(T,thePosition -> isCorner(thePosition,T))
///

doc ///
Key
    isSemistandard
   (isSemistandard, YoungTableau)
Headline
    checks if a Young tableau is semistandard
Usage
    isSemistandard T
Inputs
    T:YoungTableau
      a Young tableau.
Outputs
    b:Boolean
      returns true if the tableau is semistandard, and false otherwise.
Description
  Text
    A semistandard Young tableau (SSYT) has entries in $\N=\{1,2,3,\ldots\}$ where the rows are weakly increasing,
    and columns are strictly increasing. Here, we also require $\lambda$ and $\mu$ to have nonnegative parts.
  Example
    T = youngTableau(new Partition from {5,3,2}, new Partition from {3,1}, {1,3,2,2,2,3})
    isSemistandard T
SeeAlso
  isStandard
///

doc ///
Key
    isSkew
   (isSkew, YoungTableau)
Headline
    checks if a Young tableau is skew
Usage
    isSkew T
Inputs
    T:YoungTableau
      a Young tableau of shape $\lambda/\mu$.
Outputs
    b:Boolean
      returns true if the inner shape $\mu$ is not $0$, and true if $\mu=0$.
Description
  Example
    T = youngTableau(new Partition from {5,3,2}, new Partition from {3,1}, {1,3,2,2,2,3})
    isSkew T
///

doc ///
Key
    isStandard
   (isStandard, YoungTableau)
Headline
    checks if a Young tableau is standard
Usage
    isStandard T
Inputs
    T:YoungTableau
      a Young tableau.
Outputs
    b:Boolean
      returns true if the tableau is standard, and false otherwise.
Description
  Text
    A standard Young tableau (SYT) has entries in $\N=\{1,2,3,\ldots,N\}$ where $N$ is the number of boxes, and
    the rows are columns are strictly increasing. Here, we also require $\lambda$ to have
    nonnegative parts, and inner shape $\mu=0$.
  Example
    T = youngTableau(new Partition from {5,3,1}, toList(1..9))
    isStandard T
SeeAlso
  isSemistandard
///

doc ///
Key
    isNonnegative
   (isNonnegative, YoungTableau)
   (isNonnegative, Partition, Partition)
   (isNonnegative, Partition)
Headline
   check if shapes are nonnegative
Usage
   isNonnegative T
   isNonnegative (lam,mu)
   isNonnegative lam
Inputs
    T:YoungTableau
      a tableau.
    lam:Partition
      the outer shape, $\lambda$.
    mu:Partition
      the inner shape, $\mu$.
Outputs
    b:Boolean
      returns true if both shapes are nonnegative, and false otherwise.
Description
  Example
    isNonnegative youngTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    isNonnegative new Partition from {5,3,-1,0}
///

doc ///
Key
    isWeaklyDecreasing
   (isWeaklyDecreasing, YoungTableau)
   (isWeaklyDecreasing, Partition, Partition)
   (isWeaklyDecreasing, Partition)
Headline
   check if shapes are weakly decreasing
Usage
   isWeaklyDecreasing T
   isWeaklyDecreasing (lam,mu)
   isWeaklyDecreasing lam
Inputs
    T:YoungTableau
      a tableau.
    lam:Partition
      the outer shape, $\lambda$.
    mu:Partition
      the inner shape, $\mu$.
Outputs
    b:Boolean
      returns true if both shapes have weakly decreasing parts, and false otherwise.
Description
  Example
    isWeaklyDecreasing youngTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    isWeaklyDecreasing new Partition from {5,3,-1,0}
///

doc ///
Key
    mutableYoungTableau
   (mutableYoungTableau, Partition, Partition, List)
   (mutableYoungTableau, Sequence, List)
   (mutableYoungTableau, Partition, List)
   (mutableYoungTableau, Partition, Partition)
   (mutableYoungTableau, Partition)
Headline
    constructor for type MutableYoungTableau
Usage
    mutableYoungTableau(lam, mu, entryList)
    mutableYoungTableau((lam,mu),entryList)
Inputs
    lam:Partition
      the outer shape $\lambda$.
    mu:Partition
      the inner shape $\mu$. If not given, then it is assumed to be the $0$ partition.
    entryList:List
      the filling of the boxes. If not given, then box entries are assumed to be the empty string "".
Outputs
    T:MutableYoungTableau
      a skew Young tableau of shape $\lambda/\mu$ with the given (mutable) filling.
Consequences
    Item
      The list of entries has length equal to $\sum_{i=1}^{\ell(\lambda)}|\lambda_i-\mu_i|$. E.g.,
      if $\lambda=(2)$ and $\mu=(5)$, then the entry list must have length $3$.
    Item
      None of the entries are null.
Description
  Text
    This is the same as type @TO YoungTableau@, except that the entries may be changed.
  Example
    T = mutableYoungTableau(new Partition from {4,2,1}, new Partition from {1}, toList(1..6))
    T_(0,2) = 9
    T
SeeAlso
  ((symbol _, symbol =), MutableYoungTableau, Sequence)
  youngTableau
///

doc ///
Key
    ((symbol _, symbol =), MutableYoungTableau, Sequence)
Headline
    change an entry of a mutable Young tableau
Usage
   T_thePosition = k
Inputs
    T:MutableYoungTableau
      a mutable tableau.
    thePosition:Sequence
      a pair (i,j) of integers.
    k:Thing
      the new entry in position (i,j).
Outputs
    T:MutableYoungTableau
      a mutable tableau with entry k in position (i,j).
Description
  Example
    T = mutableYoungTableau(new Partition from {4,2,1}, new Partition from {1}, toList(1..6))
    T_(0,2) = 9
    T
///

doc ///
Key
    numSemistandardTableaux
   (numSemistandardTableaux, Partition, ZZ)
   (numSemistandardTableaux, Partition)
Headline
   compute the number of semistandard Young tableaux of a given shape
Usage
   numSemistandardTableaux(lam,N)
Inputs
    lam:Partition
      the shape, $\lambda$.
    N:ZZ
      the maximum entry.
Outputs
    n:ZZ
      the number of SSYT of the given shape, and entries in 1..N. If N is not provided, it is assumed
      to be the length of the shape.
Description
  Text
    A bagged list of the tableaux can be created with @TO allSemistandardTableaux@.
  Example
    # allSemistandardTableaux(new Partition from {6,6,5,1,1})
    numSemistandardTableaux(new Partition from {6,6,5,1,1})
SeeAlso
  allSemistandardTableaux
  numStandardTableaux
  numTabloids
///

doc ///
Key
    numStandardTableaux
   (numStandardTableaux, Partition)
Headline
   compute the number of standard Young tableaux of a given shape
Usage
   numStandardTableaux lam
Inputs
    lam:Partition
      the shape, $\lambda$.
Outputs
    n:ZZ
      the number of SYT of the given shape, and entries in $\{1,\ldots,N\}$, where $N$ is the number of boxes.
Description
  Text
    A bagged list of the tableaux can be created with @TO allStandardTableaux@.
  Example
    # allStandardTableaux(new Partition from {4,2,1})
    numStandardTableaux(new Partition from {4,2,1})
SeeAlso
  allStandardTableaux
  numSemistandardTableaux
  numTabloids
///

doc ///
Key
    numTabloids
   (numTabloids, Partition, Partition)
   (numTabloids, Partition)
Headline
   compute the number of tabloids of a given shape
Usage
   numTabloids(lam,mu)
Inputs
    lam:Partition
      the outer shape $\lambda$.
    mu:Partition
      the inner shape $\mu$. If not given, then it is assumed to be the $0$ partition.
Outputs
    n:ZZ
      the number of tabloids of the given shape, and entries in $\{1,\ldots,N\}$, where $N$ is the number of boxes.
Description
  Text
    A bagged list of the tableaux can be created with @TO allTabloids@.
  Example
    # allTabloids(new Partition from {4,2,1})
    numTabloids(new Partition from {4,2,1})
SeeAlso
  allTabloids
  numSemistandardTableaux
  numStandardTableaux
///

doc ///
Key
    outerShape
   (outerShape, YoungTableau)
Headline
    the outer shape of a (skew) Young tableau
Usage
    mu = outerShape T
Inputs
    T:YoungTableau
      a Young tableau of shape $\lambda/\mu$.
Outputs
    lam:Partition
      the outer shape, $\lambda, of T.
Description
  Example
    T = youngTableau(new Partition from {4,3,1,0}, new Partition from {3,1})
    outerShape T
SeeAlso
  innerShape
  shape
  skewShape
///

doc ///
Key
    positionList
   (positionList, YoungTableau)
   (positionList, YoungTableau, Function)
Headline
   get the positions of all the boxes in a tableau
Usage
   positionList T
   positionList(T,f)
Inputs
    T:YoungTableau
      a tableau.
    f:Function
      a function that acts on the positions, and returns either true or false.
Outputs
    l:List
      the positions (i,j) of boxes in T. If a function f is provided, then only the positions returning true
      are in the list.
Description
  Text
    This method is useful for iterating over all positions in a tableau.
  Example
    T = youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(10..18))
    theList = positionList T
    T' = youngTableau(new Partition from {6,3,2}, new Partition from {2}, theList)
  Text
    Including a function returns only the positions that return true. For example, the following code returns
    all positions of corners in the tableau, using function @TO isCorner@.
  Example
    positionList(T, thePosition -> isCorner(thePosition,T))
SeeAlso
  toPosition
  toIndex
///

doc ///
Key
    randomSemistandardTableau
   (randomSemistandardTableau, Partition, Partition, ZZ)
   (randomSemistandardTableau, Partition, ZZ)
   (randomSemistandardTableau, Partition, Partition)
   (randomSemistandardTableau, Partition)
Headline
   get a random semistandard Young Tableau
Usage
   randomSemistandardTableau(lam,mu,N)
Inputs
    lam:Partition
      the outer shape, $\lambda$, of T.
    mu:Partition
      the inner shape, $\mu$, of T.
    N:ZZ
      the maximum entry.
Outputs
    T:YoungTableau
      a random semistandard Young tableau with entries in $\{1,\ldots,N\}$. If $N$ is not provided, then it is
      assumed to be the length of $\lambda$. If $\mu$ is not provided, then it is assumed to be $0$.
Description
  Text
    Note that this does not select a tableau uniformly randomly.
  Example
    lam = new Partition from {5,4,3,3,1}
    mu = new Partition from {3,1,1}
    randomSemistandardTableau(lam,mu)
    randomSemistandardTableau lam
SeeAlso
  randomStandardTableau
  randomTabloid
  allSemistandardTableaux
///

doc ///
Key
    randomStandardTableau
   (randomStandardTableau, Partition)
Headline
   get a random standard Young Tableau
Usage
   randomStandardTableau(lam,mu,N)
Inputs
    lam:Partition
      the outer shape, $\lambda$, of T.
    mu:Partition
      the inner shape, $\mu$, of T.
Outputs
    T:YoungTableau
      a random standard Young tableau of shape $\lambda$.
Description
  Text
    Note that this does not select a tableau uniformly randomly.
  Example
    lam = new Partition from {5,4,3,3,1}
    randomStandardTableau lam
SeeAlso
  randomSemistandardTableau
  randomTabloid
  allStandardTableaux
///

doc ///
Key
    randomTabloid
   (randomTabloid, Partition, Partition)
   (randomTabloid, Partition)
Headline
   get a random tabloid
Usage
   randomTabloid(lam,mu)
Inputs
    lam:Partition
      the outer shape, $\lambda$, of T.
    mu:Partition
      the inner shape, $\mu$, of T.
Outputs
    T:Tabloid
      a random tabloid with entries in $\{1,\ldots,N\}$, where $N$ is the length of $\lambda$.
      If $\mu$ is not provided, then it is assumed to be $0$.
Description
  Text
  Example
    lam = new Partition from {5,4,3,3,1}
    mu = new Partition from {3,1,1}
    randomTabloid(lam,mu)
    randomTabloid lam
SeeAlso
  randomSemistandardTableau
  randomStandardTableau
  allTabloids
///

doc ///
Key
    readingWord
   (readingWord, YoungTableau)
Headline
   get the reading word of a tableau
Usage
   readingWord T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    l:List
      the reading word of T.
Description
  Text
    The reading word is the list of entries going up the columns, from left to right.
  Example
    T = youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(10..18))
    readingWord T
///

doc ///
Key
    representative
   (representative, Tabloid)
Headline
   get a canonical representative of a tabloid
Usage
   representative T
Inputs
    T:Tabloid
      a tabloid.
Outputs
    T':YoungTableau
      a representative of T.
Description
  Text
    A tabloid is an equivalence class of Young Tableaux. We can obtain a canonical representative by sorting the
    boxes in each row.
  Example
    T = tabloid(new Partition from {6,3,2}, new Partition from {2}, {9,5,1,2,4,3,7,8,6})
    representative T
SeeAlso
  Tabloid
///

doc ///
Key
    rowEntries
   (rowEntries, YoungTableau, ZZ)
   (rowEntries, ZZ, YoungTableau)
Headline
   get the entries in a row
Usage
   rowEntries(T,i)
   rowEntries(i,T)
Inputs
    T:YoungTableau
      a tableau.
    i:ZZ
      the index of a row.
Outputs
    l:List
      the entries of row $i$ of T.
Description
  Text
    This returns the entries exactly as they appear in the row without extra @TO null@ entries, as opposed to T^i.
  Example
    T = youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(1..9))
    rowEntries(T,0)
    T^0
SeeAlso
  (symbol ^, YoungTableau, ZZ)
///


doc ///
Key
    rowRange
   (rowRange, YoungTableau)
Headline
    the range of row indices of a tableau
Usage
   rowRange T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    seq:Sequence
      the range of row indices.
Description
  Text
    Returns the sequence 0..(numRows T - 1).
  Example
    T = youngTableau(new Partition from {5,4,2,1,0,0}, new Partition from {2,2,2}, toList(1..6))
    rowRange T
    for i in rowRange T do print rowEntries(T,i)
SeeAlso
  (symbol ^, YoungTableau, ZZ)
  rowEntries
  columnRange
  positionList
///

doc ///
Key
    rowStabilizer
   (rowStabilizer, YoungTableau)
Headline
    the row stabilizer of a tableau
Usage
   rowStabilizer T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    l:List
      the permutations in the row stabilizer.
Description
  Text
    A permutation acts on a tableau by permuting its entries. The row stabilizer of a tableau is the group
    of permutations that leave boxes in their original rows.
  Example
    T = youngTableau(new Partition from {2,2,1}, {1,4,2,5,3})
    rowStabilizer T
SeeAlso
  columnStabilizer
///

doc ///
Key
    shape
   (shape, YoungTableau)
   (trim, Partition)
Headline
    the shape of a Young tableau
Usage
    lam = shape T
Inputs
    T:YoungTableau
      a Young tableau.
Outputs
    lam:Partition
      the shape, $\lambda$, of T.
Description
  Text
    This returns the original shape used to construct the tableau.
  Example
    T = youngTableau(new Partition from {4,3,1,0})
    shape T
  Text
    It may be convenient to remove trailing $0$'s from the partitions by using trim on the partition.
  Example
    trim shape T
SeeAlso
  skewShape
///

doc ///
Key
    shift
   (shift, YoungTableau)
   (shift, YoungTableau, ZZ)
Headline
   shift a tableau
Usage
   shift T
   shift(T,n)
Inputs
    T:YoungTableau
      a tableau.
    n:ZZ
      an additional amount to shift.
Outputs
    S:YoungTableau
      each row i of T has been shifted by i + n to the right (where n=0 if not provided).
Description
  Example
    T = youngTableau(new Partition from {6,6,5,3,1})
    shift T
    shift(T,2)
SeeAlso
  unshift
///

doc ///
Key
    skewShape
   (skewShape, YoungTableau)
   (trim, Partition, Partition)
   (pad, Partition, Partition)
Headline
    the shape of a skew tableau
Usage
    (lam,mu) = skewShape T
Inputs
    T:YoungTableau
      a skew tableau.
Outputs
    lam:Partition
      the outer shape, $\lambda$, of T.
    mu:Partition
      the inner shape, $\mu$, of T.
Description
  Text
    This returns the original shape used to construct the tableau.
  Example
    T = youngTableau(new Partition from {4,3,1,0}, new Partition from {2,1}, {1,2,3,4,5})
    skewShape T
  Text
    It may be convenient to remove trailing $0$'s from the partitions by using trim on a sequence of two partitions.
  Example
    trim skewShape T
  Text
    It may also be useful to append $0$s to make the partitions have the same number of parts.
  Example
    (lam',mu') = pad skewShape T
    rowLengths' = for i from 0 to #lam'-1 list(lam'#i-mu'#i)
  Text
    You may also 'standardize' a skew shape to return a pair of partitions of equal length, without extra $0$'s.
    I.e., one of the resulting partitions will end with a nonzero part, and the other will be padded with $0$'s.
  Example
    (lam'',mu'') = standardize skewShape T
    rowLengths = for i from 0 to #lam''-1 list(lam''#i-mu''#i)
SeeAlso
  shape
  standardize
///

doc ///
Key
    standardize
   (standardize, Partition, Partition)
Headline
    standardize the lengths of a pair of partitions
Usage
    (lam',mu') = standardize (lam,mu)
Inputs
    lam:Partition
      a partition.
    mu:Partition
      a partition.
Outputs
    lam':Partition
      a partition obtained from lam, with trailing zeros added or removed.
    mu':Partition
      a partition obtained from mu, with trailing zeros added or removed.
Consequences
    Item
      Partitions lam' and mu' have the same number of parts, possibly with trailing 0s.
Description
  Text
    A tableau T of shape $\lambda/\mu$ may be constructed with $\ell(\lambda)\neq\ell(\mu)$. It is common to
    iterate over the rows of T, and it is convenient to append trailing 0s to the shorter partition. This method
    effectively removes all trailing 0s from both shapes, then appends 0s to the shorter shape until they
    have the same length.
  Example
    T = youngTableau(new Partition from {4,3,1,0}, new Partition from {2,1})
    skewShape T
    standardize skewShape T
SeeAlso
  skewShape
  shape
///

doc ///
Key
    tabloid
   (tabloid, Partition, Partition, List)
   (tabloid, Sequence, List)
   (tabloid, Partition, List)
   (tabloid, YoungTableau)
Headline
    constructor for type Tabloid
Usage
    tabloid(lam, mu, entryList)
    tabloid((lam,mu),entryList)
Inputs
    lam:Partition
      the outer shape $\lambda$.
    mu:Partition
      the inner shape $\mu$. If not given, then it is assumed to be the $0$ partition.
    entryList:List
      the filling of the boxes.
Outputs
    T:Tabloid
      a tabloid of shape $\lambda/\mu$ with the given filling.
Consequences
    Item
      The list of entries has length equal to $\sum_{i=1}^{\ell(\lambda)}|\lambda_i-\mu_i|$. E.g.,
      if $\lambda=(2)$ and $\mu=(5)$, then the entry list must have length $3$.
    Item
      The entries are $\{1,\ldots,N\}$, where $N$ is the number of boxes.
Description
  Example
    lam = new Partition from {4,3,2}
    mu = new Partition from {3,1}
    entryList = {1,4,2,3,5}
    T = tabloid(lam,mu,entryList)
SeeAlso
  Tabloid
  YoungTableau
///

doc ///
Key
    toIndex
   (toIndex, YoungTableau, Sequence)
   (toIndex, Sequence, YoungTableau)
Headline
   get the index of a box, given its position
Usage
   toIndex(T,seq)
   toIndex(T,(i,j))
   toIndex(seq,T)
   toIndex((i,j),T)
Inputs
    T:YoungTableau
      a tableau.
    seq:Sequence
      the position (i,j) of a box.
Outputs
    k:ZZ
      the index k of the box in T.
Description
  Text
    The boxes in entries T fill the tableau from top to bottom, and left to right. Hence, a position (i,j)
    corresponds to an index k in entries T
  Example
    T = youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(10..18))
    thePosition = (1,2)
    T_thePosition
    theIndex = toIndex(T,thePosition)
    (entries T)#theIndex
  Text
    We may replace the filling in each box with its index to see the order.
  Example
    youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(0..(size T - 1)))
  Text
    We may also replace the filling in each box with the corresponding position.
  Example
    entryList = for i from 0 to size T - 1 list toPosition(T,i)
    youngTableau(new Partition from {6,3,2}, new Partition from {2}, entryList)
SeeAlso
  toPosition
  positionList
///

doc ///
Key
    toPosition
   (toPosition, YoungTableau, ZZ)
   (toPosition, ZZ, YoungTableau)
Headline
   get the position of a box, given its index
Usage
   toPosition(T,k)
   toPosition(k,T)
Inputs
    T:YoungTableau
      a tableau.
    k:ZZ
      the index of a box.
Outputs
    l:Sequence
      the position (i,j) of the box in T.
Description
  Text
    The boxes in entries T fill the tableau from top to bottom, and left to right. Hence, an index in entries T
    corresponds to a position (i,j).
  Example
    T = youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(10..18))
    (entries T)#5
    toPosition(T,5)
  Text
    We may replace the filling in each box with its index to see the order.
  Example
    youngTableau(new Partition from {6,3,2}, new Partition from {2}, toList(0..(size T - 1)))
  Text
    We may also replace the filling in each box with the corresponding position.
  Example
    entryList = for i from 0 to size T - 1 list toPosition(T,i)
    youngTableau(new Partition from {6,3,2}, new Partition from {2}, entryList)
SeeAlso
  toIndex
  positionList
///

doc ///
Key
    unshift
   (unshift, YoungTableau)
   (unshift, YoungTableau, ZZ)
Headline
   unshift a tableau
Usage
   unshift T
   unshift(T,n)
Inputs
    T:YoungTableau
      a skew tableau.
    n:ZZ
      an additional amount to shift.
Outputs
    S:YoungTableau
      each row i of T has been shifted by i + n to the left (where n=0 if not provided).
Description
  Example
    T = youngTableau(new Partition from {6,6,5,3,1})
    T' = shift T
    unshift T'
SeeAlso
  shift
///

doc ///
Key
    verticalConcatenate
   (verticalConcatenate, List)
Headline
   vertical concatenation of a list of tableaux
Usage
   verticalConcatenate l
Inputs
    l:List
      a list of tableaux.
Outputs
    S:YoungTableau
      a single tableau with combining those in the list.
Description
  Example
    T1 = youngTableau(new Partition from {5}, new Partition from {2})
    T2 = youngTableau(new Partition from {3,2}, new Partition from {1}, {1,2,3,4})
    T3 = youngTableau(new Partition from {5,-3})
    verticalConcatenate {T1,T2,T3}
SeeAlso
  (symbol ||, YoungTableau, YoungTableau)
///

doc ///
Key
    verticalNet
Headline
   multiple nets concatenated vertically
Usage
   verticalNet L
Inputs
    L:List
      a list or sequence.
Outputs
    n:Net
      a net of objects in L, stacked vertically and justified on the left.
Description
  Text
    If L is a list of objects $A_1,A_2,\ldots,A_n$, then this concatentates their nets vertically. Note that
    this concatenates their nets into a single net, whereas @TO verticalConcatenate@ concatenates the
    tableaux themselves into a single tableau.
  Example
    T1 = youngTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    T2 = youngTableau(new Partition from {3,1}, new Partition from {1}, {"a","b","c"})
    verticalNet {T1, T2}
SeeAlso
  horizontalNet
///

doc ///
Key
    youngDiagram
   (youngDiagram, Partition, Partition)
   (youngDiagram, Partition)
   (youngDiagram, YoungTableau)
Headline
    a net of the Young diagram
Usage
    youngDiagram(lam, mu)
    youngDiagram T
Inputs
    lam:Partition
      the outer shape $\lambda$.
    mu:Partition
      the inner shape $\mu$. If not given, if is assumed to be the $0$ partition.
    T:YoungTableau
      a skew tableau.
Outputs
    n:Net
      a representation of the Young diagram of shape $\lambda/\mu$.
Description
  Example
    T = youngTableau(new Partition from {4,3,1}, new Partition from {2,1}, {1,2,3,4,5})
    youngDiagram T
SeeAlso
  youngTableau
  ferrersDiagram
///

doc ///
Key
    youngTableau
   (youngTableau, Partition, Partition, List)
   (youngTableau, Sequence, List)
   (youngTableau, Partition, List)
   (youngTableau, Partition, Partition)
   (youngTableau, Partition)
   (youngTableau, Tabloid)
Headline
    constructor for type YoungTableau
Usage
    youngTableau(lam, mu, entryList)
    youngTableau((lam,mu),entryList)
    youngTableau(lam, entryList)
Inputs
    lam:Partition
      the outer shape $\lambda$.
    mu:Partition
      the inner shape $\mu$. If not given, then it is assumed to be the $0$ partition.
    entryList:List
      the filling of the boxes. If not given, then box entries are assumed to be the empty string "".
Outputs
    T:YoungTableau
      a skew Young tableau of shape $\lambda/\mu$ with the given filling.
Consequences
    Item
      The list of entries has length equal to $\sum_{i=1}^{\ell(\lambda)}|\lambda_i-\mu_i|$. E.g.,
      if $\lambda=(2)$ and $\mu=(5)$, then the entry list must have length $3$.
    Item
      None of the entries are null.
Description
  Example
    lam = new Partition from {4,3,2}
    mu = new Partition from {3,1}
    entryList = {1,2,3,3,9}
    T = youngTableau(lam,mu,entryList)
  Text
    We may construct tableaux with any compositions.
  Example
    youngTableau(new Partition from {3,5,1}, new Partition from {0,1}, {7,"&",4,2,"g","u",6,0})
  Text
    The shapes may have negative parts. In this case, a vertical line is drawn by @TO2{(net, YoungTableau),"net"}@
    to indicate that negative parts are to the left.
  Example
    youngTableau(new Partition from {3,-3,-1}, new Partition from {-2,-4,-1})
  Text
    If any $\lambda_i<\mu_i$, then the boxes in that row are drawn shaded.
  Example
    youngTableau(new Partition from {-2,-4,2}, new Partition from {1,-1,-1}, {1,2,3,4,5,6,7,8,9})
  Text
    The inner shape may be drawn by calling drawInnerShape.
  Example
    T'' = youngTableau(new Partition from {5,4,-1}, new Partition from {2,4,-3})
    drawInnerShape true -* no-capture-flag *-
    T''
    drawInnerShape false -* no-capture-flag *-
    T''
  Text
    The filling may be of any class (besides @TO null@ objects), including other tableaux!
  Example
    youngTableau(lam,mu,for i from 1 to size T list randomTabloid lam)
SeeAlso
  YoungTableau
///

doc ///
Key
   (components, YoungTableau)
Headline
   get the connected components of a tableau
Usage
   components T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    l:List
      a list of all connected components of T.
Description
  Example
    T = youngTableau(new Partition from {6,2,2}, new Partition from {2}, toList(0..7))
    components T
SeeAlso
  (symbol ++, YoungTableau, YoungTableau)
///

doc ///
Key
   (conjugate, YoungTableau)
Headline
   conjugate a tableau
Usage
   conjugate T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    S:YoungTableau
      the rows and columns have been switched.
Description
  Text
    This method conjugates the tableau only if the shape has weakly decreasing parts, and no negative parts.
    Otherwise, it will raise an error.
  Example
    T = youngTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1}, toList(1..17))
    conjugate T
///

doc ///
Key
   (entries, YoungTableau)
Headline
    the filling of a tableau
Usage
    entries T
Inputs
    T:YoungTableau
      a Young tableau.
Outputs
    l:List
      the filling of T.
Description
  Text
    This returns the filling as a list.
  Example
    T = youngTableau(new Partition from {4,3,1,0},toList(1..8))
    entries T
SeeAlso
  (size, YoungTableau)
///

doc ///
Key
   (numColumns, YoungTableau)
Headline
    the number of columns in a tableau
Usage
    numColumns T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    n:ZZ
      the number of columns in T.
Description
  Text
    The number of rows is the same as largest part in its shape, minus the smallest part in the shape.
  Example
    drawInnerShape true -* no-capture-flag *-
    T = youngTableau(new Partition from {4,3,1}, new Partition from {1,1,1})
    numColumns T
  Text
    Note that, if a shape contains negative parts, then columns may have negative indices. 
  Example
    T' = youngTableau(new Partition from {4,3,1}, new Partition from {-1,1,1})
    numColumns T'
SeeAlso
  (numRows, YoungTableau)
  columnRange
///

doc ///
Key
   (numRows, YoungTableau)
Headline
    the number of rows in a tableau
Usage
    numRows T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    n:ZZ
      the number of rows in T.
Description
  Text
    The number of rows is the same as the length of its shapes, excluding trailing $0$'s.
  Example
    T = youngTableau(new Partition from {4,3,1,0}, new Partition from {1,1,3,2,0})
    numRows T
    (lam,mu) = standardize skewShape T
    #lam
SeeAlso
  (numColumns, YoungTableau)
///

doc ///
Key
   (size, YoungTableau)
Headline
    the number of boxes in a tableau
Usage
    size T
Inputs
    T:YoungTableau
      a skew Tableau.
Outputs
    n:ZZ
      the number of boxes in T.
Description
  Example
    T = youngTableau(new Partition from {4,3,1,0})
    size T
SeeAlso
  (entries, YoungTableau)
///

doc ///
Key
   (symbol ==, Tabloid, Tabloid)
Headline
   check if two tabloids are equal
Usage
   T1 == T2
Inputs
    T1:Tabloid
      a tabloid.
    T2:Tabloid
      a tabloid.
Outputs
    b:Boolean
      whether the tabloids are equal or not.
Description
  Text
    Two tabloids are equal if they have the same representative. That is, if their corresponding rows contain the
    same numbers.
  Example
    T1 = tabloid(new Partition from {5,3,1}, new Partition from {2}, {7,2,4,1,3,6,5})
    T2 = tabloid(new Partition from {5,3,1}, new Partition from {2}, {2,7,4,3,6,1,5})
    T1 == T2
///

doc ///
Key
   (tex, YoungTableau)
Headline
    LaTeX output for a tableau
Usage
    tex T
Inputs
    T:YoungTableau
      a tableau.
Outputs
    s:String
      the LaTeX code for reproducting the given tableau.
Description
  Text
    The LaTeX code uses commands from the LaTeX package @HREF("https://github.com/AndrewMathas/aTableau","aTableau")@.
  Example
    T = youngTableau(new Partition from {4,3,1}, new Partition from {2,1}, {1,2,3,4,5})
    tex T
    T' = tabloid T
    tex T'
SeeAlso
  youngTableau
  youngDiagram
  ferrersDiagram
///

doc ///
Key
   (toList, Tabloid)
Headline
    list representation of a tabloid
Usage
    toList T
Inputs
    T:Tabloid
      a tabloid.
Outputs
    l:Bag
      a bagged list of all Young tableaux in the equivelence class T.
Description
  Example
    T = tabloid(new Partition from {3,1}, {1,2,3,4})
    listT = toList T
    peek listT
///

doc ///
Key
   (symbol ++, YoungTableau, YoungTableau)
Headline
   direct sum of tableaux
Usage
   T1 ++ T2
Inputs
    T1:YoungTableau
      a tableau.
    T2:YoungTableau
      a tableau.
Outputs
    S:YoungTableau
      a direct sum of T1 and T2.
Description
  Text
    The tableaux are combined into the disconnected components (not sharing an edge) of a single tableau.
  Example
    T1 = youngTableau(new Partition from {5}, new Partition from {2})
    T2 = youngTableau(new Partition from {3,2}, new Partition from {1}, {1,2,3,4})
    T1 ++ T2
    T2 ++ T1
SeeAlso
  (components, YoungTableau)
  (symbol ||, YoungTableau, YoungTableau)
///

doc ///
Key
   (symbol ==, YoungTableau, YoungTableau)
Headline
   check if two Young tableaux are equal
Usage
   T1 == T2
Inputs
    T1:YoungTableau
      a tableau.
    T2:YoungTableau
      a tableau.
Outputs
    b:Boolean
      whether the tableaux are equal or not.
Description
  Text
    Two tableaux are equal if they have the same shape (ignoring trailing 0s), and the same filling.
  Example
    T1 = youngTableau(new Partition from {5,3,1}, new Partition from {2}, {7,2,4,1,3,6,5})
    T2 = youngTableau(new Partition from {5,3,1}, new Partition from {2}, {2,7,4,3,6,1,5})
    T1 == T2
///

doc ///
Key
   (symbol ^, YoungTableau, ZZ)
Headline
    get the entries in a row
Usage
   T^i
Inputs
    T:YoungTableau
      a tableau.
    i:ZZ
      the index of a row.
Outputs
    l:List
      the entries in the $i$th row of T.
Description
  Text
    Note that the resulting list may contain @TO null@ entries in places where a box is not present. This means that
    the notations (T^i)#j, (T_j)#i, and T_(i,j) are all equivalent for valid positions (i,j). However, the notation
    T_(i,j) will raise an error for nonvalid positions, whereas the other two will return @TO null@.
  Example
    T = youngTableau(new Partition from {4,3,1,0}, new Partition from {1,1}, {1,2,3,4,5,6})
    for i from 0 to numRows T - 1 do print T^i
    (T^0)#1
    (T_1)#0
    T_(0,1)
  Text
   If T has negative rows, then the entries at negative column coordinates are to the right in the output.
  Example
    T' = youngTableau(new Partition from {4,3,1,0}, new Partition from {1,-1}, {1,2,3,4,5,6,7,8})
    for i from 0 to numRows T' - 1 do print T'^i
    T'^1#-1
    T'_(1,-1)
  Text
    To get the row entries exactly as they appear in the tableau and without extra null's, use rowEntries.
  Example
    T'' = youngTableau(new Partition from {3,4}, new Partition from {-2}, {1,2,3,4,5,6,7,8,9})
    T''^0
    rowEntries(T'',0)
SeeAlso
  (numRows, YoungTableau)
  rowEntries
  (symbol _, YoungTableau, Sequence)
  (symbol _, YoungTableau, ZZ)
///

doc ///
Key
   (symbol _, YoungTableau, Sequence)
Headline
    get the entry at a specific position
Usage
   T_seq
   T_(i,j)
Inputs
    T:YoungTableau
      a tableau.
    seq:Sequence
      a pair (i,j) of integers in ZZ.
Outputs
    theBox:Thing
      the entry of T in position (i,j).
Description
  Text
    The notations (T^i)#j, (T_j)#i, and T_(i,j) are all equivalent for valid positions (i,j). However, the notation
    T_(i,j) will raise an error for nonvalid positions, whereas the other two will return @TO null@.
  Example
    T = youngTableau(new Partition from {4,3,1,0}, new Partition from {1,1}, {1,2,3,4,5,6})
    (T^0)#1
    (T_1)#0
    T_(0,1)
SeeAlso
  (symbol _, YoungTableau, ZZ)
  (symbol ^, YoungTableau, ZZ)
///

doc ///
Key
   (symbol _, YoungTableau, ZZ)
Headline
    get the entries in a column
Usage
   T_j
Inputs
    T:YoungTableau
      a tableau.
    j:ZZ
      the index of a column.
Outputs
    l:List
      the entries in the $j$th column of T.
Description
  Text
    Note that the resulting list may contain @TO null@ entries in places where a box is not present. This means that
    the notations (T^i)#j, (T_j)#i, and T_(i,j) are all equivalent for valid positions (i,j). However, the notation
    T_(i,j) will raise an error for nonvalid positions, whereas the other two will return @TO null@.
  Example
    T = youngTableau(new Partition from {4,3,1,0}, new Partition from {1,1}, {1,2,3,4,5,6})
    for i in columnRange T do print T_i
    (T^0)#1
    (T_1)#0
    T_(0,1)
  Text
    To get the column entries without extra null's, use columnEntries.
  Example
    T'' = youngTableau(new Partition from {3,1}, new Partition from {-2,0,-1}, {1,2,3,4,5,6,7})
    T''_-1
    columnEntries(T'',-1)
SeeAlso
  (numColumns, YoungTableau)
  columnEntries
  (symbol _, YoungTableau, Sequence)
  (symbol ^, YoungTableau, ZZ)
///

doc ///
Key
   (symbol ||, YoungTableau, YoungTableau)
Headline
   vertical concatenation of two tableaux
Usage
   T1 || T2
Inputs
    T1:YoungTableau
      a tableau.
    T2:YoungTableau
      a tableau.
Outputs
    S:YoungTableau
      a single tableau with T1 directly above T2.
Description
  Example
    T1 = youngTableau(new Partition from {5}, new Partition from {2})
    T2 = youngTableau(new Partition from {3,2}, new Partition from {1}, {1,2,3,4})
    T1 || T2
    T2 || T1
SeeAlso
  verticalConcatenate
  (symbol ++, YoungTableau, YoungTableau)
///
