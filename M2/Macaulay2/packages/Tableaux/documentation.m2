
doc ///
Node
  Key
    Tableaux
  Headline
    a package for computing with Young tableaux
  Description
    Text
      This package provides the classes @TO SkewTableau@ and @TO YoungTableau@.
///

doc ///
Key
    SkewTableau
    (net, SkewTableau)
Headline
    a type of HashTable representing a skew Young tableau
Description
  Text
    An object of type SkewTableau is a hash table containing two shapes of type @TO Partition@,
    and a list of box entries. The entries may have any type, except for @TO null@ objects.

    The inner and outer shapes, $\lambda$ and $\mu$ respectively, can be any sequence of integers. In particular,
    it accepts negative parts, compositions, rows where $\lambda_i < \mu_i$, and compositions where
    $\ell(\lambda)\neq\ell(\mu)$. 
  Example
    lam = new Partition from {4,3,2}
    mu = new Partition from {3,1}
    entryList = {1,2,3,3,9}
    T = skewTableau(lam,mu,entryList)
SeeAlso
    skewTableau
    YoungTableau
///

doc ///
Key
    YoungTableau
Headline
    a type of HashTable representing a (nonskew) Young tableau
Description
  Text
    This is a subclass of @TO SkewTableau@. Each object of YoungTableau is essentially just a skew tableau with inner
    shape $\mu=0$.
  Example
    lam = new Partition from {4,3,2}
    entryList = toList(1..(sum toList lam))
    T = youngTableau(lam,entryList)
SeeAlso
    youngTableau
    SkewTableau
///

doc ///
Key
    skewTableau
   (skewTableau, Partition, Partition, List)
   (skewTableau, Sequence, List)
   (skewTableau, Partition, List)
   (skewTableau, Partition, Partition)
   (skewTableau, Partition)
   (skewTableau, YoungTableau)
    drawInnerShape
   (drawInnerShape, Boolean)
Headline
    constructor for type SkewTableau
Usage
    skewTableau(lam, mu, entryList)
    skewTableau((lam,mu),entryList)
Inputs
    lam:Partition
      the outer shape $\lambda$.
    mu:Partition
      the inner shape $\mu$. If not given, then it is assumed to be the $0$ partition.
    entryList:List
      the filling of the boxes. If not given, then box entries are assumed to be the empty string "".
Outputs
    T:SkewTableau
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
    T = skewTableau(lam,mu,entryList)
  Text
    We may construct tableaux with any compositions.
  Example
    skewTableau(new Partition from {3,5,1}, new Partition from {0,1}, {7,"&",4,2,"g","u",6,0})
  Text
    The shapes may have negative parts. In this case, a vertical line is drawn by @TO2{(net, SkewTableau),"net"}@
    to indicate that negative parts are to the left.
  Example
    skewTableau(new Partition from {3,-3,-1}, new Partition from {-2,-4,-1})
  Text
    If any $\lambda_i<\mu_i$, then the boxes in that row are drawn shaded.
  Example
    skewTableau(new Partition from {-2,-4,2}, new Partition from {1,-1,-1}, {1,2,3,4,5,6,7,8,9})
  Text
    We may cast an object of type @TO YoungTableau@ to type SkewTableau.
  Example
    T' = youngTableau(new Partition from {3,1,1})
    skewTableau T'
  Text
    The inner shape may be drawn by calling drawInnerShape.
  Example
    T'' = skewTableau(new Partition from {5,4,-1}, new Partition from {2,4,-3})
    drawInnerShape true -* no-capture-flag *-
    T''
    drawInnerShape false
    T''
SeeAlso
  SkewTableau
  YoungTableau
  youngTableau
///

doc ///
Key
    youngTableau
   (youngTableau, Partition, List)
   (youngTableau, Partition)
Headline
    constructor for type YoungTableau
Usage
    youngTableau(lam, entryList)
Inputs
    lam:Partition
      the shape $\lambda$.
    entryList:List
      the filling of the boxes. If not given, then box entries are assumed to be the empty string "".
Outputs
    T:YoungTableau
      a (nonskew) Young tableau of shape $\lambda$ with the given filling.
Consequences
    Item
      The list of entries has length equal to $\sum_{i=1}^{\ell(\lambda)}|\lambda_i|$. E.g.,
      if $\lambda=(-2)$, then the entry list must have length $2$.
    Item
      None of the entries are @TO null@.
Description
  Example
    lam = new Partition from {4,3,2}
    entryList = {1,2,3,4,5,6,7,8,9}
    T = youngTableau(lam,entryList)
  Text
    We may construct tableaux with any compositions.
  Example
    youngTableau(new Partition from {3,5,1}, {7,"&",4,2,"g","u",6,0,-1})
  Text
    The shape may have negative parts. In this case, a vertical line is drawn by @TO2{(net, SkewTableau),"net"}@
    to indicate that negative parts are to the left. If any $\lambda_i<0$, then the boxes in that row are drawn shaded.
  Example
    youngTableau(new Partition from {-2,-4,2})
  Text
    We may cast an object of type YoungTableau to type @TO SkewTableau@.
  Example
    T' = youngTableau(new Partition from {3,1,1})
    skewTableau T'
SeeAlso
  SkewTableau
  YoungTableau
  skewTableau
///

doc ///
Key
    youngDiagram
   (youngDiagram, Partition, Partition)
   (youngDiagram, Partition)
   (youngDiagram, SkewTableau)
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
    T:SkewTableau
      a skew tableau.
Outputs
    n:Net
      a representation of the Young diagram of shape $\lambda/\mu$.
Description
  Example
    T = skewTableau(new Partition from {4,3,1}, new Partition from {2,1}, {1,2,3,4,5})
    youngDiagram T
SeeAlso
  skewTableau
  youngTableau
  ferrersDiagram
///

doc ///
Key
    ferrersDiagram
   (ferrersDiagram, Partition, Partition)
   (ferrersDiagram, Partition)
   (ferrersDiagram, SkewTableau)
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
    T:SkewTableau
      a skew tableau.
Outputs
    n:Net
      a representation of the Ferrers diagram of shape $\lambda/\mu$.
Description
  Example
    T = skewTableau(new Partition from {4,3,1}, new Partition from {2,1}, {1,2,3,4,5})
    ferrersDiagram T
SeeAlso
  skewTableau
  youngTableau
  youngDiagram
///

doc ///
Key
   (tex, SkewTableau)
Headline
    LaTeX output for a skew tableau
Usage
    tex T
Inputs
    T:SkewTableau
      a skew tableau.
Outputs
    s:String
      the LaTeX code for reproducting the given skew tableau.
Description
  Text
    The LaTeX code uses commands from the LaTeX package @HREF("https://github.com/AndrewMathas/aTableau","aTableau")@.
  Example
    T = skewTableau(new Partition from {4,3,1}, new Partition from {2,1}, {1,2,3,4,5})
    tex T
SeeAlso
  skewTableau
  youngTableau
  youngDiagram
  ferrersDiagram
///

doc ///
Key
    skewShape
   (skewShape, SkewTableau)
   (truncate, Partition, Partition)
   (pad, Partition, Partition)
    standardize
   (standardize, Partition, Partition)
Headline
    the shape of a skew tableau
Usage
    (lam,mu) = skewShape T
Inputs
    T:SkewTableau
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
    T = skewTableau(new Partition from {4,3,1,0}, new Partition from {2,1}, {1,2,3,4,5})
    skewShape T
  Text
    It may be convenient to remove trailing $0$'s from the partitions by using truncate on a sequence of two partitions.
  Example
    truncate skewShape T
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
///


doc ///
Key
    shape
   (shape, YoungTableau)
   (truncate, Partition)
Headline
    the shape of a Young tableau
Usage
    lam = shape T
Inputs
    T:SkewTableau
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
    It may be convenient to remove trailing $0$'s from the partitions by using truncate on the partition.
  Example
    truncate shape T
SeeAlso
  skewShape
///


doc ///
Key
   (entries, SkewTableau)
Headline
    the filling of a skew tableau
Usage
    entries T
Inputs
    T:SkewTableau
      a skew tableau.
Outputs
    l:List
      the filling of T.
Description
  Text
    This returns the filling as a list.
  Example
    T = skewTableau(new Partition from {4,3,1,0},toList(1..8))
    entries T
SeeAlso
  (size, SkewTableau)
///

doc ///
Key
   (size, SkewTableau)
Headline
    the number of boxes in a skew tableau
Usage
    size T
Inputs
    T:SkewTableau
      a skew Tableau.
Outputs
    n:ZZ
      the number of boxes in T.
Description
  Example
    T = skewTableau(new Partition from {4,3,1,0})
    size T
SeeAlso
  (entries, SkewTableau)
///

doc ///
Key
   (numRows, SkewTableau)
Headline
    the number of rows in a tableau
Usage
    numRows T
Inputs
    T:SkewTableau
      a tableau.
Outputs
    n:ZZ
      the number of rows in T.
Description
  Text
    The number of rows is the same as the length of its shapes, excluding trailing $0$'s.
  Example
    T = skewTableau(new Partition from {4,3,1,0}, new Partition from {1,1,3,2,0})
    numRows T
    (lam,mu) = standardize skewShape T
    #lam
SeeAlso
  (numColumns, SkewTableau)
///

doc ///
Key
   (numColumns, SkewTableau)
Headline
    the number of columns in a tableau
Usage
    numColumns T
Inputs
    T:SkewTableau
      a tableau.
Outputs
    n:ZZ
      the number of columns in T.
Description
  Text
    The number of rows is the same as largest part in its shape, minus the smallest part in the shape.
  Example
    drawInnerShape true -* no-capture-flag *-
    T = skewTableau(new Partition from {4,3,1}, new Partition from {1,1,1})
    numColumns T
  Text
    Note that, if a shape contains negative parts, then columns may have negative indices. 
  Example
    T' = skewTableau(new Partition from {4,3,1}, new Partition from {-1,1,1})
    numColumns T'
SeeAlso
  (numRows, SkewTableau)
  columnRange
///

doc ///
Key
   (symbol ^, SkewTableau, ZZ)
Headline
    get the entries in a row.
Usage
   T^i
Inputs
    T:SkewTableau
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
    T = skewTableau(new Partition from {4,3,1,0}, new Partition from {1,1}, {1,2,3,4,5,6})
    for i from 0 to numRows T - 1 do print T^i
    (T^0)#1
    (T_1)#0
    T_(0,1)
  Text
   If T has negative rows, then the entries at negative column coordinates are to the right in the output.
  Example
    T' = skewTableau(new Partition from {4,3,1,0}, new Partition from {1,-1}, {1,2,3,4,5,6,7,8})
    for i from 0 to numRows T' - 1 do print T'^i
    T'^1#-1
    T'_(1,-1)
  Text
    To get the row entries exactly as they appear in the tableau and without extra null's, use rowEntries.
  Example
    T'' = skewTableau(new Partition from {3,4}, new Partition from {-2}, {1,2,3,4,5,6,7,8,9})
    T''^0
    rowEntries(T'',0)
SeeAlso
  (numRows, SkewTableau)
  rowEntries
  (symbol _, SkewTableau, Sequence)
  (symbol _, SkewTableau, ZZ)
///

doc ///
Key
   (symbol _, SkewTableau, ZZ)
Headline
    get the entries in a column.
Usage
   T_j
Inputs
    T:SkewTableau
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
    T = skewTableau(new Partition from {4,3,1,0}, new Partition from {1,1}, {1,2,3,4,5,6})
    for i in columnRange T do print T_i
    (T^0)#1
    (T_1)#0
    T_(0,1)
  Text
    To get the column entries without extra null's, use columnEntries.
  Example
    T'' = skewTableau(new Partition from {3,1}, new Partition from {-2,0,-1}, {1,2,3,4,5,6,7})
    T''_-1
    columnEntries(T'',-1)
SeeAlso
  (numColumns, SkewTableau)
  columnEntries
  (symbol _, SkewTableau, Sequence)
  (symbol ^, SkewTableau, ZZ)
///

doc ///
Key
   (symbol _, SkewTableau, Sequence)
Headline
    get the entry at a specific position.
Usage
   T_seq
   T_(i,j)
Inputs
    T:SkewTableau
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
    T = skewTableau(new Partition from {4,3,1,0}, new Partition from {1,1}, {1,2,3,4,5,6})
    (T^0)#1
    (T_1)#0
    T_(0,1)
SeeAlso
  (symbol _, SkewTableau, ZZ)
  (symbol ^, SkewTableau, ZZ)
///

doc ///
Key
    columnRange
   (columnRange, SkewTableau)
Headline
    the range of column indices of a tableau.
Usage
   columnRange T
Inputs
    T:SkewTableau
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
    T = skewTableau(new Partition from {8,7,6,6,1}, new Partition from {4,4,3,2,-2}, toList(1..17))
    columnRange T
    for j in columnRange T do print columnEntries(T,j)
SeeAlso
  (symbol _, SkewTableau, ZZ)
  columnEntries
  rowRange
  positionList
///

doc ///
Key
    rowRange
   (rowRange, SkewTableau)
Headline
    the range of row indices of a tableau.
Usage
   rowRange T
Inputs
    T:SkewTableau
      a tableau.
Outputs
    seq:Sequence
      the range of row indices.
Description
  Text
    Returns the sequence 0..(numRows T - 1).
  Example
    T = skewTableau(new Partition from {5,4,2,1,0,0}, new Partition from {2,2,2}, toList(1..6))
    rowRange T
    for i in rowRange T do print rowEntries(T,i)
SeeAlso
  (symbol ^, SkewTableau, ZZ)
  rowEntries
  columnRange
  positionList
///

doc ///
Key
    rowEntries
   (rowEntries, SkewTableau, ZZ)
   (rowEntries, ZZ, SkewTableau)
Headline
   get the entries in a row.
Usage
   rowEntries(T,i)
   rowEntries(i,T)
Inputs
    T:SkewTableau
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
    T = skewTableau(new Partition from {6,3,2}, new Partition from {2}, toList(1..9))
    rowEntries(T,0)
    T^0
SeeAlso
  (symbol ^, SkewTableau, ZZ)
///

doc ///
Key
    columnEntries
   (columnEntries, SkewTableau, ZZ)
   (columnEntries, ZZ, SkewTableau)
Headline
   get the entries in a column.
Usage
   columnEntries(T,j)
   columnEntries(j,T)
Inputs
    T:SkewTableau
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
    T = skewTableau(new Partition from {6,3,2}, new Partition from {2}, toList(1..9))
    columnEntries(T,0)
    T_0
SeeAlso
  (symbol _, SkewTableau, ZZ)
///

doc ///
Key
    toPosition
   (toPosition, SkewTableau, ZZ)
   (toPosition, ZZ, SkewTableau)
Headline
   get the position of a box, given its index.
Usage
   toPosition(T,k)
   toPosition(k,T)
Inputs
    T:SkewTableau
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
    T = skewTableau(new Partition from {6,3,2}, new Partition from {2}, toList(10..18))
    (entries T)#5
    toPosition(T,5)
  Text
    We may replace the filling in each box with its index to see the order.
  Example
    skewTableau(new Partition from {6,3,2}, new Partition from {2}, toList(0..(size T - 1)))
  Text
    We may also replace the filling in each box with the corresponding position.
  Example
    entryList = for i from 0 to size T - 1 list toPosition(T,i)
    skewTableau(new Partition from {6,3,2}, new Partition from {2}, entryList)
SeeAlso
  toIndex
  positionList
///


doc ///
Key
    toIndex
   (toIndex, SkewTableau, Sequence)
   (toIndex, Sequence, SkewTableau)
Headline
   get the index of a box, given its position.
Usage
   toIndex(T,seq)
   toIndex(T,(i,j))
   toIndex(seq,T)
   toIndex((i,j),T)
Inputs
    T:SkewTableau
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
    T = skewTableau(new Partition from {6,3,2}, new Partition from {2}, toList(10..18))
    thePosition = (1,2)
    T_thePosition
    theIndex = toIndex(T,thePosition)
    (entries T)#theIndex
  Text
    We may replace the filling in each box with its index to see the order.
  Example
    skewTableau(new Partition from {6,3,2}, new Partition from {2}, toList(0..(size T - 1)))
  Text
    We may also replace the filling in each box with the corresponding position.
  Example
    entryList = for i from 0 to size T - 1 list toPosition(T,i)
    skewTableau(new Partition from {6,3,2}, new Partition from {2}, entryList)
SeeAlso
  toPosition
  positionList
///

doc ///
Key
    positionList
   (positionList, SkewTableau)
Headline
   get the positions of all the boxes in a tableau
Usage
   positionList T
Inputs
    T:SkewTableau
      a tableau.
Outputs
    l:List
      the positions (i,j) of boxes in T.
Description
  Text
    This method is useful for iterating over all positions in a tableau
  Example
    T = skewTableau(new Partition from {6,3,2}, new Partition from {2}, toList(10..18))
    theList = positionList T
    T' = skewTableau(new Partition from {6,3,2}, new Partition from {2}, theList)
SeeAlso
  toPosition
  toIndex
///

doc ///
Key
    applyEntries
   (applyEntries, SkewTableau, Function)
Headline
   apply a function to all entries in the tableau
Usage
   applyEntries(T,f)
Inputs
    T:SkewTableau
      a tableau.
    f:Function
      acts on the entries of T.
Outputs
    T':SkewTableau
      a tableau with the same shape as T, and entries n -> f n.
Description
  Example
    T = skewTableau(new Partition from {6,3,2}, new Partition from {2}, toList(0..8))
    applyEntries(T, theBox -> theBox^2)
SeeAlso
  applyPositions
///

doc ///
Key
    applyPositions
   (applyPositions, SkewTableau, Function)
Headline
   apply a function to all positions of boxes in a tableau
Usage
   applyPositions(T,f)
Inputs
    T:SkewTableau
      a skew Tableau.
    f:Function
      acts on the entries of T.
Outputs
    T':SkewTableau
      a tableau with the same shape as T, and entries (i,j) -> f (i,j).
Description
  Example
    T = skewTableau(new Partition from {6,3,2}, new Partition from {2}, toList(0..8))
    applyPositions(T, thePosition -> thePosition)
    applyPositions(T, thePosition -> thePosition#1 - thePosition#0)
    applyPositions(T, thePosition -> (T_thePosition)^2)
SeeAlso
  applyEntries
///

doc ///
Key
   (components, SkewTableau)
Headline
   get the connected components of a tableau
Usage
   components T
Inputs
    T:SkewTableau
      a tableau.
Outputs
    l:List
      a list of all connected components of T.
Description
  Example
    T = skewTableau(new Partition from {6,2,2}, new Partition from {2}, toList(0..7))
    components T
SeeAlso
  (symbol ++, SkewTableau, SkewTableau)
///

doc ///
Key
   (symbol ++, SkewTableau, SkewTableau)
Headline
   direct sum of tableaux
Usage
   T1 ++ T2
Inputs
    T1:SkewTableau
      a tableau.
    T2:SkewTableau
      a tableau.
Outputs
    S:SkewTableau
      a direct sum of T1 and T2.
Description
  Text
    The tableaux are combined into the disconnected components (not sharing an edge) of a single tableau.
  Example
    T1 = skewTableau(new Partition from {5}, new Partition from {2})
    T2 = skewTableau(new Partition from {3,2}, new Partition from {1}, {1,2,3,4})
    T1 ++ T2
    T2 ++ T1
SeeAlso
  (components, SkewTableau)
  (symbol ||, SkewTableau, SkewTableau)
///

doc ///
Key
   (symbol ||, SkewTableau, SkewTableau)
Headline
   vertical concatenation of two tableaux
Usage
   T1 || T2
Inputs
    T1:SkewTableau
      a tableau.
    T2:SkewTableau
      a tableau.
Outputs
    S:SkewTableau
      a single tableau with T1 directly above T2.
Description
  Example
    T1 = skewTableau(new Partition from {5}, new Partition from {2})
    T2 = skewTableau(new Partition from {3,2}, new Partition from {1}, {1,2,3,4})
    T1 || T2
    T2 || T1
SeeAlso
  (symbol ++, SkewTableau, SkewTableau)
  verticalConcatenate
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
    S:SkewTableau
      a single tableau with combining those in the list.
Description
  Example
    T1 = skewTableau(new Partition from {5}, new Partition from {2})
    T2 = skewTableau(new Partition from {3,2}, new Partition from {1}, {1,2,3,4})
    T3 = skewTableau(new Partition from {5,-3})
    verticalConcatenate {T1,T2,T3}
SeeAlso
  (symbol ||, SkewTableau, SkewTableau)
///

doc ///
Key
    shift
   (shift, SkewTableau)
   (shift, SkewTableau, ZZ)
Headline
   shift a tableau
Usage
   shift T
   shift(T,n)
Inputs
    T:SkewTableau
      a tableau.
    n:ZZ
      an additional amount to shift.
Outputs
    S:SkewTableau
      each row i of T has been shifted by i + n to the right (where n=0 if not provided).
Description
  Example
    T = skewTableau(new Partition from {6,6,5,3,1})
    shift T
    shift(T,2)
SeeAlso
  unshift
///

doc ///
Key
    unshift
   (unshift, SkewTableau)
   (unshift, SkewTableau, ZZ)
Headline
   unshift a tableau
Usage
   unshift T
   unshift(T,n)
Inputs
    T:SkewTableau
      a skew tableau.
    n:ZZ
      an additional amount to shift.
Outputs
    S:SkewTableau
      each row i of T has been shifted by i + n to the left (where n=0 if not provided).
Description
  Example
    T = skewTableau(new Partition from {6,6,5,3,1})
    T' = shift T
    unshift T'
SeeAlso
  shift
///

doc ///
Key
   (conjugate, SkewTableau)
Headline
   conjugate a tableau
Usage
   conjugate T
Inputs
    T:SkewTableau
      a tableau.
Outputs
    S:SkewTableau
      the rows and columns have been switched.
Description
  Text
    This method conjugates the tableau only if the shape has weakly decreasing parts, and no negative parts.
    Otherwise, it will raise an error.
  Example
    T = skewTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1}, toList(1..17))
    conjugate T
///

doc ///
Key
    hookLength
   (hookLength, Sequence, SkewTableau)
Headline
   compute the hook length of a box of a tableau
Usage
   hookLength(thePosition,T)
Inputs
    thePosition:Sequence
      a pair (i,j) of integers.
    T:SkewTableau
      a tableau.
Outputs
    n:ZZ
      the hook length of box (i,j) in T.
Description
  Text
    The hook length of box (i,j) is defined as the number of boxes directly below, and directly to the right,
    of a box, including the box itself.
  Example
    T = skewTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    hookLength((1,1),T)
    applyPositions(T, thePosition -> hookLength(thePosition,T))
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
    T = skewTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    boxContent(1,1)
    applyPositions(T, thePosition -> boxContent thePosition)
///

doc ///
Key
    isWeaklyDecreasing
   (isWeaklyDecreasing, SkewTableau)
   (isWeaklyDecreasing, Partition, Partition)
   (isWeaklyDecreasing, Partition)
Headline
   check if shapes are weakly decreasing
Usage
   isWeaklyDecreasing T
   isWeaklyDecreasing (lam,mu)
   isWeaklyDecreasing lam
Inputs
    T:SkewTableau
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
    isWeaklyDecreasing skewTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    isWeaklyDecreasing new Partition from {5,3,-1,0}
///

doc ///
Key
    isNonnegative
   (isNonnegative, SkewTableau)
   (isNonnegative, Partition, Partition)
   (isNonnegative, Partition)
Headline
   check if shapes are nonnegative
Usage
   isNonnegative T
   isNonnegative (lam,mu)
   isNonnegative lam
Inputs
    T:SkewTableau
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
    isNonnegative skewTableau(new Partition from {6,6,5,3,1}, new Partition from {2,1,1})
    isNonnegative new Partition from {5,3,-1,0}
///

doc ///
Key
    allSemistandardTableaux
   (allSemistandardTableaux, Partition, Partition, ZZ)
   (allSemistandardTableaux, Partition, ZZ)
   (allSemistandardTableaux, Partition, Partition)
   (allSemistandardTableaux, Partition)
Headline
   list all semistandard Young tableaux of a given shape
Usage
   allSemistandardTableaux(lam,mu,N)
Inputs
    lam:Partition
      the outer shape, $\lambda$.
    mu:Partition
      the inner shape, $\mu$.
    N:ZZ
      the maximum entry.
Outputs
    b:Bag
      bagged list of all SSYT of given shape and entries in 1..N. If N is not provided, it is assumed
      to be the length of the shape.
Description
  Example
    theBag = allSemistandardTableaux(new Partition from {5,4,2}, new Partition from {3,1})
    for i from 0 to 4 do print theBag#i
  Text
    When the inner partition is $0$, the function @TO numSemistandardTableaux@ computes the number of SSYT via
    the hook-content formula.
  Example
    # allSemistandardTableaux(new Partition from {6,6,5,1,1})
    numSemistandardTableaux(new Partition from {6,6,5,1,1})
SeeAlso
  allSemistandardTableaux
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
///
