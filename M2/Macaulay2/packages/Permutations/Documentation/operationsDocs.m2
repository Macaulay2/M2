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