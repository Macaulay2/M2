doc///
 Key
  table
 Headline
  make a table from a binary function
 Usage
  table(a, b, f)
  table(m, n, f)
 Inputs
  a:List
  b:List
  m:ZZ
  n:ZZ
  f:
   a function {\tt f(i,j)} of two variables
 Outputs
  T:
   a table, or list of lists, where $T_{ij}$ is the value
   of $f(a_i, b_j)$, OR, if using integer arguments $m$ and $n$,
   $T_{ij}=f(i,j)$ for $0\le i < m, 0\le j < n$
 Description
  Text
   The command {\tt table(m, n, f)} is equivalent to
   {\tt table(0..(m-1), 0..(n-1), f)}.
  Example
   t1 = table({1,3,5,7}, {0,1,2,4}, (i,j) -> i^j)
   t2 = table(5, 5, (i,j) -> i+j)
  Text
   Tables can be displayed nicely using @TO netList@.
  Example
   netList t1
 SeeAlso
  applyTable
  isTable
  subtable
  "lists and sequences"
///

doc///
 Key
  applyTable
 Headline
  apply a function to each element of a table
 Usage
  applyTable(T, f)
 Inputs
  T:List
   a table (list of lists of the same length)
  f:Function
 Outputs
  A:List
   a table of the same shape as $T$, where the function
   $f$ has been applied elementwise
 Description
  Example
   t = table({1,3,5,7}, {0,1,2,4}, (i,j) -> i^j);
   netList t
   netList applyTable(t, i -> 2*i)
   netList applyTable(t, isPrime)
 SeeAlso
  isTable
  subtable
  table
  "lists and sequences"
///

doc ///
 Key
  subtable
 Headline
  extract a subtable from a table
 Usage
  subtable(a, b, T)
 Inputs
  a:List
   of rows to extract
  b:List
   of columns to extract
 Outputs
  S:
   the subtable of $T$ defined by restricting to rows in the list $a$
   and columns in the list $b$
 Description
  Example
   t = table({1,3,5,7}, {0,1,2,4}, (i,j) -> i^j);
   netList t
   s1 = subtable({0,2}, {1,3}, t);
   netList s1
   s2 = subtable(toList(0..3), {1}, t);
   netList s2
 SeeAlso
  applyTable
  isTable
  positions
  table
  select
  "lists and sequences"
///

doc///
 Key
  isTable
 Headline
  whether something is a list of lists of equal length
 Usage
  isTable t
 Inputs
  t:Thing
 Outputs
  b:Boolean
   whether or not $t$ is a table
 Description
  Example
   isTable {{1,2,3},{4,5,6}}
   isTable {{1,2,3},{4,5}}
 Caveat
  It is intrinsically impossible to represent a $0\times k$ matrix
  as a list of lists.
 SeeAlso
  applyTable
  table
  subtable
  "lists and sequences"
///
