-- status: rewritten September 2018
-- author: Lily Silverstein

doc ///
Node
 Key
   merge
  (merge, HashTable, HashTable, Function)
 Headline
  merge two hash tables
 Usage
  z = merge(x, y, f)
 Inputs
  x:HashTable
  y:HashTable
  f:Function
   of two variables specifying how to handle duplicate keys
 Outputs
  z:HashTable
   a new hash table whose keys are the union of 
   the keys of {\tt x} and the keys of {\tt y}
 Description
  Text
   Key-value pairs whose keys appear in only one of the input tables 
   appear unchanged in the output table. If the key {\tt k} appears in 
   both {\tt x} and {\tt y}, then {\tt f(x#k, y#k)} is the value
   associated to {\tt k} in the output hash table.
   
   For example, we could take the max of the two values, their average,
   or make a list containing both.
  Example
   x = new HashTable from {1 => 203, 2 => 21, 3 => 5, 4 => 130}
   y = new HashTable from {2 => 37, 3 => 5, 4 => 56, 5 => 1}
   merge(x, y, max)
   merge(x, y, (i,j) -> (i+j)/2)   
   merge(x, y, (i,j) -> {i,j})
  Text
   If the function {\tt f(x#k, y#k)} returns @TO "continue"@, then the key {\tt k} is
   omitted from the merged table.
  Example
   merge(x, y, (i,j) -> if i==j then i else continue)
  Text
   Here is a simple implementation of the free abelian group on 
   four letters, where each element is represented as
   a hash table that associates coefficients to strings.
  Example
   Free = new Type of HashTable
   p = new Free from { "x" => 2, "y" => 3, "z" => 5 }
   q = new Free from { "x" => 100, "y" => 200, "w" => 7 }
   Free + Free := (p,q) -> merge(p, q, plus);
   p+q
  Text
   If {\tt x} and {\tt y} have the same @TO class@ 
   and have the same @TO parent@, as in the previous example,
   then so will {\tt z}.
  Example
   x = new MutableHashTable from {"alice" => 53709, "bob" => 6549};
   y = new MutableHashTable from {"bob" => 86, "charlie" => 23};
   mutable merge(x, y, plus)
  Text
   The function @TO combine@ allows much greater control when combining
   two hash tables: you can give functions for how to handle every key and
   value of the input tables, not just the duplicates. The function
   @TO mergePairs@ is similar to {\tt merge}, but works on lists of pairs
   rather than hash tables.
 SeeAlso
  applyKeys
  applyPairs
  applyValues
  combine
  keys
  mergePairs
  pairs
  scanKeys
  scanPairs
  scanValues
  values
  "hash tables"

Node
  Key
    mergePairs
   (mergePairs, BasicList, BasicList, Function)
  Headline
    merge sorted lists of pairs
  Usage
    mergePairs(x,y,f)
  Inputs
    x:BasicList
    y:BasicList
    f:Function
  Outputs
    :BasicList
  Description
    Text
      This function merges sorted lists of pairs.

      It merges @TT "x"@ and @TT "y"@, which should be lists of pairs @TT "(k,v)"@
      arranged in increasing order according to the key @TT "k"@. The result will
      be a list of pairs, also arranged in increasing order, each of which is
      either from @TT "x"@ or from @TT "y"@, or in the case where a key @TT "k"@
      occurs in both, with say @TT "(k,v)"@ in @TT "x"@ and @TT "(k,w)"@ in @TT "y"@,
      then the result will contain the pair @TT "(k,f(v,w))"@. Thus the function
      @TT "f"@ is used for combining the values when the keys collide.

      The class of the result is taken to be the minimal common
      ancestor of the class of @TT "x"@ and the class of @TT "y"@.
  SeeAlso
    merge
///
