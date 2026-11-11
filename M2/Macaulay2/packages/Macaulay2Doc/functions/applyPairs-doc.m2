--- status: DRAFT
--- author(s): L.Gold, Lily Silverstein
--- notes: 

doc ///
 Key
  applyPairs
  (applyPairs,HashTable,Function)
  (applyPairs,BasicList,Function)
  (applyPairs,Dictionary,Function)
  (applyPairs,Thing,Function)
 Headline
  apply a function to each pair in a hash table
 Usage
  applyPairs(H, f)
 Inputs
  H:{HashTable, BasicList, Dictionary}
   or any instance of a class with an @TO iterator@ method installed
  f:Function
   of two arguments, returning a pair or @TO null@
 Outputs
  :HashTable
   obtained by applying {\tt f} to each key/value pair in {\tt H}
 Description
  Example
   H = new HashTable from {1 => 10, 2 => 15, 3 => 20}
   applyPairs(H, (k,v) -> (k+1, v+10))
   applyPairs(H, (k,v) -> (v,k))
  Text
   If @CODE "H"@ is not a hash table, then @M2CODE "apply(pairs H, f)"@ is
   called.
  Example
   applyPairs({4, 5, 6}, (i, x) -> i * x)
 Caveat	  
  It is an error for the function {\tt f} to return two pairs with the same key.

  When applied to @ofClass MutableHashTable@, this function does not lock it
  like most functions. The function argument should not modify the hash table
  as a side effect.  If it does, results may be incorrect or unpredictable. If
  such side effects are unavoidable, acquire your own @TO Mutex@ before
  calling this function.
 SeeAlso
  "hash tables"
  applyKeys
  applyValues
  pairs  
  scanPairs
///
