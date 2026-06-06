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
 SeeAlso
  "hash tables"
  applyKeys
  applyValues
  pairs  
  scanPairs
///
