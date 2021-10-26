--- status: DRAFT
--- author(s): L.Gold, Lily Silverstein
--- notes: 

doc ///
 Key
  applyPairs
  (applyPairs,HashTable,Function)
 Headline
  apply a function to each pair in a hash table
 Usage
  applyPairs(H, f)
 Inputs
  H:HashTable
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
 Caveat	  
  It is an error for the function {\tt f} to return two pairs with the same key.
 SeeAlso
  "hash tables"
  applyKeys
  applyValues
  pairs  
  scanPairs
///
