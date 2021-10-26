--- status: DRAFT
--- author(s): L.Gold, Lily Silverstein
--- notes:

doc ///
 Key
  applyKeys
  (applyKeys, HashTable, Function)
  (applyKeys, HashTable, Function, Function)
 Headline
  apply a function to each key in a hash table
 Usage
  applyKeys(H, f)
  applyKeys(H, f, g)
 Inputs
  H:HashTable
  f:Function
   of one argument
  g:Function
   of two arguments, for collision handling
 Outputs
  :HashTable
   obtained by applying {\tt f} to each key in {\tt H}
 Caveat
  It is an error for the function {\tt f} to return two pairs with the same key.
  When this is a possibility, use the function {\tt g} to specify how the two
  pairs should be reconciled.
 Description
  Example
   H = new HashTable from {1 => 10, 2 => 15, 3 => 20}
   applyKeys(H, k -> k + 100)
   applyKeys(H, k -> k//2, max)
   applyKeys(H, k -> k//2, plus)
 SeeAlso
  "hash tables"
  applyPairs
  applyValues
  keys
  scanKeys
///