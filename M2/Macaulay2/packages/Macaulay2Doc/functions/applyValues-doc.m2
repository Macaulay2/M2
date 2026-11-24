--- status: DRAFT
--- author(s): L.Gold, Lily Silverstein
--- notes: 

doc ///
 Key
  applyValues
  (applyValues, HashTable, Function)
 Headline
  apply a function to each value in a hash table
 Usage
  applyValues(H, f)
 Inputs
  H:HashTable
  f:Function
   of one argument
 Outputs
  :HashTable
   obtained by applying {\tt f} to each value in {\tt H}
 Description
  Example
   H = new HashTable from {1 => 10, 2 => 15, 3 => 20}
   applyValues(H, v -> v + 100)
   applyValues(H, v -> 1)
 Caveat
  When applied to @ofClass MutableHashTable@, this function does not lock it
  like most functions. The function argument should not modify the hash table
  as a side effect.  If it does, results may be incorrect or unpredictable. If
  such side effects are unavoidable, acquire your own @TO Mutex@ before
  calling this function.
 SeeAlso
  "hash tables"
  applyKeys
  applyPairs
  scanValues
  values
///
