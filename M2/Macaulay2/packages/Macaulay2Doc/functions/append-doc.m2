--- status: Rewritten July 2018
--- author(s): Lily Silverstein
--- notes: 

doc ///
 Key
  append
  (append, BasicList, Thing)
 Headline
  add an element to the end of a list
 Usage
  append(L, x)
 Inputs
  L:BasicList
  x:Thing
 Outputs
  :BasicList
   the list obtained by adding the element {\tt x} to the end of list {\tt L}
 Description
  Example
   append({1, 7, 8, 3}, 3) 
   L = {"old", "old", "old"};
   append(L, "new")
  Text
   The new list will be of the same @TO class@ as {\tt L}.
  Example
   K = (a, b, c);
   append(K, z)
  Text
   Only a single element can be appended with this function. To append the elements
   of a list, use @TO join@. To add the new element to the beginning of the list, or
   at a particular index, use @TO prepend@ or @TO insert@, respectively.
  Example
   join(K, (x, y, z))
   prepend(z, K)
   insert(1, z, K)
  Text
   Append always returns a {\em new} list, rather than modifying the input list,
   even if {\tt L} is a @TO MutableList@.
  Example
   L = new MutableList from {2,3,5};
   peek append(L, 7)
   peek L
  Text
   Notice that the order of the arguments is switched in {\tt prepend} versus {\tt append}:
   we write {\tt prepend(x, L)} and {\tt append(L, x)}. A good way to remember this is
   that the new element is visually placed {\em before} or {\em after} the list, depending
   on where we want it to appear in the output.
 SeeAlso
  insert
  join
  prepend
  "lists and sequences"
///