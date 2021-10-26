--- status: Rewritten July 2018
--- author(s): Lily Silverstein
--- notes: 

doc ///
 Key
  prepend
  (prepend, Thing, BasicList)
 Headline
  add an element to the beginning of a list
 Usage
  prepend(x, L)
 Inputs
  L:BasicList
  x:Thing
 Outputs
  :BasicList
   the list obtained by adding the element {\tt x} to the beginning of list {\tt L}
 Description
  Example
   prepend(3, {1, 7, 8, 3}) 
   L = {"old", "old", "old"};
   prepend("new", L)
  Text
   The new list will be of the same @TO class@ as {\tt L}.
  Example
   K = (a, b, c);
   prepend(z, K)
  Text
   Only a single element can be prepended with this function. To prepend the elements
   of a list, use @TO join@. To add the new element to the end of the list, or
   at a particular index, use @TO append@ or @TO insert@, respectively.
  Example
   join((x, y, z), K)
   append(K, z)
   insert(1, z, K)
  Text
   Prepend always returns a {\em new} list, rather than modifying the input list,
   even if {\tt L} is a @TO MutableList@.
  Example
   L = new MutableList from {2,3,5};
   peek prepend(7, L)
   peek L
  Text
   Notice that the order of the arguments is switched in {\tt prepend} versus {\tt append}:
   we write {\tt prepend(x, L)} and {\tt append(L, x)}. A good way to remember this is
   that the new element is visually placed {\em before} or {\em after} the list, depending
   on where we want it to appear in the output.
 SeeAlso
  append
  insert
  join
  "lists and sequences"
///