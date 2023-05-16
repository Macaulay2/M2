-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  delete
  (delete, Thing, BasicList)
 Headline
  delete some elements of a list
 Usage
  delete(x, A)
 Inputs
  A:
   list or sequence
  x:
   thing
 Outputs
  A2:
    a new list from {\tt A} with every occurrence of {\tt x} removed
 Description
  Example
   delete(c, {a,b,c,d,e,a,b,c,d,e})
  Text
   Equality is determined with @TO"==="@, which is quick, but not always
   intuitive. For instance, in the next example, the first item in the list is
   {\bf not} removed, because it is an element of {\tt QQ} and will not match
   an element of {\tt ZZ}.
  Example
   delete(1, {2/2, 3/2, 4/2})
  Text
   To delete items from a list by index, rather than value, see @TO drop@.
 SeeAlso
  drop
  positions
  select
  "lists and sequences"
///