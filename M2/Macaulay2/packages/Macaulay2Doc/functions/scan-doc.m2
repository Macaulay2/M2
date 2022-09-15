-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  scan
  (scan, BasicList, Function)
  (scan, BasicList, BasicList, Function)
  (scan, ZZ, Function)
  (scan, String, Function)
  (scan, Thing, Function)
 Headline
  apply a function to each element in a list or sequence
 Usage
  scan(L, f)
  scan(L, L', f)
  scan(n, f)
  scan(x, f)
 Inputs
  L: {BasicList, String}
  n: ZZ
  x: Thing -- an instance of an iterable class (see @TO "iterators"@)
  f: Function
 Description
  Text
   {\tt scan(L, f)} applies the function {\tt f} to each element
   of the list {\tt L}. The function values are discarded.
  Example
   scan({a, 4, "George", 2^100}, print)
   scan("foo", print)
  Text
   {\tt scan(n, f)} applies the function {\tt f} to each element 
   of the list 0, 1, ..., n-1
  Example
   scan(4, print)
   v = {a,b,c}; scan(#v, i -> print(i,v#i))
  Text
   The keyword @TO "break"@ can be used to terminate the scan prematurely,
   and optionally to specify a return value for the expression. Here we
   use it to locate the first even number in a list.
  Example
   scan({3,5,7,11,44,55,77}, i -> if even i then break i)
 SeeAlso
  apply
  accumulate
  fold
  "lists and sequences"
///
