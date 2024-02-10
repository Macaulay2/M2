-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  fold
  (fold, Function, Thing)
  (fold, Function, Thing, Thing)
  (fold, Function, VisibleList)
  (fold, VisibleList, Thing, Function)
  (fold, VisibleList, Function)
 Headline
  apply a binary operator repeatedly
 Usage
  fold(f, x, L)
  fold(f, L)
  fold(L, x, f)
  fold(L, f)
 Inputs
  f:Function
  x:Thing
  L:VisibleList
    or an instance of a class with the @TO iterator@ method installed
 Outputs
  M:List
 Description
  Text
   Suppose {\tt L=\{x0, x1, ..., xn\}}. Then for any binary operator {\tt f}, 
   {\tt fold(f, L)} computes {\tt f(...f(f(x0, x1), x2), ...)}. 
   In other words, the binary operator is applied
   to the first two elements of {\tt L}, then to that result along with the next unused element of
   {\tt L}, and so forth.
  Example
   fold(plus, {0,1,2,3,4,5})
   fold(identity, {a,b,c,d,e})
   fold((i,j) -> i|j|i, {"a","b","c","d","e"})
  Text
   If {\tt fold(f, x, L)} is called, the element {\tt x} is used as the first argument of the
   binary function {\tt f}. In other words, {\tt fold(f, \{x0, x1, \ldots, xn\})} is 
   equivalent to {\tt fold(f, x0, \{x1, \ldots, xn\})}.
  Example
   fold(plus, 0, {1,2,3,4,5})
   fold((x, y) -> x^y, 2, {3,2,1,2})
  Text
   The function {\tt fold(\{x_0, x_1, \ldots, x_n\}, f)} returns 
   {\tt f...f(f(x_{n-2}, f(x_{n-1}, x_n)))}. That is, {\tt f} is applied
   to the last two elements of the list first, then the repeated calls to
   {\tt f} proceed backwards through the list. The optional argument {\tt x} in
   {\tt fold(L, x, f)} is used as the second argument in the first evaluation of
   {\tt f}. So {\tt fold(\{x_0, x_1, \ldots, x_{n-1}\}, x_n, f)} is equivalent
   to {\tt fold(\{x_0, x_1, \ldots, x_n\}, f)}.
  Example
   fold({a,b,c,d,e}, identity)
   fold({a,b,c,d}, e, identity)  
   fold({2,3,2,1}, 2, (x, y) -> x^y)
  Text
   If @TT "L"@ is an instance of class the the @TO iterator@ method installed,
   e.g., a string, then it may also be used with @TT "fold"@, but only the
   versions with @TT "f"@ as the first argument.
  Example
   fold(identity, "abcde")
   fold(identity, "a", "bcde")
  Text
   The difference between @TO fold@ and {\tt accumulate} is that {\tt fold} returns the
   final result of all the nested evaluations of {\tt f}, while {\tt accumulate} lists 
   all the intermediate values as well.
  Example
   accumulate({2,3,2,1}, 2, (x, y) -> x^y)
 SeeAlso
  apply
  accumulate
  "lists and sequences"
///
