-- author: Lily Silverstein

doc ///
 Key
  substring
  (substring, String, ZZ)
  (substring, ZZ, String)
  (substring, ZZ, ZZ, String)
  (substring, String, ZZ, ZZ)
  (substring, Sequence, String)
 Headline
  extract part of a string
 Usage
  substring(i, s)
  substring(i, n, s)
  substring(s, i, n)
  substring(s, i)
  substring((i, n), s)
 Inputs
  i:ZZ
   starting index of substring
  n:ZZ
   length of substring
  s:String
 Outputs
  :String
 Description
  Text
   The expressions {\tt substring(i, n, s)}, {\tt substring(s, i, n)}, and 
   {\tt substring((i, n), s)} all return the substring that starts at index 
   {\tt i} and has {\tt n} characters.
   
   In other words, if $s=s_0s_1s_2\ldots s_k$, then 
   the substring returned is $s_i s_{i+1}\ldots s_{i+n-1}$.
   
   Indices begin at zero, and a negative index is counted from
   the end of the string. Requests for out-of-bound character positions
   are silently ignored.
  Example
   s = "I love computing Groebner bases in Macaulay2.";
   substring(0, 5, s)
   substring(s, -10, 5)
   substring(s, 100, 5)
   substring((3, 10), s)
  Text
   The expressions {\tt substring(i, s)} and {\tt substring(s, i)} return
   the substring that starts at index {\tt i} and continues to the end of the string.

   In other words, if $s=s_0s_1s_2\ldots s_k$, then 
   the substring returned is $s_i s_{i+1}\ldots s_k$.

  Example
   substring(3, s)
   substring(s, -10)
 SeeAlso
  lines
  separate
  "strings and nets"
  "regular expressions"
///

