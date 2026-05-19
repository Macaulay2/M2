--- status: DRAFT
--- author(s): PZJ
--- notes:

doc ///
 Key
  setupPromote
  (setupPromote,RingMap)
  (setupPromote,RingMap,Ring,Ring,Function)
  (setupPromote,Function,Ring,Ring,Function)
  (setupPromote,Function,Ring,Ring)
  (setupPromote,RingMap,Ring,Ring)
  (setupPromote,Ring,Ring)
 Headline
  set up promote from one ring to another
 Usage
  setupPromote f
  setupPromote(f, R, S)
  setupPromote(f, R, S, d)
  setupPromote(R, S)
 Inputs
  f:{RingMap, Function}
  R:Ring
  S:Ring
  d:Function
 Description
  Text
   This defines promotion from one ring @VAR "R"@ to another ring @VAR "S"@ as
   the application of a ring map or function @VAR "f"@.  After calling
   @M2CODE "setupPromote"@, any operation that is given an element of @VAR "R"@
   but expects an element of @VAR "S"@ will automatically @TO "promote"@ it by
   applying @VAR "f"@.

   Note that @VAR "R"@ and @VAR "S"@ must be specified when @VAR "f"@ is
   @ofClass Function@, but may be omitted when it is @ofClass RingMap@.
  Example
   R=QQ[x_1,x_2]
   R'=QQ[e_1,e_2,Degrees=>{1,2}]
   setupPromote map(R,R',{x_1+x_2,x_1*x_2})
   promote(e_1^2,R)
   e_1*x_1
   e_2==x_1*x_2
  Text
   If @VAR "f"@ is omitted, then the map created by @TO (map, Ring, Ring)@
   that maps variables in @VAR "R"@ to variables in @VAR "S"@ with the same name
   is used.
  Example
   R = QQ[x]
   S = QQ[x,y]
   setupPromote(R, S)
   promote(R_0, S)
  Text
   The optional argument @VAR "d"@ is a function that translates degrees
   from @VAR "R"@ to @VAR "S"@. It is used to set up
   @M2CODE "promote(List, R, S)"@, which translates degree lists when
   promoting free modules. When @VAR "f"@ is @ofClass Function@, there is
   no @TO DegreeMap@ option available, so @VAR "d"@ must be specified
   explicitly if the degree map is not the default (which pads or truncates
   degrees).  Consider the following example.
  Example
   R = QQ[x]
   S = QQ[a, b, Degrees => {{1,0},{0,1}}]
   setupPromote(r -> sub(r, x => b), R, S)
   promote(R^{1,2}, S) -- wrong degrees!
   setupPromote(r -> sub(r, x => b), R, S, d -> {0, d#0})
   promote(R^{1,2}, S)
 SeeAlso
  setupLift
///
