--- status: DRAFT
--- author(s): PZJ
--- notes: 

undocumented {(setupLift,Function,Ring,Ring,Function), (setupLift,RingMap,Ring,Ring)}

doc ///
 Key
  setupLift
  (setupLift,Function,Ring,Ring)
  (setupLift,RingMap)
 Headline
  set up lift from one ring to another
 Usage
  setupLift (f,R,S)
  setupLift g
 Inputs
  f: Function
  R: Ring
  S: Ring
  g: RingMap
 Description
  Text
   There are two possible ways of implementing @TO "lift"@ using @TT "setupLift"@. In the first one, we use a function:
  Example
   R=QQ[x]
   S=QQ[y]
   setupPromote map(R,S,{x^2})
   setupLift(a->sum(listForm a, (e,c)->if odd first e then error "not liftable" else c*y^(first e//2)),R,S)
   promote(y^2+2,R)
   lift(oo,S)
   lift(x,S,Verify=>false)
  Text
   In the second one, we define a ring map which is a partial inverse to the promotion map:
  Example
   R=QQ[x,y]
   S=R[s]
   setupPromote map(R,S,{x+y})
   setupLift map(S,R,{s,0})
   promote(s^3+2*s-1,R)
   lift(oo,S)
   lift(x,S,Verify=>false)
 SeeAlso
  setupPromote
///
