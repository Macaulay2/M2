--- status: DRAFT
--- author(s): PZJ
--- notes: 

undocumented {(setupPromote,RingMap,Ring,Ring,Function), (setupPromote,Function,Ring,Ring,Function), (setupPromote,Function,Ring,Ring), (setupPromote,RingMap,Ring,Ring), (setupPromote,Ring,Ring)}


doc ///
 Key
  setupPromote
  (setupPromote,RingMap)
 Headline
  set up promote from one ring to another
 Usage
  setupPromote f
 Inputs
  f: RingMap
 Description
  Text
   This defines promotion from one ring to another as the application of a ring map.
   After calling @TT "setupPromote"@, any operation that is given an element of the source of @TT "f"@ but
   expects an element of the target of @TT "f"@ will automatically @TO "promote"@ it by applying @TT "f"@.
  Example
   R=QQ[x_1,x_2]
   R'=QQ[e_1,e_2,Degrees=>{1,2}]
   setupPromote map(R,R',{x_1+x_2,x_1*x_2})
   promote(e_1^2,R)
   e_1*x_1
   e_2==x_1*x_2
 SeeAlso
  setupLift
///
