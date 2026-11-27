--- status: Draft
--- author(s): Gregory G. Smith
--- notes: 

doc ///
  Key
    isRing
    (isRing,Ring)
    (isRing, Thing)
  Headline
    whether something is a ring
  Usage
    isRing R
  Inputs
    R:Thing
  Outputs
    :Boolean
      @TO "true"@ if @TT "R"@ is a @TO2("Ring", "ring")@ and @TO "false"@
      otherwise
  Description
    Example
      isRing QQ
      R = QQ[x,y];
      isRing(R)
      isRing(R^1)
      isRing(GF(2,3)[x,dx, WeylAlgebra => {x => dx}])
///
