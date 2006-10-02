newPackage (
     "LocalRings",
     Version => "0.1",
     Authors => {
	  {Name => "Michael E. Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"}
	  },
     Date => "October 2, 2006",
     Headline => { "a package for creating local rings and computing with them" },
     DebuggingMode => true
     )

export {
     localRing
     }

localRing = new method(
     Options => {
	  MonomialOrder => null
	  }
     )

localRing Ring := R -> localRing(R, ideal vars R)

localRing(Ring,Ideal) := (R,P) -> (
     R							    -- at first, we do nothing
     )
