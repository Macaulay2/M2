--- status: moved August 2022
--- author(s):
--- notes: implemented in newring.m2

doc ///
Node
  Key
     newRing
    (newRing, QuotientRing)
    (newRing, PolynomialRing)
    [newRing, DegreeLift]
    [newRing, DegreeMap]
    [newRing, DegreeRank]
    [newRing, DegreeGroup]
    [newRing, Degrees]
    [newRing, Global]
    [newRing, Heft]
    [newRing, Constants]
    [newRing, Inverses]
    [newRing, Join]
    [newRing, Local]
    [newRing, MonomialOrder]
    [newRing, MonomialSize]
    [newRing, VariableBaseName]
    [newRing, Variables]
    [newRing, SkewCommutative]
    [newRing, Weights]
    [newRing, WeylAlgebra]
  Headline
    make a copy of a ring, with some features changed
  Usage
    S = newRing(R, options)
  Inputs
    R:{PolynomialRing,QuotientRing}
    Variables        => List    -- see @TO [monoid, Variables]@
    VariableBaseName => Symbol  -- see @TO [monoid, VariableBaseName]@
    Global           => Boolean -- see @TO [monoid, Global]@
    Local            => Boolean -- see @TO [monoid, Local]@
    Inverses         => Boolean -- see @TO [monoid, Inverses]@
    Weights          => List    -- see @TO [monoid, Weights]@
    Degrees          => List    -- see @TO [monoid, Degrees]@
    DegreeMap        => Boolean -- see @TO [monoid, DegreeMap]@
    DegreeLift       => Boolean -- see @TO [monoid, DegreeLift]@
    DegreeRank       => ZZ      -- see @TO [monoid, DegreeRank]@
    Heft             => List    -- see @TO [monoid, Heft]@
    Join             => Boolean -- see @TO [monoid, Join]@
    Constants        => Boolean -- see @TO [monoid, Join]@
    MonomialOrder    => List    -- see @TO [monoid, MonomialOrder]@
    MonomialSize     => ZZ      -- see @TO [monoid, MonomialSize]@
    SkewCommutative  => Boolean -- see @TO [monoid, SkewCommutative]@
    WeylAlgebra      => List    -- see @TO [monoid, WeylAlgebra]@
  Outputs
    S:Ring
      a new ring, constructed in the same way @TT "R"@ was, over the same coefficient
      ring, but with the newly specified options overriding those used before.
      See @TO monoid@ for a description of those options. If @TT "R"@ was a quotient
      ring, then the number of variables must be the same, and S will be a quotient
      ring, too, with defining ideal obtained from the old by substituting the new
      variables for the old, preserving their order.
  Description
    Text
      If a different number of variables is given with @TO Variables@, then the list
      of degrees in @TT "R"@ will be ignored.  If a new degree rank is specified with
      @TO DegreeRank@ then the list of degrees and the heft vector of @TT "R"@ will be
      ignored.  If a new nonempty list of degrees is specified with @TO Degrees@, then
      the degree rank and the heft vector of @TT "R"@ will be ignored.
    Example
       R = QQ[x,y, MonomialOrder => Lex, Degrees => {3,5}];
       describe newRing(R, MonomialOrder => GRevLex)
       describe newRing(R, Variables => 4)
       describe newRing(R, Heft => {2})
       S = R/(x^2+y^3);
       describe newRing(R, Variables => 2)
    Text
      The default values for the options of @TT "newRing"@ are all set to a
      non-accessible private symbol whose name is @TT "nothing"@.
///
