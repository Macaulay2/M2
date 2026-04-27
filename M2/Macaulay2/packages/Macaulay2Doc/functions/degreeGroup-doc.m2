undocumented {
    (degreeGroup, FractionField),
}

doc ///
  Key
     degreeGroup
    (degreeGroup, Ring)
    (degreeGroup, Monoid)
    (degreeGroup, PolynomialRing)
    (degreeGroup, QuotientRing)
  Headline
    the degree group of a ring or monoid
  Usage
    degreeGroup A
  Inputs
    A:{Ring,Monoid}
  Outputs
    :Module -- over integers, representing a group
  Description
    Example
      degreeGroup ZZ
      degreeGroup(ZZ[])
      degreeGroup(ZZ[x])
      degreeGroup(ZZ[x, Degrees => {{1,2,3}}])
      degreeGroup(ZZ[x,y,z, DegreeRank => 3])
      degreeGroup(ZZ[x,y,z, DegreeGroup => ZZ^3])
      degreeGroup(ZZ[x,y,z, DegreeGroup => ZZ^2 ++ coker(2 * id_(ZZ^3))])
  SeeAlso
    degree
    degreeLength
    degreesRing
    degreesMonoid
    [monoid, DegreeGroup]
///
