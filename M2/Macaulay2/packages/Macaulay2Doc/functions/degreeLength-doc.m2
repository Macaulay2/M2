--- status: Draft
--- author(s): DRG, MES
--- notes: 

undocumented {
     (degreeLength, InexactField),
     (degreeLength,PolynomialRing), 
     (degreeLength,QuotientRing),
     (degreeLength,FractionField)
     }

doc ///
  Key
     degreeLength
    (degreeLength, Ring)
    (degreeLength, Monoid)
  Headline
    the length of the degree vector
  Usage
    degreeLength A
  Inputs
    A:{Ring,Monoid}
  Outputs
    :ZZ
      the length of a multidegree vector used as a degree of elements of @TT "A"@.
  Description
    Example
      degreeLength ZZ
      degreeLength(ZZ[])
      degreeLength(ZZ[x])
      degreeLength(ZZ[x, Degrees => {{1,2,3}}])
      degreeLength(ZZ[x,y,z, DegreeRank => 3])
      degreeLength(GF 9)
  SeeAlso
    degree
    degreesRing
    degreesMonoid
    [monoid, DegreeRank]
///
