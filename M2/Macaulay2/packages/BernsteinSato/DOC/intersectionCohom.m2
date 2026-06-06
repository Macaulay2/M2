doc ///
Node
  Key
    intersectionCohomology
    IH
    (intersectionCohomology, Ideal)
    (intersectionCohomology, ZZ, Ideal)
    [intersectionCohomology, Strategy]
    [intersectionCohomology, LocStrategy]
    [intersectionCohomology, LocCohomStrategy]
  Headline
    intersection cohomology of an irreducible affine variety
  Usage
    IH I
    intersectionCohomology(I)
    IH^d I
    intersectionCohomology(d,I)
  Inputs
    I:Ideal
      in the polynomial ring $R$
    d:ZZ
      the degree
    Strategy=>String
       see @TO [Dintegration, Strategy]@
    LocCohomStrategy=>Sequence -- (String, String)
       see @TO [localCohom, Strategy]@ and @TO [localCohom, LocStrategy]@
    LocStrategy=>String
       see @TO [IHmodule, LocStrategy]@
  Outputs
     :HashTable
      a table including the intersection cohomology groups of the irreducible variety Spec($R/I$), 
     :Module
      the intersection cohomology group in degree d
  Description
    Text
     This routine computes the middle intersection cohomology groups of the irreducible variety defined by $I$ in the affine space Spec($R$).
    Example
      R=QQ[x,y]
      I = ideal(x^2+y^3)
      intersectionCohomology(I)
  Caveat
     Must be over a ring of characteristic 0. The ideal $I$ should have only 1 minimal prime.

Node
  Key
    IHmodule
    (IHmodule, Ideal)
    [IHmodule, LocStrategy]
    [IHmodule, LocCohomStrategy]
    LocCohomStrategy
  Headline
    intersection (co)homology module of an irreducible closed subvariety
  Usage 
     IHmodule(I)
  Inputs
     I:Ideal
       ideal in the polynomial ring $R$
     LocCohomStrategy=>Sequence -- (String, String)
       see @TO [localCohom, Strategy]@ and @TO[localCohom, LocStrategy]@
     LocStrategy=>String
       see @TO [Dlocalize, Strategy]@, or for regular sequence use CompleteIntersection
  Outputs
     :Module
        the intersection cohomology $D$-module
  Description
    Text
       This routine gives a presentation of the Brylinski-Kashiwara intersection cohomology $D$-module of the closed subvariety defined by $I$. Via the Riemann-Hilbert correspondence, this corresponds to the trivial local system on the smooth locus of the variety.
    Example
       R=QQ[x,y,z]
       I=ideal(x^2+y^3)
       IHmodule(I)
    Text 
       When the given generators of $I$ form a regular sequence, use LocStrategy=>CompleteIntersection for a generally faster algorithm, which implements the determination of the IC module in terms of the fundamental class as described in: 
       D. Barlet and M. Kashiwara, Le réseau $L^2$ d’un système holonome régulier, Invent. Math. 86 (1986), no. 1, 35–62. 
    Example
       R=QQ[x,y]
       I=ideal(x^2+y^3)
       IHmodule(I, LocStrategy=>CompleteIntersection)
  
  Caveat
      Must be a ring of characteristic 0. The ideal $I$ should have only 1 minimal prime.
  
///
