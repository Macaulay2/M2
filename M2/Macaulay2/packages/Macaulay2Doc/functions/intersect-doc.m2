--- status: Draft
--- author(s):Giulio
--- notes: 


doc ///
Node
  Key
    intersect
  Headline
    compute an intersection
Node
  Key
    (intersect, List)
    (intersect, Sequence)
    (intersect, Ideal)
    (intersect, Module)
    [intersect, Strategy]
  Headline
    compute an intersection of ideals or modules
  Usage
    intersect(M, N, ..., P)
  Inputs
    :{List,Sequence}
      containing modules that are submodules of the same module or ideals in the same ring
    Strategy =>
      specifies the algorithm
  Outputs
    :{Ideal,Module}
      the intersection of the objects given
  Description
    Text
      This function calculates the intersection of submodules of the same free module, or of ideals in the same ring.

      The following example computes the intersection of a sequence of ideals.
    Example
      R=ZZ/101[a..d];
      I=intersect(ideal(a, b), ideal(b, c), ideal(c, d), ideal(d, a))
    Text
      The following example computes the intersection of a list of modules.
    Example
      R=ZZ[x, y, z];
      M=image matrix{{3*x}, {3*x}};
      N=image matrix{{5*y}, {5*y}};
      P=image matrix{{7*z}, {7*z}};
      intersect{M, N, P}
    Text
      The command @TO "intersect"@ will only work with proper ideals. To intersect an ideal with a ring,
      use @TO "selectInSubring"@ along with the elimination ordering, see @TO "Eliminate"@.
///
