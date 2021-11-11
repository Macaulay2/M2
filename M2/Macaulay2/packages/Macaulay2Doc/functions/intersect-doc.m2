--- status: Draft
--- author(s):Giulio
--- notes: 

doc ///
Node
  Key
     (intersect, Ideal, Ideal)
    [(intersect, Ideal, Ideal), Strategy]
    [(intersect, Ideal, Ideal), MinimalGenerators]
     (intersect, Ideal)
     (intersect, Module, Module)
    [(intersect, Module, Module), Strategy]
    [(intersect, Module, Module), MinimalGenerators]
     (intersect, Module)
  Headline
    compute an intersection of a sequence of ideals or modules
  Usage
    intersect(M, N)
    intersect(M, N, ..., P)
  Inputs
    :{Ideal,Module}
    :{Ideal,Module}
      submodules of the same module or ideals in the same ring
    Strategy=>Thing
      specifies the algorithm
    MinimalGenerators=>Boolean
      indicates whether the output should be @TO2 {trim, "trimmed"}@
  Outputs
    :{Ideal,Module}
      the intersection of the sequence of objects
  Description
    Text
      This function calculates the intersection of submodules of the same free module, or of ideals in the same ring.

      The following example computes the intersection of a sequence of ideals.
    Example
      R = ZZ/101[a..d];
      I = intersect(ideal(a, b), ideal(b, c), ideal(c, d), ideal(d, a))
    Text
      The following example computes the intersection of a list of modules.
    Example
      R=ZZ[x, y, z];
      M=image matrix{{3*x}, {3*x}};
      N=image matrix{{5*y}, {5*y}};
      P=image matrix{{7*z}, {7*z}};
      intersect{M, N, P}
    Text
      The command @TO "intersect"@ does not accept subrings. To intersect an ideal with a subring,
      use @TO "selectInSubring"@ along with the elimination ordering, see @TO "Eliminate"@.

      Multiple strategies are implemented via @TO2 {"Macaulay2Doc :: using hooks", "hooks"}@ and
      can be listed using the function @TO hooks@. More strategies may be added using @TO addHook@.
    Example
      hooks(intersect, Ideal, Ideal)
      hooks(intersect, Module, Module)
    Text
      By default, the strategies are attempted in the reverse order in which the were added, until one is successful.
      To run a specific strategy instead, use the optional argument @TT "Strategy"@.
    Example
      intersect(ideal(x, y), (ideal(x, y, z))^2, Strategy => Monomial)
  SeeAlso
    selectInSubring
///
