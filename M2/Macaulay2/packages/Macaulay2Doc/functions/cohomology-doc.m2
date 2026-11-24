--- status: draft
--- author(s): Sorin Popescu
--- notes: 

undocumented {(cohomology,ZZ,Sequence)}

doc ///
  Key
    cohomology
    [cohomology, Degree]
  Headline
    general cohomology functor
  Description
    Text
      @TT "cohomology"@ -- a method name available for computing expressions of
      the forms @TT "HH^i(X)"@ and @TT "HH^i(M,N)"@.

      If it is intended that @TT "i"@ be of class @TO "ZZ"@, @TT "M"@ be of
      class @TT "A"@, and @TT "N"@ be of class @TT "B"@, then the method can be
      installed with
    Pre
      cohomology(ZZ, A, B) := opts -> (i,M,N) -> ...
  SeeAlso
    "homology"
    "HH"
    "ScriptedFunctor"
  Subnodes
    (cohomology, ZZ, Module)
///

doc ///
  Key
    (cohomology,ZZ,Module)
  Headline
    local cohomology of a module
  Usage
    HH^i(M)
  Inputs
    i:
      which is non negative
    M:
      which is graded over its base polynomial ring
  Outputs
    :Module
  Description
    Text
      The command computes the local cohomology of the graded module @TT "M"@
      with respect to the maximal irrelevant ideal (the ideal of variables in
      the base ring of @TT "M"@).

      The package @TO "Dmodules::Dmodules"@ has alternative code to compute
      local cohomology (even in the non homogeneous case)

      A very simple example:
    Example
      R = QQ[a,b];
      HH^2 (R^{-3})
      HH^2 (R^{-4})
    Text
      Another example, a singular surface in projective fourspace (with one
      apparent double point):
    Example
      R = ZZ/101[x_0..x_4];
      I = ideal(x_1*x_4-x_2*x_3, x_1^2*x_3+x_1*x_2*x_0-x_2^2*x_0, x_3^3+x_3*x_4*x_0-x_4^2*x_0)
      M = R^1/module(I)
      HH^1(M)
      HH^2(M)
  Caveat
    There is no check made if the given module is graded over the base
    polynomial ring
  SeeAlso
    "Dmodules::Dmodules"
    "Varieties::HH^ZZ SumOfTwists"
    "Varieties::HH^ZZ CoherentSheaf"
///

