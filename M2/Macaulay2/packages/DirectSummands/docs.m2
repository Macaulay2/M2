doc ///
Node
  Key
    DirectSummands
  Headline
    decompositions of graded modules and coherent sheaves
  Description
    Text
      As an example, we prove the indecomposability of the @wikipedia "Horrocks–Mumford bundle"@ on $\PP^4$.
    Example
      needsPackage "BGG"
      S = ZZ/32003[x_0..x_4];
      E = ZZ/32003[e_0..e_4, SkewCommutative => true];
      alphad = map(E^5, E^{-2,-2}, transpose matrix{
	      { e_1*e_4, -e_0*e_2, -e_1*e_3, -e_2*e_4,  e_0*e_3},
	      {-e_2*e_3, -e_3*e_4,  e_0*e_4, -e_0*e_1, -e_1*e_2}});
      alpha = syz alphad;
      alphad = beilinson(alphad, S);
      alpha = beilinson(alpha, S);
      FHM = prune homology(alphad, alpha)
      assert(2 == rank FHM)
      -- initially ~30s for End(FHM), ~110s for basis; ~35s in ZZ/2; now down to ~1s total!
      assert({FHM} == summands FHM)
      assert FHM.cache.Indecomposable
  Acknowledgement
    The authors thank the organizers of the @HREF{"https://aimath.org/pastworkshops/macaulay2efie.html",
	"Macaulay2 workshop at AIM"}@, where significant progress on this package was made.
  Subnodes
    directSummands

Node
  Key
    directSummands
    (directSummands, Module)
    (directSummands, CoherentSheaf)
  Headline
    computes the direct summands of a graded module or coherent sheaf
  Usage
    summands M
  Inputs
    M:{Module,CoherentSheaf}
  Outputs
    :List
      containing modules or coherent sheaves which are direct summands of $M$
  Description
    Text
      This function attempts to find the indecomposable summands of a module or coherent sheaf $M$.
    Example
      S = QQ[x,y]
      M = coker matrix{{x,y},{x,x}}
      L = summands M
      assert first isIsomorphic(M, directSum L)
  SeeAlso
    findIdempotent
///

-- Template:
///
Node
  Key
  Headline
  Usage
  Inputs
  Outputs
  Consequences
    Item
  Description
    Text
    Example
    CannedExample
    Code
    Pre
  ExampleFiles
  Contributors
  References
  Caveat
  SeeAlso
--    Tree
--    CannedExample
--  Contributors
--  References
--  Caveat
--  SeeAlso
///
