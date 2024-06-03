TEST ///
  -- tests for Euler sequence
  S = QQ[x,y,z]
  R = S/(x^2-y*z)
  X = Proj S
  Y = Proj R
  C = eulerSequence Proj R
  assert(C.dd_1 * C.dd_2 == 0)
  assert(homology(C.dd_1, C.dd_2) == 0)
  M = sheaf(module cotangentSheaf X ** R)
  assert first isIsomorphic(M, C_2)
///

end--
restart
loadPackage("Truncations", FileName => currentDirectory() | "Truncations.m2", Reload => true)
loadPackage("Complexes",   FileName => currentDirectory() | "Complexes.m2",   Reload => true)
debug loadPackage("Varieties",   FileName => currentDirectory() | "Varieties.m2",   Reload => true)

check Varieties
