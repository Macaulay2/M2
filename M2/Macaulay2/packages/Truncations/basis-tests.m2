TEST ///
  A = QQ[x,y]
  assert(basis_-2 A == map(A^1,A^0,0))
  assert(basis_0 A  == map(A^1,A^1,1))
  assert try (basis A; false) else true
  assert(basis_2 A  == matrix"x2,xy,y2")

  B = A/ideal"x2,xy,y2"
  assert(basis_-2 B == map(B^1,B^0,0))
  assert(basis_0 B  == map(B^1,B^1,1))
  assert(basis   B  == matrix"1,x,y")
  assert(basis_1 B  == matrix"x,y")

  C = QQ[x,y, Degrees => {1,0}]
  assert(basis_-2 C == map(C^1,C^0,0))
  assert(basis_0 C  == map(C^1,C^1,1))
  assert try (basis C; false) else true
  assert(basis_2 C  == matrix"x2")

  D = QQ[x,y, Degrees => {0,1}]
  assert(basis_-2 D == map(D^1,D^0,0))
  assert(basis_0 D  == map(D^1,D^1,1))
  assert try (basis D; false) else true
  assert(basis_2 D  == matrix"y2")
///

TEST ///
  A = QQ[x,y, DegreeRank => 2]
  assert(basis_-2 A == map(A^1,A^0,0))
  assert(basis_0 A  == map(A^1,A^1,1))
  assert try (basis A; false) else true
  assert(basis_2 A  == matrix"x2")
  assert(basis_{0,0} A  == map(A^1,A^1,1))
  assert(basis_{0,2} A  == matrix"y2")

  debug needsPackage "Truncations"
  B = A[w,z]
  -- FIXME in feature/hilbert
  -- basis(1, B)
  -- basis({1,0,0}, B)
  -- hilbertFunction({1,0,0}, B)
  basis'({1,0,0}, module B)
  basis({1,0,0}, first flattenRing B)
  hilbertFunction({1,0,0}, first flattenRing B)
  basis'({0,1,0}, module B)
  basis({0,1,0}, first flattenRing B)
  hilbertFunction({0,1,0}, first flattenRing B)
  basis'({0,0,1}, module B)
  basis({0,0,1}, first flattenRing B)
  hilbertFunction({0,0,1}, first flattenRing B)
///

TEST ///
  debug needsPackage "Truncations"
  A = QQ[x,y, Degrees => {{1,1},{1,2}}]
  basis({3}, A)
  basis'({3,2}, module A, "partial degrees" => {})
  basis'({3,2}, module A, "partial degrees" => {0})
  basis'({3,2}, module A, "partial degrees" => {1})
  basis'({3,2}, module A, "partial degrees" => {0,1})
  -- FIXME: caching is broken
  basis'({3,2}, module A)
  truncate({3,0}, module A)
  -- TODO: can we do partial multidegrees for hilbertFunction?
  sum apply(7, i -> hilbertFunction({3,i}, A))
///
