-- Coverage tests for exports that previously had no direct test
-- (stellarSubdivision, faceFan, objectiveVector, polarFace, and the deprecated
-- shims triangulate / cellDecompose), together with stress tests that exercise
-- the core constructions over a range of parameters and check standard
-- polyhedral invariants.

-- stellarSubdivision: subdividing the normal fan of the square at the interior
-- ray (1,1) splits one maximal cone in two and keeps the fan complete.
-- error test: the ray must be a one-column matrix with ambDim-many rows.
TEST ///
NF = normalFan hypercube 2
ssF = stellarSubdivision(NF, matrix{{1},{1}})
assert(instance(ssF, Fan))
assert(isComplete ssF)
assert(ambDim ssF == 2)
assert(#maxCones ssF == #maxCones NF + 1)
assert(try (stellarSubdivision(NF, matrix{{1},{1},{1}}); false) else true)
///

-- faceFan: the fan of cones over the facets of a polytope that contains the
-- origin in its interior. It is a complete fan with one maximal cone per facet.
-- error test: faceFan rejects a polytope without the origin in its interior.
TEST ///
for P in {hypercube 2, hypercube 3, crossPolytope 3} do (
   F = faceFan P;
   assert(instance(F, Fan));
   assert(isComplete F);
   assert(ambDim F == dim P);
   assert(#maxCones F == #(faces(1, P)))
   )
assert(try (faceFan simplex 2; false) else true)
///

-- objectiveVector(P, Q): for a face Q of P it returns a weight vector whose
-- maximal face on P is exactly Q.
-- error test: the second argument must be a face of the first.
TEST ///
P = hypercube 3
for Q in facesAsPolyhedra(1, P) do (
   v = objectiveVector(P, Q);
   assert(instance(v, Matrix));
   assert(maxFace(v, P) == Q)
   )
assert(try (objectiveVector(hypercube 2, hypercube(2,0,1)); false) else true)
///

-- polarFace(f, P): the face of the polar polytope dual to a face f of P.
-- Polar duality reverses dimension: dim polarFace(f,P) = dim P - 1 - dim f.
TEST ///
for P in {hypercube 3, crossPolytope 3} do (
   d = dim P;
   for j from 1 to d do (
      for f in facesAsPolyhedra(j, P) do (
         pf = polarFace(f, P);
         assert(instance(pf, Polyhedron));
         assert(dim pf == d - 1 - dim f)
         )
      )
   )
///

-- triangulate and cellDecompose are deprecated shims; they must agree with the
-- methods they delegate to (barycentricTriangulation and regularSubdivision).
TEST ///
P = hypercube 2
assert(triangulate P == barycentricTriangulation P)
U = hypercube(2, 0, 1)
w = matrix {{0,0,0,1}}
assert(cellDecompose(U, w) == regularSubdivision(U, w))
///

-- Stress test: the d-cube [-1,1]^d for d = 1..5. Checks dimension, vertex count
-- 2^d, volume 2^d, reflexivity, the f-vector shape, the Euler relation (the
-- alternating sum of the f-vector is 1), and that polar is an involution.
TEST ///
for d from 1 to 5 do (
   P = hypercube d;
   assert(dim P == d and ambDim P == d);
   assert(isCompact P and isFullDimensional P and isLatticePolytope P);
   assert(numColumns vertices P == 2^d);
   assert(nVertices P == 2^d);
   assert(volume P == 2^d);
   assert(isReflexive P);
   fv = fVector P;
   assert(#fv == d+1 and first fv == 2^d and last fv == 1);
   assert(sum(#fv, i -> (-1)^i * fv#i) == 1);
   assert(polar polar P == P)
   )
///

-- Stress test: cross-polytopes, simplices and cyclic polytopes over a range of
-- parameters. Checks the cross-polytope / cube polar duality, that simplices
-- and cyclic polytopes are simplicial, simplex volume 1/d!, the Euler relation.
TEST ///
for d from 1 to 5 do (
   X = crossPolytope d;
   assert(dim X == d and numColumns vertices X == 2*d);
   assert(isSimplicial X);
   assert(polar X == hypercube d);
   S = simplex d;
   assert(dim S == d and numColumns vertices S == d+1);
   assert(volume S == 1/d!);
   assert(isSimplicial S)
   )
for n from 4 to 8 do (
   C = cyclicPolytope(3, n);
   assert(dim C == 3 and numColumns vertices C == n);
   assert(isSimplicial C);
   fv = fVector C;
   assert(sum(#fv, i -> (-1)^i * fv#i) == 1)
   )
///

-- Stress test: the positive orthant in n-space for n = 1..6 is a pointed,
-- simplicial, full-dimensional cone, and cone duality is an involution.
TEST ///
for n from 1 to 6 do (
   C = posOrthant n;
   assert(dim C == n and ambDim C == n);
   assert(isPointed C and isSimplicial C and isFullDimensional C);
   assert(dualCone dualCone C == C)
   )
///

-- Stress test: the normal fan of a compact full-dimensional polytope is
-- complete and pure, and every Hirzebruch surface is a complete pure fan with
-- four maximal cones.
TEST ///
for P in {hypercube 2, hypercube 3, crossPolytope 2, crossPolytope 3} do (
   nf = normalFan P;
   assert(isComplete nf and isPure nf)
   )
for r from 0 to 4 do (
   H = hirzebruch r;
   assert(instance(H, Fan));
   assert(isComplete H and isPure H);
   assert(#maxCones H == 4)
   )
///
