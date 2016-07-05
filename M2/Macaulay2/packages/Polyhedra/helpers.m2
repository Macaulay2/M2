spanSmoothCone = method();
spanSmoothCone(Matrix, Matrix) := (rays, lineality) -> (
   -- The rays need to form a lattice basis in N/L
   d := rank(rays || lineality);
   spanSmoothCone(rays, lineality, d)
)

spanSmoothCone(Matrix, Matrix, ZZ) := (rays, lineality, d) -> (
   n := d - numRows lineality;
   SNF := (smithNormalForm (rays || lineality))#0; 
   numRows rays == n and (product apply(d, i -> SNF_(i,i)) == 1)
)

scalarProduct = method()
scalarProduct(Vector, Vector) := (v1, v2) -> (
   if not (rank module v1) == (rank module v2) then error("Vectors have different dimension.");
   n := rank module v1;
   result := for i from 0 to n-1 list (
      v1_i * v2_i
   );
   sum result
)

scalarProduct(Matrix, Matrix) := (m1, m2) -> (
   if not ((numColumns m1 == 1) and (numColumns m2 == 1))then error("Only column matrices allowed");
   scalarProduct(m1_0, m2_0)
)

rayCorrespondenceMap = method()
rayCorrespondenceMap(Matrix, Matrix) := (sources, targets) -> (
   L := for i from 0 to (numColumns sources -1) list (
      source := sources_{i};
      corresponding := positions(0..(numColumns targets -1),
         j -> (
            target := targets_{j};
            -- << target << endl;
            -- << rank(source | target) == 1 << endl;
            (rank(source | target) == 1) and scalarProduct(source, target) > 0
         )
      );
      if #corresponding == 1 then i=>corresponding#0
      else if #corresponding == 0 then i=>-1
      else error("Too many possible targets.")
   );
   new HashTable from L
)


slice = method()
slice(Vector, List) := (v, L) -> (
   result := entries v;
   result = result_L;
   vector result
)


slice(Vector, Sequence) := (v, S) -> (
   slice(v, toList S)
)


matrixFromVectorList = method()
matrixFromVectorList(List, ZZ, Ring) := (L, dim, r) -> (
   if #L > 0 then return matrix L
   else return map(r^dim, r^0, 0)
)


prependOnes = method()
prependOnes Matrix := M -> (
   r := ring M;
   map(r^1, source M, (i,j) -> 1) || M
)


prependZeros = method()
prependZeros Matrix := M -> (
   r := ring M;
   map(r^1, source M, (i,j) -> 0) || M
)



orthogonalComplement = method()
orthogonalComplement Matrix := M -> (
   gens kernel M
)


computeRaysFromFacetData = method()
computeRaysFromFacetData(Matrix, Matrix) := (facetData, hyperplaneData) -> (
   fourierMotzkin(transpose(-facetData), transpose(hyperplaneData))
)


computeFacetsFromRayData = method()
computeFacetsFromRayData(Matrix, Matrix) := (rayData, linealityData) -> (
   (A, B) := fourierMotzkin(rayData, linealityData);
   (transpose(-A), transpose(B))
)
