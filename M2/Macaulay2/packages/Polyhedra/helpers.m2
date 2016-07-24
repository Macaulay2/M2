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
   r := ring sources;
   rayCorrespondenceMap(sources, map(r^(numRows sources), r^0, 0), targets)
)

export{
   "rayCorrespondenceMap"
}

pointInSameDirection = method()
pointInSameDirection(Matrix, Matrix, Matrix) := (v1, v2, lineality) -> (
   if numColumns v1 != 1 then error("v1 not a vector");
   if numColumns v2 != 1 then error("v2 not a vector");
   r := rank lineality;
   if numColumns lineality != r then error("Not a basis of lineality");
   if rank(v1 | lineality) == r then error("v1 is lineality");
   if rank(v2 | lineality) == r then error("v2 is lineality");
   testmat := v1 | -v2 | lineality;
   K := generators kernel testmat;
   K_(0,0) * K_(1,0) > 0
)

rayCorrespondenceMap(Matrix, Matrix, Matrix) := (sources, lineality, targets) -> (
   L := for i from 0 to (numColumns sources -1) list (
      source := sources_{i};
      lr := rank lineality;
      corresponding := positions(0..(numColumns targets -1),
         j -> (
            target := targets_{j};
            -- << target << endl;
            -- << rank(source | target | lineality) == lr + 1 << endl;
            sameAffineSpace := (rank(source | target | lineality) == lr + 1);
            if not sameAffineSpace then return false;
            sameDirection := pointInSameDirection(source, target, lineality);
            sameAffineSpace and sameDirection
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

makeRaysUniqueAndPrimitive = method()
makeRaysUniqueAndPrimitive(Matrix) := M -> (
   L := apply(numColumns M, i -> M_i);
   newCols := {};
   L = scan(L,
      l -> (
         g := gcd entries l;
         if g != 0 then (
            newEntries := apply(entries l, e -> e/g);
            newCols = append(newCols, vector newEntries)
         ) 
      )
   );
   matrix unique newCols
)

