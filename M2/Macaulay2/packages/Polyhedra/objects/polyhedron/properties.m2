vertices Polyhedron := P -> (
   getProperty(P, computedVertices)
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

compute#Polyhedron#computedVertices = method()
compute#Polyhedron#computedVertices Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   n := ambDim P;
   homogVert := rays C;
   r := ring homogVert;
   vList := {};
   rList := {};
   latticeTest := true;
   for i from 0 to numColumns homogVert - 1 do (
      current := homogVert_i;
      if current_0 > 0 then (
         if current_0 != 1 then (
            latticeTest = false;
            current = (1/(current_0)) * promote(current, QQ);
         );
         vList = append(vList, slice(current, 1..n));
      ) else if current_0 == 0 then (
         rList = append(rList, slice(current, 1..n));
      ) else (
         error("Something went wrong, vertex with negative height.");
      );
   );
   setProperty(P, lattice, latticeTest);
   vMat := matrixFromVectorList(vList, n-1, r);
   rMat := matrixFromVectorList(rList, n-1, r);
   setProperty(P, computedRays, rMat);
   setProperty(P, empty, numColumns vMat == 0);
   return vMat
)

compute#Polyhedron#computedDimension = method()
compute#Polyhedron#computedDimension Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   dim C - 1
)

compute#Polyhedron#computedLinealityBasis = method()
compute#Polyhedron#computedLinealityBasis Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   result := linealitySpace C;
   test := all(0..(numColumns result - 1), i-> result#i_0 == 0);
   if not test then error("Something went wrong while computing linealitySpace.");
   submatrix(result, 1..(numRows result -1), 0..(numColumns result - 1))
)


compute#Polyhedron#underlyingCone = method()
compute#Polyhedron#underlyingCone Polyhedron := P -> (
   if hasProperties(P, {points, inputRays}) then (
      pMat := getProperty(P, points);
      rMat := getProperty(P, inputRays);
      r := ring pMat;
      pMat = map(r^1, source pMat, (i,j)-> 1) || pMat;
      rMat = map(r^1, source rMat, (i,j)-> 1) || rMat;
      return posHull(pMat, rMat)
   ) else (
      -- TODO:
      -- vertices + rays
      -- facets + linealitySpace
      -- ...
      error("Underlying cone not computable from given data.");
   )
)

