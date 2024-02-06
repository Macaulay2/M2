compute#Polyhedron#computedVertices = method()
compute#Polyhedron#computedVertices Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   n := ambDim P;
   homogVert := promote(rays C, QQ);
   vList := {};
   rList := {};
   latticeTest := true;
   for i from 0 to numColumns homogVert - 1 do (
      current := homogVert_i;
      if current_0 > 0 then (
         if current_0 > 1 then latticeTest = false;
         current = (1/(current_0)) * current;
         vList = append(vList, slice(current, 1..n));
      ) else if current_0 == 0 then (
         rList = append(rList, slice(current, 1..n));
      ) else (
         error("Something went wrong, vertex with negative height.");
      );
   );
   setProperty(P, lattice, latticeTest);
   vMat := matrixFromVectorList(vList, n, QQ);
   rMat := matrixFromVectorList(rList, n, QQ);
   setProperty(P, rays, rMat);
   setProperty(P, empty, numColumns vMat == 0);
   return vMat
)


compute#Polyhedron#lattice = method()
compute#Polyhedron#lattice Polyhedron := P -> (
   try (lift(vertices P, ZZ); true) else false
)


compute#Polyhedron#empty = method()
compute#Polyhedron#empty Polyhedron := P -> (
   if hasProperty(P, points) then (numColumns getProperty(P, points)) == 0
   else (numColumns vertices P) == 0
)


compute#Polyhedron#rays = method()
compute#Polyhedron#rays Polyhedron := P -> (
   vertices P;
   getProperty(P, rays)
)


compute#Polyhedron#computedDimension = method()
compute#Polyhedron#computedDimension Polyhedron := P -> (
   if isEmpty P then
      return -1
   else
      C := getProperty(P, underlyingCone);
      dim C - 1
)


compute#Polyhedron#computedLinealityBasis = method()
compute#Polyhedron#computedLinealityBasis Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   result := promote(linealitySpace C, QQ);
   test := all(0..(numColumns result - 1), i-> result_i_0 == 0);
   if not test then error("Something went wrong while computing linealitySpace.");
   submatrix(result, 1..(numRows result -1), 0..(numColumns result - 1))
)


compute#Polyhedron#underlyingCone = method()
compute#Polyhedron#underlyingCone Polyhedron := P -> (
   result := {};
   local r;
   local pMat;
   local rMat;
   local ezero;
   local L;
   -- Copy every information the polyhedron provides to the
   -- underlyingCone.
   if hasProperties(P, {points, inputRays}) then (
      pMat = prependOnes getProperty(P, points);
      rMat = prependZeros getProperty(P, inputRays);
      result = append(result, inputRays => (pMat | rMat));
   );
   if hasProperties(P, {computedVertices, rays}) then (
      pMat = prependOnes getProperty(P, computedVertices);
      rMat = prependZeros getProperty(P, rays);
      result = append(result, rays => (pMat | rMat));
   );
   if hasProperty(P, inputLinealityGenerators) then (
      pMat = prependZeros getProperty(P, inputLinealityGenerators);
      result = append(result, inputLinealityGenerators => pMat);
   );
   if hasProperty(P, computedLinealityBasis) then (
      pMat = prependZeros getProperty(P, computedLinealityBasis);
      result = append(result, computedLinealityBasis => pMat);
   );
   if hasProperty(P, facets) then (
      L = getProperty(P, facets);
      pMat = -L#1 | L#0;
      ezero = matrix {flatten {1 , toList ((numgens source L#0):0)}};
      result = append(result, inequalities => ezero || (-pMat));
   ) else if hasProperty(P, inequalities) then (
      L = getProperty(P, inequalities);
      pMat = -L#1 | L#0;
      ezero = matrix {flatten {1 , toList ((numgens source L#0):0)}};
      -- At this point we do not know whether the height inequality
      -- is implied.
      result = append(result, inequalities => ezero || (-pMat));
   );
   if hasProperty(P, computedHyperplanes) then (
      L = getProperty(P, computedHyperplanes);
      pMat = (-L#1) | L#0;
      result = append(result, computedHyperplanes => pMat);
   );
   if hasProperty(P, equations) then (
      L = getProperty(P, equations);
      pMat = (-L#1) | L#0;
      result = append(result, equations => pMat);
   );
   resultHash := new HashTable from result;
   internalConeConstructor resultHash
)


compute#Polyhedron#facets = method()
compute#Polyhedron#facets Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   hpC := promote(hyperplanes C, QQ);
   result := promote(facets C, QQ);
   -- Elimination of the trivial half-space
   ezero := matrix {flatten {1 , toList (((numgens source result)-1):0)}};
   ezero = promote(ezero, QQ);
   trivialIndex := positions(0..(numRows result)-1, i -> 
      if numRows hpC != 0 then pointInSameDirection(transpose ezero, transpose result^{i}, transpose hpC)
      else ezero === result^{i}
   );
   if #trivialIndex > 0 then (
      trivialIndex = trivialIndex#0;
   ) else (
      trivialIndex = -1;
   );
   result = result^(toList select(0..(numRows result)-1, i -> i != trivialIndex));
   (- submatrix(result, 0..(numRows result - 1), 1..(numColumns result -1)), result_{0})
)


compute#Polyhedron#computedHyperplanes = method()
compute#Polyhedron#computedHyperplanes Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   result := promote(hyperplanes C, QQ);
   (submatrix(result, 0..(numRows result - 1), 1..(numColumns result -1)), -result_{0})
)


compute#Polyhedron#verticesThroughFacets = method()
compute#Polyhedron#verticesThroughFacets Polyhedron := P -> (
   facetsP := facets P;
   C := getProperty(P, underlyingCone);
   facetVectors := facetsP#0;
   facetValues := facets;
   verticesP := vertices P;
   L := for i from 0 to (numColumns verticesP -1) list (
      vertex := verticesP_i;
      select(0..(numRows facetsP - 1), 
         j-> (
            facet := facetsP^{j};
         )
      )
   );
)


compute#Polyhedron#isWellDefined = method()
compute#Polyhedron#isWellDefined Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   return isWellDefined C
)


compute#Polyhedron#facetToFacetMap = method()
compute#Polyhedron#facetToFacetMap Polyhedron := P -> (
   facetsP := facets P;
   facetsP = (-facetsP#1) | facetsP#0;
   C := getProperty(P, underlyingCone);
   facetsC := promote(facets C, QQ);
   rayCorrespondenceMap( - transpose facetsC, transpose facetsP)
)


compute#Polyhedron#computedNormalFan = method()
compute#Polyhedron#computedNormalFan Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   raysC := rays C;
   raysNF := - transpose (facets P)#0;
   facetMap := getProperty(P, facetToFacetMap);
   maximalConesNF := getProperty(C, raysThroughFacets);
   goodConeIndices := select(numColumns raysC, i -> (raysC_i)_0 > 0);
   maximalConesNF = maximalConesNF_goodConeIndices;
   maximalConesNF = apply(maximalConesNF,
      mc -> (
         apply(mc, i -> facetMap#i)
      )
   );
   linealitySpaceNF := transpose((hyperplanes P)#0);
   result := fan(raysNF, linealitySpaceNF, maximalConesNF);
   setProperty(result, computedPolytope, P);
   setProperty(result, polytopal, true);
   result
)


compute#Polyhedron#ambientDimension = method()
compute#Polyhedron#ambientDimension Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   ambDim C - 1
)


compute#Polyhedron#computedLatticePoints = method()
compute#Polyhedron#computedLatticePoints Polyhedron := P -> (
   if isEmpty P then error("Polyhedron is empty!");
   C := getProperty(P, underlyingCone);
   H := hilbertBasis C;
   result := select(H, h -> h_(0,0) == 1);
   result = apply(result, r -> submatrix(r, 1..(numRows r -1) ,(1:0)));
   apply(result, r -> lift(r, ZZ))
)

compute#Polyhedron#computedFacesThroughRays = method()
compute#Polyhedron#computedFacesThroughRays Polyhedron := P -> (
   C := getProperty(P, underlyingCone);
   vertP := vertices P;
   raysP := rays P;
   raysC := rays C;
   vertPCMap := rayCorrespondenceMap(raysC, prependOnes vertP);
   raysPCMap := rayCorrespondenceMap(raysC, prependZeros raysP);
   -- << "VP: " << vertP << endl;
   -- << "RP: " << raysP << endl;
   -- << "RC: " << raysC << endl;
   -- << vertPCMap << endl;
   -- << raysPCMap << endl;
   facesC := faces C;
   result := for codim in keys facesC list (
      facesPcodim := apply(facesC#codim,
         face -> (
            vertFace := apply(face, v -> vertPCMap#v);
            vertFace = select(vertFace, v -> v != -1);
            raysFace := apply(face, v -> raysPCMap#v);
            raysFace = select(raysFace, v -> v != -1);
            (vertFace, raysFace)
         )
      );
      -- << facesPcodim << endl;
      facesPcodim = select(facesPcodim, face -> #(face#0) > 0);
      codim  => facesPcodim
   );
   new HashTable from result
)


compute#Polyhedron#computedFVector = method()
compute#Polyhedron#computedFVector Polyhedron := P -> (
   apply(dim P + 1, d -> #faces(dim P - d,P))
)


compute#Polyhedron#computedCompact = method()
compute#Polyhedron#computedCompact Polyhedron := P -> (
   linealitySpace(P) == 0 and rays(P) == 0
)


compute#Polyhedron#simplicial = method()
compute#Polyhedron#simplicial Polyhedron := P -> (
   facetsP := faces(1, P);
   vertP := vertices P;
   raysP := rays P;
   linP := linealitySpace P;
   facetsP = apply(facetsP, f->(
         A := vertP_(f#0);
         A = matrix {toList ((numColumns A):1/1)} || A;
         B := raysP_(f#1) | linP;
         B = matrix {toList ((numColumns B):0/1)} || B;
         A | B
      )
   );
   all(facetsP, m -> numColumns m == rank m)
)


compute#Polyhedron#nFacets = method()
compute#Polyhedron#nFacets Polyhedron := P -> (
   numRows ((facets P)#0)
)


compute#Polyhedron#nVertices = method()
compute#Polyhedron#nVertices Polyhedron := P ->(
   numColumns vertices P
)



