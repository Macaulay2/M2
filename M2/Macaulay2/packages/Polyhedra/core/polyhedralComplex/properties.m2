compute#PolyhedralComplex#computedVertices = method()
compute#PolyhedralComplex#computedVertices PolyhedralComplex := PC -> (
   F := getProperty(PC, underlyingFan);
   n := ambDim PC;
   homogVert := promote(rays F, QQ);
   vList := {};
   rList := {};
   for i from 0 to numColumns homogVert - 1 do (
      current := homogVert_i;
      if current_0 > 0 then (
         current = (1/(current_0)) * current;
         vList = append(vList, slice(current, 1..n));
      ) else if current_0 == 0 then (
         rList = append(rList, slice(current, 1..n));
      ) else (
         error("Something went wrong, vertex with negative height.");
      );
   );
   vMat := matrixFromVectorList(vList, n, QQ);
   rMat := matrixFromVectorList(rList, n, QQ);
   setProperty(PC, rays, rMat);
   setProperty(PC, empty, numColumns vMat == 0);
   return vMat
)


compute#PolyhedralComplex#pure = method()
compute#PolyhedralComplex#pure PolyhedralComplex := PC -> (
   F := getProperty(PC, underlyingFan);
   return isPure F
)


compute#PolyhedralComplex#isWellDefined = method()
compute#PolyhedralComplex#isWellDefined PolyhedralComplex := PC -> (
   F := getProperty(PC, underlyingFan);
   return isWellDefined F
)


compute#PolyhedralComplex#rays = method()
compute#PolyhedralComplex#rays PolyhedralComplex := PC -> (
   vertices PC;
   getProperty(PC, rays)
)


compute#PolyhedralComplex#computedDimension = method()
compute#PolyhedralComplex#computedDimension PolyhedralComplex := PC -> (
   F := getProperty(PC, underlyingFan);
   dim F - 1
)


compute#PolyhedralComplex#computedLinealityBasis = method()
compute#PolyhedralComplex#computedLinealityBasis PolyhedralComplex := PC -> (
   F := getProperty(PC, underlyingFan);
   result := promote(linealitySpace F, QQ);
   test := all(0..(numColumns result - 1), i-> result_i_0 == 0);
   if not test then error("Something went wrong while computing linealitySpace.");
   submatrix(result, 1..(numRows result -1), 0..(numColumns result - 1))
)


compute#PolyhedralComplex#ambientDimension = method()
compute#PolyhedralComplex#ambientDimension PolyhedralComplex := PC -> (
   F := getProperty(PC, underlyingFan);
   ambDim F - 1
)


compute#PolyhedralComplex#generatingObjects = method()
compute#PolyhedralComplex#generatingObjects PolyhedralComplex := PC -> (
   F := getProperty(PC, underlyingFan);
   vertPC := vertices PC;
   raysPC := rays PC;
   raysF := rays F;
   vertPCMap := rayCorrespondenceMap(raysF, prependOnes vertPC);
   raysPCMap := rayCorrespondenceMap(raysF, prependZeros raysPC);
   maxConesF := maxCones F;
   mO := apply(maxConesF,
      cone -> (
         vertFace := apply(cone, v -> vertPCMap#v);
         vertFace = select(vertFace, v -> v != -1);
         raysFace := apply(cone, v -> raysPCMap#v);
         raysFace = select(raysFace, v -> v != -1);
         (vertFace, raysFace)
      )
   );
   mO
)


compute#PolyhedralComplex#computedFacesThroughRays = method()
compute#PolyhedralComplex#computedFacesThroughRays PolyhedralComplex := PC -> (
   F := getProperty(PC, underlyingFan);
   vertPC := vertices PC;
   raysPC := rays PC;
   raysF := rays F;
   vertPCMap := rayCorrespondenceMap(raysF, prependOnes vertPC);
   raysPCMap := rayCorrespondenceMap(raysF, prependZeros raysPC);
   conesF := getProperty(F, computedFacesThroughRays);
   result := new MutableHashTable;
   for i from 0 to dim PC do result#i = {};
   for k in keys conesF do (
      result#(k-1) = apply(conesF#k,
         cone -> (
            vertFace := apply(cone, v -> vertPCMap#v);
            vertFace = select(vertFace, v -> v != -1);
            raysFace := apply(cone, v -> raysPCMap#v);
            raysFace = select(raysFace, v -> v != -1);
            (vertFace, raysFace)
         )
      )
   );
   hashTable pairs result
)

compute#PolyhedralComplex#honestMaxObjects = method()
compute#PolyhedralComplex#honestMaxObjects PolyhedralComplex := PC -> (
   vertPC := vertices PC;
   raysPC := rays PC;
   linPC := linealitySpace PC;
   mP := maxPolyhedra PC;
   apply(mP, m-> convexHull(vertPC_(m#0), raysPC_(m#1), linPC))
)


compute#PolyhedralComplex#simplicial = method()
compute#PolyhedralComplex#simplicial PolyhedralComplex := PC -> (
   hmO := getProperty(PC, honestMaxObjects);
   all(hmO, m -> isSimplicial m)
)
