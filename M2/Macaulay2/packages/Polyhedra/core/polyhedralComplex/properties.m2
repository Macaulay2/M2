-- The underlying fan of a polyhedral complex is always set at construction
-- time (see internalPolyhedralComplexConstructor), so this simply provides a
-- single, cached access point for it -- in place of getProperty(PC, underlyingFan) --
-- that could later grow a real computation if that ever becomes necessary.
-- (Named getUnderlyingFan, not underlyingFan, since the latter is a protected
-- symbol used as the property/cache key.)
getUnderlyingFan = method()
getUnderlyingFan PolyhedralComplex := (cacheValue symbol underlyingFan) (
   PC -> error("No underlying fan set for this PolyhedralComplex.")
)


-- vertices/rays/linealitySpace mutually reference each other's computation
-- for PolyhedralComplex (via the underlying Fan), migrated together; see the
-- analogous blocks in core/cone/properties.m2 and core/polyhedron/properties.m2.
vertices PolyhedralComplex := (cacheValue symbol computedVertices) (
   PC -> (
      F := getUnderlyingFan PC;
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
      vMat
   )
)


compute#PolyhedralComplex#pure = method()
compute#PolyhedralComplex#pure PolyhedralComplex := PC -> (
   F := getUnderlyingFan PC;
   return isPure F
)


compute#PolyhedralComplex#isWellDefined = method()
compute#PolyhedralComplex#isWellDefined PolyhedralComplex := PC -> (
   F := getUnderlyingFan PC;
   return isWellDefined F
)


rays PolyhedralComplex := {} >> o -> (cacheValue rays) (
   PC -> (
      vertices PC;
      rays PC
   )
)


compute#PolyhedralComplex#computedDimension = method()
compute#PolyhedralComplex#computedDimension PolyhedralComplex := PC -> (
   F := getUnderlyingFan PC;
   dim F - 1
)


linealitySpace PolyhedralComplex := (cacheValue symbol computedLinealityBasis) (
   PC -> (
      F := getUnderlyingFan PC;
      result := promote(linealitySpace F, QQ);
      test := all(0..(numColumns result - 1), i-> result_i_0 == 0);
      if not test then error("Something went wrong while computing linealitySpace.");
      submatrix(result, 1..(numRows result -1), 0..(numColumns result - 1))
   )
)


compute#PolyhedralComplex#ambientDimension = method()
compute#PolyhedralComplex#ambientDimension PolyhedralComplex := PC -> (
   F := getUnderlyingFan PC;
   ambDim F - 1
)


compute#PolyhedralComplex#generatingObjects = method()
compute#PolyhedralComplex#generatingObjects PolyhedralComplex := PC -> (
   F := getUnderlyingFan PC;
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
   F := getUnderlyingFan PC;
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

compute#PolyhedralComplex#simplicial = method()
compute#PolyhedralComplex#simplicial PolyhedralComplex := PC -> (
   hmO := maxPolyhedra(PC, Polyhedron);
   all(hmO, m -> isSimplicial m)
)
