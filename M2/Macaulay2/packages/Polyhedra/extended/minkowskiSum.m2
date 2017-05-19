-- PURPOSE : Computing the Minkowskisum of two polyhedra in the same ambient space
minkowskiSum = method(TypicalValue => Polyhedron)

--   INPUT : '(P1,P2)',  two polyhedra
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Polyhedron) := (P1,P2) -> (
   -- Checking for input errors
   if ambDim(P1) =!= ambDim(P2) then error("Polyhedra must lie in the same space");
   if isEmpty P1 or isEmpty P2 then (
      return emptyPolyhedron ambDim P1 
   ) else if P1 == P2 then (
      return 2 * P1
   )
   else return pointwiseMinkowskiSum(P1,P2)
)

pointwiseMinkowskiSum = (P1,P2) -> (
     vertexData1 := getSufficientVertexData(P1);
     vertexData2 := getSufficientVertexData(P2);
     -- Vertices or points:
     V1 := vertexData1#0;
     V2 := vertexData2#0;
     -- Rays:
     R1 := vertexData1#1;
     R2 := vertexData2#1;
     -- Lineality:
     L1 := vertexData1#2;
     L2 := vertexData2#2;
     resultVertices := matrix {unique flatten apply(numColumns V1, i -> apply(numColumns V2, j -> V1_{i}+V2_{j}))};
     convexHull(resultVertices, R1|R2, L1|L2)
)



--   INPUT : '(C1,C2)',  two cones
--  OUTPUT : The Minkowskisum as a cone
minkowskiSum(Cone,Cone) := (C1,C2) -> (
     -- Checking for input errors
     if ambDim(C1) =!= ambDim(C2) then error("Cones must lie in the same space");
     -- Saving the vertices and rays
     R := rays(C1) | rays(C2);
     LS := linSpace(C1) | linSpace(C2);
     coneFromVData(R,LS))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Cone,Polyhedron) := (C,P) -> (
   -- Checking for input errors
   if ambDim(C) =!= ambDim(P) then error("Cone and polyhedron must lie in the same space");
   minkowskiSum(polyhedron C, P)
)


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Cone) := (P,C) -> (
   minkowskiSum(C, P)
)


Polyhedron + Polyhedron := minkowskiSum
Polyhedron + Cone := minkowskiSum
Cone + Polyhedron := minkowskiSum
Cone + Cone := minkowskiSum
