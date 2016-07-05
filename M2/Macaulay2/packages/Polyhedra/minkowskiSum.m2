-- PURPOSE : Computing the Minkowskisum of two polyhedra in the same ambient space
minkowskiSum = method(TypicalValue => Polyhedron)

--   INPUT : '(P1,P2)',  two polyhedra
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Polyhedron) := (P1,P2) -> (
     -- Checking for input errors
     if ambDim(P1) =!= ambDim(P2) then error("Polyhedra must lie in the same space");
     if isEmpty P1 or isEmpty P2 then emptyPolyhedron ambDim P1 else if P1 == P2 then 2 * P1 else pointwiseMinkowskiSum(P1,P2))


pointwiseMinkowskiSum = (P1,P2) -> (
     -- Saving the vertices and rays
     V1 := vertices P1;
     V2 := vertices P2;
     R := promote(rays P1 | rays P2,QQ) | map(target V1,QQ^1,0);
     Vnew := map(target V1,QQ^0,0);
     -- Collecting all sums of vertices of P1 with vertices of P2
     Vnew = matrix {unique flatten apply(numColumns V1, i -> apply(numColumns V2, j -> V1_{i}+V2_{j}))};
     convexHull(Vnew,R))



--   INPUT : '(C1,C2)',  two cones
--  OUTPUT : The Minkowskisum as a cone
minkowskiSum(Cone,Cone) := (C1,C2) -> (
     -- Checking for input errors
     if ambDim(C1) =!= ambDim(C2) then error("Cones must lie in the same space");
     -- Saving the vertices and rays
     R := rays(C1) | rays(C2);
     LS := linSpace(C1) | linSpace(C2);
     posHull(R,LS))


--   INPUT : '(C,P)',  a cone and a polyhedron
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Cone,Polyhedron) := (C,P) -> (
     -- Checking for input errors
     if ambDim(C) =!= ambDim(P) then error("Cone and polyhedron must lie in the same space");
     -- Saving the vertices and rays
     V := vertices(P);
     R := rays(P) | rays(C) | linSpace(C) | -(linSpace(C));
     convexHull(V,R))


--   INPUT : '(P,C)',  a polyhedron and a cone
--  OUTPUT : The Minkowskisum as a polyhedron
minkowskiSum(Polyhedron,Cone) := (P,C) -> (
     -- Checking for input errors
     if ambDim(C) =!= ambDim(P) then error("Cone and polyhedron must lie in the same space");
     -- Saving the vertices and rays
     V := vertices(P);
     R := rays(P) | rays(C) | linSpace(C) | -(linSpace(C));
     convexHull(V,R))


Polyhedron + Polyhedron := minkowskiSum
Polyhedron + Cone := minkowskiSum
Cone + Polyhedron := minkowskiSum
Cone + Cone := minkowskiSum
