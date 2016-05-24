

-- PURPOSE : Check if 'P' contains 'Q'
--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
contains = method(TypicalValue => Boolean)
contains(Polyhedron,Polyhedron) := (P,Q) -> (
      -- checking for input errors
      if ambDim(P) =!= ambDim(Q) then error("Polyhedra must lie in the same ambient space");
      -- Saving the equations of P and vertices/rays of Q
      (A,B) := P#"homogenizedHalfspaces";
      (C,D) := Q#"homogenizedVertices";
      A = transpose A;
      B = transpose B;
      E := A*C;
      -- Checking if vertices/rays of Q satisfy the equations of P
      all(flatten entries E, e -> e <= 0) and A*D == 0*A*D and B*C == 0*B*C and B*D == 0*B*D)


-- PURPOSE : Check if 'C1' contains 'C2'
--   INPUT : '(C1,C2)'  two Cones
contains(Cone,Cone) := (C1,C2) -> (
      -- checking for input errors
      if ambDim(C1) =!= ambDim(C2) then error("Cones must lie in the same ambient space");
      -- Saving the equations of C1 and rays of C2
      (A,B) := C1#"dualgens";
      (C,D) := C2#"genrays";
      A = transpose A;
      B = transpose B;
      E := A*C;
      -- Checking if the rays of C2 satisfy the equations of C1
      all(flatten entries E, e -> e <= 0) and A*D == 0*A*D and B*C == 0*B*C and B*D == 0*B*D)
 

 
-- PURPOSE : Check if 'C' contains 'P'
--   INPUT : '(C,P)'  a Cone and a Polyhedron
contains(Cone,Polyhedron) := (C,P) -> (
      -- checking for input errors
      if ambDim(C) =!= ambDim(P) then error("Cone and Polyhedron must lie in the same ambient space");
      -- Saving the equations of C and vertices/rays of P
      M := makePrimitiveMatrix P#"vertices" | rays(P);
      LS := P#"linealitySpace";
      C1 := posHull(M,LS);
      contains(C,C1))



-- PURPOSE : Check if 'P' contains 'C'
--   INPUT : '(P,C)'  a Polyhedron and a Cone
contains(Polyhedron,Cone) := (P,C) -> (
      -- checking for input errors
      if ambDim(C) =!= ambDim(P) then error("Polyhedron and Cone must lie in the same ambient space");
      -- Saving the cone 'C' as a polyhedron and using the function on two polyhedra
      Q := coneToPolyhedron C;
      contains(P,Q))



-- PURPOSE : Check if 'P' contains 'p'
--   INPUT : '(P,p)'  a Polyhedron 'P' and a point 'p' given as a matrix
contains(Polyhedron,Matrix) := (P,p) -> (
      -- checking for input errors
      if ambDim(P) =!= numRows p then error("Polyhedron and point must lie in the same ambient space");
      if numColumns p =!= 1 then error("The point must be given as a one row matrix");
      contains(P,convexHull p))



-- PURPOSE : Check if 'C' contains 'p'
--   INPUT : '(C,p)'  a Cone 'C' and a point 'p' given as a matrix
contains(Cone,Matrix) := (C,p) -> (
      -- checking for input errors
      if ambDim(C) =!= numRows p then error("Polyhedron and point must lie in the same ambient space");
      if numColumns p =!= 1 then error("The point must be given as a one row matrix");
      contains(C,convexHull p))



-- PURPOSE : Check if a list of cones 'L' contains 'C'
--   INPUT : '(L,C)'  a List of cones 'L' and a Cone 'C'
contains(List,Cone) := (L,C) -> any(L, C1 -> C1 == C)
 
 
-- PURPOSE : Check if a list of cones 'L' contains 'C'
--   INPUT : '(L,C)'  a List of cones 'L' and a Cone 'C'
contains(List,Polyhedron) := (L,P) -> any(L, Q -> Q == P)
 
 
-- PURPOSE : Check if 'F' contains 'C'
--   INPUT : '(F,C)'  a Fan 'F' and a Cone 'C'
contains(Fan,Cone) := (F,C) -> (
      -- Checking for input errors
      if ambDim F != ambDim C then error("Fan and Cone must lie in the same ambient space");
      -- Making the list of cones of same dimension as 'C'
      L := cones(dim C,F);
      contains(L,C))
