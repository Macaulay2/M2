
-- PURPOSE : Tests whether the intersection of two Polyhedra/Cones is a face of both
commonFace = method(TypicalValue => Boolean)

--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
commonFace(Polyhedron,Polyhedron) := (P,Q) -> (
   CP := getProperty(P, underlyingCone);
   CQ := getProperty(Q, underlyingCone);
   commonFace(CP, CQ)
)

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : 'true' or 'false'
commonFace(Cone,Cone) := (C1,C2) -> (
   if ambDim(C1) == ambDim(C2) then (
      I := intersection(C1,C2);
      isFace(I,C1) and isFace(I,C2)
   ) else false
)


--   INPUT : '(C,F)'  a Cone and a Fan
--  OUTPUT : 'true' or 'false'
-- COMMENT : For this it checks if the cone has a common face with every generating cone of the fan
commonFace(Cone,Fan) := (C,F) -> (
   if ambDim(C) == ambDim(F) then 
      all(values getProperty(F, honestMaxObjects), C1 -> commonFace(C,C1)) 
   else false
)


--   INPUT : '(F,C)'  a Fan and a Cone
--  OUTPUT : 'true' or 'false'
-- COMMENT : For this it checks if the cone has a common face with every generating cone of the fan
commonFace(Fan,Cone) := (F,C) -> commonFace(C,F)


--   INPUT : '(F1,F2)'  two Fans
--  OUTPUT : 'true' or 'false'
-- COMMENT : For this it checks if all generating cones of 'F1' have a common face with every generating cone of 'F2'
commonFace(Fan,Fan) := (F1,F2) -> all(values getProperty(F1, honestMaxObjects), 
   C -> commonFace(C,F2)
)


--   INPUT : 'L'  a List
--  OUTPUT : 'true' or 'false'
commonFace List := L -> all(#L-1, i -> all(i+1..#L-1, j -> commonFace(L#i,L#j)))

   
