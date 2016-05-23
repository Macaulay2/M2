-- Defining the new type PolyhedralComplex
PolyhedralComplex = new Type of PolyhedralObjectFamily
globalAssignment PolyhedralComplex


-- PURPOSE : Giving the generating Polyhedra of the PolyhedralComplex
--   INPUT : 'PC'  a PolyhedralComplex
--  OUTPUT : a List of Cones
maxPolyhedra = method(TypicalValue => List)
maxPolyhedra PolyhedralComplex := PC -> maxObjects PC

