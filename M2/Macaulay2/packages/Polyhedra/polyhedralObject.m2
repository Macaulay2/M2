-- Defining the new type PolyhedraHash
PolyhedraHash = new Type of HashTable
globalAssignment PolyhedraHash

-- PURPOSE : Giving the defining affine hyperplanes
ambDim = method(TypicalValue => ZZ)

--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, the dimension of the ambient space
ambDim PolyhedraHash := X -> X#"ambient dimension"



-- Defining the new type PolyhedralObject
PolyhedralObject = new Type of PolyhedraHash
globalAssignment PolyhedralObject

-- PURPOSE : Giving the defining affine hyperplanes
--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : '(N,w)', where M and v are matrices and P={x in HS | Nx=w}, where 
--		 HS is the intersection of the defining affine half-spaces
hyperplanes = method()

-- PURPOSE : Giving a basis of the lineality space
linSpace = method(TypicalValue => Matrix)




-- Defining the new type PolyhedralObjectFamily
PolyhedralObjectFamily = new Type of PolyhedraHash
globalAssignment PolyhedralObjectFamily
