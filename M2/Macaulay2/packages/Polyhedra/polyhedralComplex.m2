-- Defining the new type PolyhedralComplex
PolyhedralComplex = new Type of PolyhedralObjectFamily
globalAssignment PolyhedralComplex

-- Modifying the standard output for a Polyhedral Complex to give an overview of its characteristica
net PolyhedralComplex := F -> ( horizontalJoin flatten (
	  "{",
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"ambient dimension", 
			                      "dimension",
					      "number of generating polyhedra"}, key -> (net key, " => ", net F#key))),
	  "}" ))


-- PURPOSE : Giving the generating Polyhedra of the PolyhedralComplex
--   INPUT : 'PC'  a PolyhedralComplex
--  OUTPUT : a List of Cones
maxPolyhedra = method(TypicalValue => List)
maxPolyhedra PolyhedralComplex := PC -> maxObjects PC


-- PURPOSE : Giving the k dimensionial Polyhedra of the Polyhedral Complex
--   INPUT : (k,PC)  where 'k' is a positive integer and PC is a PolyhedralComplex 
--  OUTPUT : a List of Polyhedra
polyhedra = method(TypicalValue => List)
polyhedra(ZZ,PolyhedralComplex) := (k,PC) -> objectsOfDim(k, PC)

vertices PolyhedralComplex := PC -> matrix {toList PC#"vertices"}
