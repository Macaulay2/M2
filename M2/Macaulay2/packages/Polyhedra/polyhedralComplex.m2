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


boundaryMap (ZZ,PolyhedralComplex) := (i,PC) -> (
     L1 := polyhedra(i,PC);
     L2 := polyhedra(i-1,PC);
     L1 = apply(L1, e -> (Vm := vertices e; apply(numColumns Vm, i -> Vm_{i})));
     L2 = apply(L2, e -> (Vm := vertices e; apply(numColumns Vm, i -> Vm_{i})));
     transpose matrix apply(L1, l1 -> (
	       apply(L2, l2 -> (
			 if isSubset(set l2,set l1) then (
			      l3 := toList(set l1 - set l2);
			      l3 = apply(l3, e -> position(l1, e1 -> e1 == e));
			      l := #l3; 
			      k := #l2; 
			      (-1)^(k*l + sum l3 - substitute((l^2-l)/2,ZZ))) else 0)))))
