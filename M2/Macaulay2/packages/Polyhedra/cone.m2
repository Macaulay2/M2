-- Defining the new type Cone
Cone = new Type of PolyhedralObject
Cone.synonym = "convex rational cone"
globalAssignment Cone

-- Modifying the standard output for a Cone to give an overview of its characteristica
net Cone := C -> ( horizontalJoin flatten (
	  "{",
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"ambient dimension", 
			                      "dimension of the cone",
					      "dimension of lineality space",
					      "number of rays",
					      "number of facets"}, key -> (net key, " => ", net C#key))),
	  "}" ))


-- PURPOSE : Computing the positive hull of a given set of rays lineality 
--		 space generators
posHull = method(TypicalValue => Cone)

--   INPUT : 'Mrays'  a Matrix containing the generating rays as column vectors
--		 'LS'  a Matrix containing the generating rays of the 
--				lineality space as column vectors
--  OUTPUT : 'C'  a Cone
-- COMMENT : The description by rays and lineality space is stored in C as well 
--		 as the description by defining half-spaces and hyperplanes.
posHull(Matrix,Matrix) := (Mrays,LS) -> (
     -- checking for input errors
     if numRows Mrays =!= numRows LS then error("rays and linSpace generators must lie in the same space");
     Mrays = chkZZQQ(Mrays,"rays");
     LS = chkZZQQ(LS,"lineality space");
     -- Computing generators of the cone and its dual cone
     dualgens := fourierMotzkin(Mrays,LS);
     local genrays;
     (genrays,dualgens) = fMReplacement(Mrays,dualgens#0,dualgens#1);
--     genrays := fourierMotzkin dualgens;
     coneBuilder(genrays,dualgens))


--   INPUT : 'R'  a Matrix containing the generating rays as column vectors
posHull Matrix := R -> (
     R = chkZZQQ(R,"rays");
     -- Generating the zero lineality space LS
     LS := map(target R,QQ^1,0);
     posHull(R,LS))


--   INPUT : '(C1,C2)'  two cones
posHull(Cone,Cone) := (C1,C2) -> (
	-- Checking for input errors
	if C1#"ambient dimension" =!= C2#"ambient dimension" then error("Cones must lie in the same ambient space");
	-- Combining the rays and the lineality spaces into one matrix each
	R := C1#"rays" | C2#"rays";
	LS := C1#"linealitySpace" | C2#"linealitySpace";
	dualgens := fourierMotzkin(R,LS);
	local genrays;
	(genrays,dualgens) = fMReplacement(R,dualgens#0,dualgens#1);
--	genrays := fourierMotzkin dualgens;
	coneBuilder(genrays,dualgens))


