-- Defining the new type Cone
Cone = new Type of PolyhedralObject
Cone.synonym = "convex rational cone"
globalAssignment Cone

Cone == Cone := (C1,C2) -> C1 === C2

-- Modifying the standard output for a Cone to give an overview of its characteristica
net Cone := C -> ( horizontalJoin flatten (
	  "{",
	  -- prints the parts vertically
	  stack (horizontalJoin \ sort apply({"ambient dimension", 
			                      "dimension",
					      "dimension of lineality space",
					      "number of rays",
					      "number of facets"}, key -> (net key, " => ", net C#key))),
	  "}" ))


-- PURPOSE : Tests if a Cone is pointed
--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isPointed Cone := C -> rank C#"linealitySpace" == 0


--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isSmooth Cone := C -> (
     -- generating the non-linealityspace cone of C
     R := lift(transpose rays C,ZZ);
     n := dim C - C#"dimension of lineality space";
     -- if the cone is full dimensional then it is smooth iff its rays form a basis over ZZ
     numRows R == n and (M := (smithNormalForm R)#0; product apply(n, i -> M_(i,i)) == 1))
	   


-- PURPOSE : Building the Cone 'C'
--   INPUT : '(genrays,dualgens)',  a pair of two matrices each describing the cone C
--                                	directly  as generating rays ('genrays') and in the 
--						dual description as intersection of half-spaces through 
--						the origin ('dualgens')
--  OUTPUT : The Cone 'C'
coneBuilder = (genrays,dualgens) -> (
      -- Sorting into rays, lineality space generators, supporting half-spaces, and hyperplanes
      RM := genrays#0;
      LS := genrays#1;
      HS := transpose(-dualgens#0);
      hyperplanesTmp := transpose(dualgens#1);
      -- Defining C
      new Cone from {
	   "ambient dimension" => numgens target RM,
	   "dimension" => (numgens target RM)-(rank hyperplanesTmp),
	   "dimension of lineality space" => numgens source LS,
	   "linealitySpace" => LS,
	   "number of rays" => numgens source RM,
	   "rays" => RM,
	   "number of facets" => numgens target HS,
	   "halfspaces" => HS,
	   "hyperplanes" => hyperplanesTmp,
	   "genrays" => genrays,
	   "dualgens" => dualgens,
	   symbol cache => new CacheTable})


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




--   INPUT : 'C'  a Cone
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace Cone := C -> C#"linealitySpace"


     
fVector Cone := C -> apply(dim C + 1, d -> #faces(dim C - d,C))


-- PURPOSE : Computing the Hilbert basis of a Cone 
--   INPUT : 'C',  a Cone
--  OUTPUT : 'L',  a list containing the Hilbert basis as one column matrices 
hilbertBasis = method(TypicalValue => List)
hilbertBasis Cone := C -> (
     -- Computing the row echolon form of the matrix M
     ref := M -> (
	  n := numColumns M;
	  s := numRows M;
	  BC := map(ZZ^n,ZZ^n,1);
	  m := min(n,s);
	  -- Scan through the first square part of 'M'
	  i := 0;
	  stopper := 0;
	  while i < m and stopper < n do (
		    -- Selecting the first non-zero entry after the i-th row in the i-th column
		    j := select(1,toList(i..s-1),k -> M_i_k != 0);
		    -- if there is a non-zero entry, scan the remaining entries and compute the reduced form for this column
		    if j != {} then (
			 j = j#0;
			 scan((j+1)..(s-1), k -> (
				   if M_i_k != 0 then (
					a := M_i_j;
					b := M_i_k;
					L := gcdCoefficients(a,b);
					a = substitute(a/(L#0),ZZ);
					b = substitute(b/(L#0),ZZ);
					M = M^{0..j-1} || (L#1)*M^{j} + (L#2)*M^{k} || M^{j+1..k-1} || (-b)*M^{j} + a*M^{k} || M^{k+1..s-1})));
			 if i != j then (
			      M = M^{0..i-1} || M^{j} || M^{i+1..j-1} || M^{i} || M^{j+1..s-1});
			 if M_i_i < 0 then M = M^{0..i-1} || -M^{i} || M^{i+1..s-1})
		    else (
			 M = M_{0..i-1} | M_{i+1..n-1} | M_{i};
			 BC = BC_{0..i-1} | BC_{i+1..n-1} | BC_{i};
			 i = i-1);
		    i = i+1;
		    stopper = stopper + 1);
	  (M,BC));
     -- Function to compute the/one preimage of h under A
     preim := (h,A) -> (
	  -- Take the generators of the kernel of '-h|A' and find an element with 1 as first entry -> the other entrys are a preimage
	  -- vector
	  N := gens ker(-h|A);
	  N = transpose (ref transpose N)#0;
	  N_{0}^{1..(numRows N)-1});
     A := C#"halfspaces";
     if C#"hyperplanes" != 0 then A = A || C#"hyperplanes" || -(C#"hyperplanes");
     A = substitute(A,ZZ);
     -- Use the project and lift algorithm to compute a basis of the space of vectors positive on 'A' whose preimages are the HilbertBasis
     (B,BC) := ref transpose A; 
     H := constructHilbertBasis B;
     BC = inverse transpose BC;
     apply(H,h -> preim(BC*h,A)))


