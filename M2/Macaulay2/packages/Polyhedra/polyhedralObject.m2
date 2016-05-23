-- Defining the new type PolyhedraHash
PolyhedraHash = new Type of HashTable
globalAssignment PolyhedraHash

-- PURPOSE : Giving the defining affine hyperplanes
ambDim = method(TypicalValue => ZZ)

--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, the dimension of the ambient space
ambDim PolyhedraHash := X -> X#"ambient dimension"

isSimplicial = method(TypicalValue => Boolean)

isSimplicial PolyhedraHash := (cacheValue symbol isSimplicial)(X -> (
	if instance(X,Cone) then (isPointed X and numColumns rays X == dim X)
	else if instance(X,Fan) then all(maxCones X,isSimplicial)
	else if instance(X,Polyhedron) then (isCompact X and numColumns vertices X == dim X + 1)
	else all(maxPolyhedra X,isSimplicial)))





-- Defining the new type PolyhedralObject
PolyhedralObject = new Type of PolyhedraHash
globalAssignment PolyhedralObject

-- PURPOSE : Giving the defining affine hyperplanes
--   INPUT : 'P'  a Polyhedron or Cone 
--  OUTPUT : '(N,w)', where M and v are matrices and P={x in HS | Nx=w}, where 
--		 HS is the intersection of the defining affine half-spaces
--     for cones, 'w' is omitted.
hyperplanes = method()
hyperplanes PolyhedralObject := P -> P#"hyperplanes"

-- PURPOSE : Giving a basis of the lineality space
linSpace = method(TypicalValue => Matrix)
linSpace PolyhedralObject := P -> P#"linealitySpace"

-- PURPOSE : Giving the defining affine half-spaces
--   INPUT : 'P'  a Polyhedron or Cone
--  OUTPUT : '(M,v)', where M and v are matrices and P={x in H | Mx<=v}, where 
--		 H is the intersection of the defining affine hyperplanes
--     for cones, 'v' is omitted.
halfspaces = method()
halfspaces PolyhedralObject := P -> P#"halfspaces"

-- PURPOSE : Giving the rays
--   INPUT : 'P'  a Polyhedron or Cone
--  OUTPUT : a Matrix, containing the rays of P as column vectors
rays = method()
rays PolyhedralObject := P -> P#"rays"





-- Defining the new type PolyhedralObjectFamily
PolyhedralObjectFamily = new Type of PolyhedraHash
globalAssignment PolyhedralObjectFamily

-- PURPOSE : Checks if the Fan or PolyhedralComplex is of pure dimension
--   INPUT : 'PC'  a Fan or PolyhedralComplex
--  OUTPUT : 'true' or 'false'
isPure = method(TypicalValue => Boolean)
isPure PolyhedralObjectFamily := PC -> PC#"isPure"

-- PURPOSE : Tests if a Fan or PolyhedralComplex is complete
--   INPUT : 'POF'  a Fan or PolyhedralComplex
--  OUTPUT : 'true' or 'false'
isComplete = method(TypicalValue => Boolean)
isComplete PolyhedralObjectFamily := POF -> (
     if not POF.cache.?isComplete then (
	  n := dim POF;
	  POF.cache.isComplete = if n == ambDim POF then (
	       symmDiff := (x,y) -> ((x,y) = (set x,set y); toList ((x-y)+(y-x)));
	       Lfaces := {};
	       scan(toList POF#"generatingObjects", C -> if dim C == n then Lfaces = symmDiff(Lfaces,faces(1,C)));
	       Lfaces == {})
	  else false);
     POF.cache.isComplete)

