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










