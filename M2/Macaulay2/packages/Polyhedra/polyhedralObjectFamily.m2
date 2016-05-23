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

