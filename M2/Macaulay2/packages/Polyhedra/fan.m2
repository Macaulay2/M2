-- Defining the new type Fan
Fan = new Type of PolyhedralObjectFamily
globalAssignment Fan


-- PURPOSE : Giving the generating Cones of the Fan
--   INPUT : 'F'  a Fan
--  OUTPUT : a List of Cones
maxCones = method(TypicalValue => List)
maxCones Fan := F -> toList F#"generatingObjects"

--   INPUT : 'F',  a Fan
--  OUTPUT : 'true' or 'false'
isPointed Fan := F -> (
     if not F.cache.?isPointed then F.cache.isPointed = isPointed((maxCones F)#0);
     F.cache.isPointed)
