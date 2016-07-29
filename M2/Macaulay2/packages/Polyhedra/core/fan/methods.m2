--   INPUT : 'F'  a Fan
--  OUTPUT : a Matrix, where the column vectors are a basis of the lineality space
linSpace Fan := F -> linealitySpace F

linealitySpace Fan := F -> (
   getProperty(F, computedLinealityBasis)
)


--   INPUT : 'F'  a Fan
rays Fan := F -> getProperty(F, computedRays)

--   INPUT : 'F',  a Fan
--  OUTPUT : 'true' or 'false'
isPointed Fan := F -> (
     if not F.cache.?isPointed then F.cache.isPointed = isPointed((maxCones F)#0);
     F.cache.isPointed)


--   INPUT : 'F'  a Fan
--  OUTPUT : 'true' or 'false'
isSmooth Fan := F -> (
   getProperty(F, smooth)
)


-- PURPOSE : Computing the subfan of all smooth cones of the Fan
--   INPUT : 'F',  a Fan
--  OUTPUT : The Fan of smooth cones
smoothSubfan = method(TypicalValue => Fan)
smoothSubfan Fan := F -> (
   cones := getProperty(F, smoothCones);
   result := new HashTable from {
      inputCones => cones,
      computedRays => rays F,
      computedLinealityBasis => linealitySpace F
   };
   fan result
)

isPolytopal = method(TypicalValue => Boolean)
isPolytopal Fan := F -> getProperty(F, polytopal)

-- PURPOSE : Giving the k dimensionial Cones of the Fan
--   INPUT : (k,F)  where 'k' is a positive integer and F is a Fan 
--  OUTPUT : a List of Cones
cones = method(TypicalValue => List)
cones(ZZ,Fan) := (k,F) -> (
   d := dim F;
   faces := getProperty(F, computedFacesThroughRays);
   faces#(d-k)
)

-- PURPOSE : Computing the 'n'-skeleton of a fan
--   INPUT : (n,F),  where 'n' is a positive integer and
--                   'F' is a Fan
--  OUTPUT : the Fan consisting of the 'n' dimensional cones in 'F'
skeleton = method(TypicalValue => Fan)
skeleton(ZZ,Fan) := (n,F) -> (
   -- Checking for input errors
   if n < 0 or dim F < n then error("The integer must be between 0 and dim F");
   result := new HashTable from {
      inputRays => rays F,
      inputCones => cones(n,F),
      computedLinealityBasis => linealitySpace F
   };
   fan result
)
