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
