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
