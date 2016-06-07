-- PURPOSE : Computing the inner normalFan of a polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'F',  a Fan, the inner normalFan of 'P'
normalFan = method(TypicalValue => Fan)
normalFan Polyhedron := P -> (
   if not P.cache.?normalFan then (
   -- Saving the vertices
   vm := vertices P;
   -- For every vertex translate P by -this vertex and take the dual cone of the positive hull of it
   L := sort apply(numColumns vm, i -> (dualCone posHull affineImage(P,-vm_{i})));
   HS := transpose (halfspaces P)#0;
   HS = apply(numColumns HS, i -> -HS_{i});
   F := new Fan from {
       "generatingObjects" => set L,
       "ambient dimension" => ambDim P,
       "dimension" => dim L#0,
       "number of generating cones" => #L,
       "rays" => set HS,
       "number of rays" => #HS,
       "isPure" => true,
       symbol cache => new CacheTable};
   F.cache.isPolytopal = true;
   F.cache.polytope = P;
   P.cache.normalFan = F);
   P.cache.normalFan)      

