-- PURPOSE : Computing the inner normalFan of a polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'F',  a Fan, the inner normalFan of 'P'
normalFan = method(TypicalValue => Fan)
normalFan Polyhedron := P -> (
   getProperty(P, computedNormalFan)
)      


vertices Polyhedron := P -> (
   getProperty(P, computedVertices)
)

