-------------------------------------------------------------------------------
-- For fan and cone
isPointed = method(TypicalValue => Boolean)
-- PURPOSE : Checks if the input is smooth
isSmooth = method(TypicalValue => Boolean)



-------------------------------------------------------------------------------
-- For polyhedron and polyhedral complex

-- PURPOSE : Giving the vertices
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a Matrix, containing the vertices of P as column vectors
vertices = method()




-------------------------------------------------------------------------------
-- For polyhedron and cone and fan

-- PURPOSE : Giving the rays
--   INPUT : 'P'  a Polyhedron or Cone
--  OUTPUT : a Matrix, containing the rays of P as column vectors
rays = method()
