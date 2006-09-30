-- test monomial ordering

R = QQ[a..d]
assert ( sort basis(0,3,R) ==
  matrix {{1, d, c, b, a, d^2, c*d, b*d, a*d, c^2, b*c, a*c, b^2, a*b, a^2, d^3, c*d^2, b*d^2, a*d^2, c^2*d, b*c*d, a*c*d, b^2*d, a*b*d, a^2*d, c^3, b*c^2, a*c^2, b^2*c, a*b*c, a^2*c,
      b^3, a*b^2, a^2*b, a^3}} )
assert ( sort basis(0,3,R^2) ==
  -- this one will change
  matrix {{1, 0, d, 0, c, 0, b, 0, a, 0, d^2, 0, c*d, 0, b*d, 0, a*d, 0, c^2,
     0, b*c, 0, a*c, 0, b^2, 0, a*b, 0, a^2, 0, d^3, 0, c*d^2, 0, b*d^2, 0,
     a*d^2, 0, c^2*d, 0, b*c*d, 0, a*c*d, 0, b^2*d, 0, a*b*d, 0, a^2*d, 0, c^3,
     0, b*c^2, 0, a*c^2, 0, b^2*c, 0, a*b*c, 0, a^2*c, 0, b^3, 0, a*b^2, 0,
     a^2*b, 0, a^3, 0}, {0, 1, 0, d, 0, c, 0, b, 0, a, 0, d^2, 0, c*d, 0, b*d,
     0, a*d, 0, c^2, 0, b*c, 0, a*c, 0, b^2, 0, a*b, 0, a^2, 0, d^3, 0, c*d^2,
     0, b*d^2, 0, a*d^2, 0, c^2*d, 0, b*c*d, 0, a*c*d, 0, b^2*d, 0, a*b*d, 0,
     a^2*d, 0, c^3, 0, b*c^2, 0, a*c^2, 0, b^2*c, 0, a*b*c, 0, a^2*c, 0, b^3, 0,
     a*b^2, 0, a^2*b, 0, a^3}}  
  )

R = QQ[a..d,MonomialOrder => Position => Up]
assert ( sort basis(0,3,R) ==
  matrix {{1, d, c, b, a, d^2, c*d, b*d, a*d, c^2, b*c, a*c, b^2, a*b, a^2, d^3, c*d^2, b*d^2, a*d^2, c^2*d, b*c*d, a*c*d, b^2*d, a*b*d, a^2*d, c^3, b*c^2, a*c^2, b^2*c, a*b*c, a^2*c,
      b^3, a*b^2, a^2*b, a^3}} )
assert ( sort basis(0,3,R^2) ==
  matrix {{1, d, c, b, a, d^2, c*d, b*d, a*d, c^2, b*c, a*c, b^2, a*b, a^2, d^3, c*d^2, b*d^2, a*d^2, c^2*d, b*c*d, a*c*d, b^2*d, a*b*d, a^2*d,
     c^3, b*c^2, a*c^2, b^2*c, a*b*c, a^2*c, b^3, a*b^2, a^2*b, a^3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, d, c, b,
     a, d^2, c*d, b*d, a*d, c^2, b*c, a*c, b^2, a*b, a^2, d^3, c*d^2, b*d^2, a*d^2, c^2*d, b*c*d, a*c*d, b^2*d, a*b*d, a^2*d, c^3, b*c^2, a*c^2,
     b^2*c, a*b*c, a^2*c, b^3, a*b^2, a^2*b, a^3}} )

R = QQ[a..d,MonomialOrder => Position => Down]
assert ( sort basis(0,3,R) ==
  matrix {{1, d, c, b, a, d^2, c*d, b*d, a*d, c^2, b*c, a*c, b^2, a*b, a^2, d^3, c*d^2, b*d^2, a*d^2, c^2*d, b*c*d, a*c*d, b^2*d, a*b*d, a^2*d, c^3, b*c^2, a*c^2, b^2*c, a*b*c, a^2*c,
      b^3, a*b^2, a^2*b, a^3}})
assert ( sort basis(0,3,R^2) ==
  matrix {{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, d, c, b, a, d^2, c*d, b*d,
     a*d, c^2, b*c, a*c, b^2, a*b, a^2, d^3, c*d^2, b*d^2, a*d^2, c^2*d, b*c*d, a*c*d, b^2*d, a*b*d, a^2*d, c^3, b*c^2, a*c^2, b^2*c, a*b*c, a^2*c,
     b^3, a*b^2, a^2*b, a^3}, {1, d, c, b, a, d^2, c*d, b*d, a*d, c^2, b*c, a*c, b^2, a*b, a^2, d^3, c*d^2, b*d^2, a*d^2, c^2*d, b*c*d, a*c*d, b^2*d,
     a*b*d, a^2*d, c^3, b*c^2, a*c^2, b^2*c, a*b*c, a^2*c, b^3, a*b^2, a^2*b, a^3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}})

R = QQ[a..d,MonomialOrder => Lex]
assert ( sort basis(0,3,R) ==
  matrix {{1, d, d^2, d^3, c, c*d, c*d^2, c^2, c^2*d, c^3, b, b*d, b*d^2, b*c, b*c*d, b*c^2, b^2, b^2*d, b^2*c, b^3, a, a*d, a*d^2, a*c, a*c*d,
      a*c^2, a*b, a*b*d, a*b*c, a*b^2, a^2, a^2*d, a^2*c, a^2*b, a^3}})
assert ( sort basis(0,3,R^2) ==
  matrix {{1, 0, d, 0, d^2, 0, d^3, 0, c, 0, c*d, 0, c*d^2, 0, c^2, 0, c^2*d, 0, c^3, 0, b, 0, b*d, 0, b*d^2, 0, b*c, 0, b*c*d, 0, b*c^2, 0, b^2,
      0, b^2*d, 0, b^2*c, 0, b^3, 0, a, 0, a*d, 0, a*d^2, 0, a*c, 0, a*c*d, 0, a*c^2, 0, a*b, 0, a*b*d, 0, a*b*c, 0, a*b^2, 0, a^2, 0, a^2*d, 0,
      a^2*c, 0, a^2*b, 0, a^3, 0}, {0, 1, 0, d, 0, d^2, 0, d^3, 0, c, 0, c*d, 0, c*d^2, 0, c^2, 0, c^2*d, 0, c^3, 0, b, 0, b*d, 0, b*d^2, 0, b*c, 0,
      b*c*d, 0, b*c^2, 0, b^2, 0, b^2*d, 0, b^2*c, 0, b^3, 0, a, 0, a*d, 0, a*d^2, 0, a*c, 0, a*c*d, 0, a*c^2, 0, a*b, 0, a*b*d, 0, a*b*c, 0, a*b^2,
      0, a^2, 0, a^2*d, 0, a^2*c, 0, a^2*b, 0, a^3}}
      )
 
R = QQ[a..d,MonomialOrder => RevLex,Global => false]
assert ( sort basis(0,3,R) ==
  matrix {{a^3, a^2*b, a^2*c, a^2*d, a^2, a*b^2, a*b*c, a*b*d, a*b, a*c^2, a*c*d, a*c, a*d^2, a*d, a, b^3, b^2*c, b^2*d, b^2, b*c^2, b*c*d, b*c,
      b*d^2, b*d, b, c^3, c^2*d, c^2, c*d^2, c*d, c, d^3, d^2, d, 1}})
assert ( sort basis(0,3,R^2) ==
  matrix {{a^3, 0, a^2*b, 0, a^2*c, 0, a^2*d, 0, a^2, 0, a*b^2, 0, a*b*c, 0, a*b*d, 0, a*b, 0, a*c^2, 0, a*c*d, 0, a*c, 0, a*d^2, 0, a*d, 0, a,
      0, b^3, 0, b^2*c, 0, b^2*d, 0, b^2, 0, b*c^2, 0, b*c*d, 0, b*c, 0, b*d^2, 0, b*d, 0, b, 0, c^3, 0, c^2*d, 0, c^2, 0, c*d^2, 0, c*d, 0, c, 0,
      d^3, 0, d^2, 0, d, 0, 1, 0}, {0, a^3, 0, a^2*b, 0, a^2*c, 0, a^2*d, 0, a^2, 0, a*b^2, 0, a*b*c, 0, a*b*d, 0, a*b, 0, a*c^2, 0, a*c*d, 0, a*c,
      0, a*d^2, 0, a*d, 0, a, 0, b^3, 0, b^2*c, 0, b^2*d, 0, b^2, 0, b*c^2, 0, b*c*d, 0, b*c, 0, b*d^2, 0, b*d, 0, b, 0, c^3, 0, c^2*d, 0, c^2, 0,
      c*d^2, 0, c*d, 0, c, 0, d^3, 0, d^2, 0, d, 0, 1}}
      )

R = ZZ
assert( id_(R^2) == sort id_(R^2) )

R = QQ
assert( id_(R^2) == sort id_(R^2) )

R = GF 9
assert( id_(R^2) == sort id_(R^2) )

R = ZZ[x]
assert( id_(R^2) == sort id_(R^2) )

R = QQ[x]
assert( id_(R^2) == sort id_(R^2) )

R = ZZ[]
assert( id_(R^2) == sort id_(R^2) )

R = QQ[]
assert( id_(R^2) == sort id_(R^2) )

R = degreesRing 0
assert( id_(R^2) == sort id_(R^2) )
