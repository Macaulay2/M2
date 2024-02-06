-- these tests provided by Greg Smith

R = QQ[vars(0..3)]
I = monomialIdeal( a^3, b^3, a^2*b^2)
assert( ideal I == ideal ( a^3, b^3, a^2*b^2))
assert( module I == module ideal ( a^3, b^3, a^2*b^2))
assert( I_0 == a^3 )
assert( I_2 == b^3 )

J = ideal(b*c, c^2 - b*d, -a*c+b^2)
assert( monomialIdeal J == monomialIdeal gens gb J )

--  ISMONOMIAL  --------------------------------------------

R = QQ[vars(0..3)]
I = ideal( a^3, b^3, a^2*b^2)
assert isMonomialIdeal I

J = ideal(b*c, c^2 - b*d, -a*c+b^2)
assert not isMonomialIdeal J

--  BOREL  -------------------------------------------------

R = QQ[a,b,c,d]
I = monomialIdeal( a^2, a*b, a*c, b^2, b*c, c^2)
assert isBorel I

J = monomialIdeal(b^5)
assert( borel J == monomialIdeal matrix {{a^5, a^4*b, a^3*b^2, a^2*b^3, a*b^4, b^5}} )



--  ALEXANDER DUALS  ---------------------------------------

-- stick twisted cubic 
R = QQ[a,b,c,d];
I = monomialIdeal(a*b, b*c, c*d)
assert isSquareFree I
assert( dual I == monomialIdeal (a*c, b*c, b*d))
assert( dual dual I == I )

-- permutohedron ideal with n=3
R = QQ[x,y,z];
I = monomialIdeal(x*y^2*z^3, x*y^3*z^2, x^2*y*z^3, 
     x^2*y^3*z, x^3*y*z^2, x^3*y^2*z)
J = monomialIdeal(x^3, y^3, z^3, x*y*z, x^2*y^2, x^2*z^2, 
     y^2*z^2)
assert( dual(I) == J )

-- (dual dual =!= identity) in general
R = QQ[x,y,z]
I = monomialIdeal( x^3*z, x*y*z, y^3*z, x^3*y^3)
J = dual(I, {3,4,1})
assert ( J == monomialIdeal matrix {{x^3*y^2, x*y^4, x*z, y^2*z}} )
K = dual J
assert( K == monomialIdeal matrix {{x^3*y^3, x^3*z, x*y*z, y^3*z}} )
dual dual J == J -- false

-- example 4.3 in Miller's thesis
R = QQ[a,b,c,d]
Iprime = monomialIdeal(a*b, b*c, c*d)
I = Iprime * (monomialIdeal vars R)
assert(
     dual(I, {2,2,2,2}) == 
     monomialIdeal matrix {{a^2*c^2, b^2*c^2, a^2*b^2*c*d, b^2*d^2, a^2*b*c*d^2, a*b*c^2*d^2}} )

assert(
     dual(I, a^2*b^2*c^2*d^2) == 
     monomialIdeal matrix {{a^2*c^2, b^2*c^2, a^2*b^2*c*d, b^2*d^2, a^2*b*c*d^2, a*b*c^2*d^2}} )

--  PRIMARY DECOMPOSITION  ---------------------------------

R = QQ[x,y,z];
I = monomialIdeal(z^5, x^2*z^2, x^4*y^3, x^3*y^5, y^4*z^3,
      y^2*z^4, x*y*z)
P = primaryDecomposition(I)
assert( intersect(P) == I )
A = associatedPrimes(I)
assert( radical(I) == intersect(A) )

J = monomialIdeal(x^3*y^5*z, y^5*z^4, y^3*z^5, x*y*z^5, 
     x^2*z^5, x^4*z^3, x^4*y^2*z^2, x^4*y^4*z)
Q = primaryDecomposition(J)
assert( intersect(Q) == J )
B = associatedPrimes(J)
assert( intersect(B) == radical(J) )



--  STANDARD PAIRS  ----------------------------------------

setf = x -> set apply(x, y -> {y#0, set y#1})

-- example 3.2.6 in Saito-Sturmfels-Takayama
R = QQ[x,y,z,w];
I = monomialIdeal(y^2, y*w, y*z, x*w^2)
S = standardPairs(I)
assert( setf {{1_R, {z, w}}, {y, {x}}, {1_R, {x, z}}, {w, {x, z}}} === setf S )
-- output: {{1, {z, w}}, {y, {x}}, {1, {x, z}}, {w, {x, z}}}

-- equation (3.14) in Saito-Sturmfels-Takayama
R = QQ[x,y]
J = monomialIdeal(x^2, x*y^2)
S = standardPairs(J)
assert( setf {{1_R, {y}}, {x, {}}, {x*y, {}}} === setf S )
-- output: {{1, {y}}, {x, {}}, {x*y, {}}}

-- page 115 in Saito-Sturmfels-Takayama
R = QQ[a,b,c,d,e]
K = monomialIdeal(b*c, a^3*d^2)
S = standardPairs(K)
assert( setf S === setf {{1_R, {a, b, e}}, {d, {a, b, e}}, {1_R, {c, d, e}}, 
         {a, {c, d, e}}, {a^2, {c, d, e}}, {1_R, {b, d, e}}, 
         {a, {b, d, e}}, {a^2, {b, d, e}}, {1_R, {a, c, e}}, 
         {d, {a, c, e}}})

-- output: {{1, {a, b, e}}, {d, {a, b, e}}, {1, {c, d, e}}, 
--          {a, {c, d, e}}, {a^2, {c, d, e}}, {1, {b, d, e}}, 
--          {a, {b, d, e}}, {a^2, {b, d, e}}, {1, {a, c, e}}, 
--          {d, {a, c, e}}}



--  ISSQUAREFREE  ------------------------------------------

R = QQ[x,y,z];
J = monomialIdeal(x^3*y^5*z, y^5*z^4, y^3*z^5, x*y*z^5, 
           x^2*z^5, x^4*z^3, x^4*y^2*z^2, x^4*y^4*z)
assert not isSquareFree J
assert     isSquareFree radical J
I = monomialIdeal(z^5, x^2*z^2, x^4*y^3, x^3*y^5, y^4*z^3,
      y^2*z^4, x*y*z)
assert not isSquareFree(I)
assert     isSquareFree(radical I)



--  MONOMIALSUBIDEAL  --------------------------------------

R = QQ[a,b,c,d];
I = ideal(b*c, c^2 - b*d, -a*c+b^2)
M = monomialSubideal(I)
J = intersect(ideal leadTerm I, I)
assert( monomialIdeal J == J )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test monideal2.out"
-- End:
