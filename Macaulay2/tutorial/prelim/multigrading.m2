-- Tutorial:Multi-gradings
--
-- In this tutorial we describe some of the 
-- basic operations involving multi-graded
-- rings.
--
-- As in other tutorials, our favorite field:

KK = ZZ/32003

-- 
--$
R = KK[a..d,
       Degrees=>{{1,4,0},{1,3,1},
                {1,1,3},{1,0,4}}]

-- We use the 'monomialCurve' script to find
-- the ideal of the rational quartic curve in P^3

I = monomialCurve(R,{1,3,4})
M = coker gens I

H = poincare coker gens I
substitute(H, {(ring H)_0 => 1})

C = res M
C.dd
degrees C_0
degrees C_1
degrees C_2
degrees C_3
mi = monomialIdeal o23
basis({5,10,10},M) ** R
basis({5,10,10}, R)
basis({10,20,20},M)

F = R^{{1,2,3}}
degrees F
basis({-1,-2,-3},F)

degree a
isHomogeneous (a^2*c^2*d-a*b^2*d^2)
isHomogeneous (a^2*c^2*d-a*c^2*d^2)


"In this example, our plan is to compute the multi-graded betti numbers of a
toric ideal and then to investigate the associated 'Spanish complexes'.
Along the way, we explain how to use multi-gradings in Macaulay 2.",

PARA,

TEX "Let's start with a simple example: the rational quartic curve in $P^3$.  This
is the ideal defining the image of the polynomial map $(s,t) |-> (s^4, s^3 t,
s t^3, t^4).$",

EXAMPLE "R = ZZ/32003[s,t]",
EXAMPLE "S = ZZ/32003[a..d]",
EXAMPLE "Sm = ZZ/32003[a..d,Degrees=>{{1,4,0},{1,3,1},{1,1,3},{1,0,4}}]",
EXAMPLE "use S",
EXAMPLE "f = map(R,S,matrix{{s^4, s^3*t, s*t^3, t^4}})",
EXAMPLE "I = generators kernel f",
EXAMPLE "J = substitute(I, Sm)",
EXAMPLE "poincare cokernel J",
EXAMPLE "C = resolution cokernel J",
EXAMPLE "degrees C_0",
EXAMPLE "degrees C_1",
EXAMPLE "degrees C_2",
EXAMPLE "degrees C_3"
}
