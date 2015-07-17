-- this is the 1-dim component of the variety for the knot 6_3 (via Stavros Garoufalidis)
CC[z0, z1, z2, z3, z4, z5, w0, w1, w2, w3, w4, w5];
I = ideal(w2-w4,w1-w3,z5+w5-1,z4+w4-1,z3+w3-1,z2+w4-1,z1+w3-1,z0+w0-1,2*w4^3-w0*w3*w5-2*w0*w4*w5+w0*w3+w4^2+3*w0*w5+
      w3*w5-w3,2*w3*w4^2-w0*w3*w5-4*w0*w4*w5-w0*w3+4*w0*w4-3*w4^2+5*w0*w5-w3*w5+4*w4*w5-2*w0+w3-2*w4-2*w5,2*w0*w3^2-w0*
      w3*w5+2*w3^2*w5-2*w0*w4*w5-3*w0*w3-2*w3^2+2*w0*w4-w4^2+3*w0*w5-3*w3*w5+2*w4*w5+3*w3,w0*w3*w5^2-2*w3^2*w5^2+2*w0*
      w4*w5^2-3*w0*w3*w5+2*w3^2*w5-4*w0*w4*w5+w4^2*w5-3*w0*w5^2+3*w3*w5^2-2*w4*w5^2+2*w0*w3+2*w0*w4-2*w4^2+6*w0*w5-w3*
      w5+2*w4*w5-2*w0-2*w3-2*w4-2*w5,2*w0*w4^2*w5-2*w0*w4^2+w0*w3*w5-2*w4^2*w5+w0*w3+w4^2-3*w0*w5+w3*w5-w3,w0^2*w3*w5+2
      *w0^2*w4*w5+2*w3^2*w5^2-w0^2*w3-2*w0^2*w4+2*w0*w3*w4+w0*w4^2-3*w0^2*w5+w0*w3*w5-2*w3^2*w5-4*w0*w4*w5+2*w3*w4*w5-4
      *w3*w5^2+2*w0^2+w0*w3+2*w0*w4-2*w3*w4+4*w3*w5+2*w4*w5+2*w5^2,w0^3*w4*w5+w0*w4*w5^3-w0^3*w4+w0^2*w3*w4-w0^3*w5+13*
      w0^2*w4*w5+w0^2*w5^2+13*w0*w4*w5^2+w3*w4*w5^2-w0*w5^3-w4*w5^3+w0^3+w0^2*w3-15*w0^2*w4+15*w0*w3*w4+5*w0*w4^2-19*w0
      ^2*w5+12*w0*w3*w5-63*w0*w4*w5+15*w3*w4*w5+5*w4^2*w5-19*w0*w5^2+w3*w5^2-15*w4*w5^2+w5^3+12*w0^2+6*w0*w3+29*w0*w4-
      16*w3*w4-15*w4^2+36*w0*w5+6*w3*w5+29*w4*w5+12*w5^2-13*w0-7*w3-13*w4-13*w5,2*w0^3*w5^3+2*w0^3*w4^2-20*w0^3*w5^2-20
      *w0^2*w5^3+2*w4^2*w5^3-2*w0^4+2*w0^3*w3+4*w0^3*w4+4*w0^2*w3*w4-14*w0^2*w4^2+34*w0^3*w5+18*w0^2*w4*w5+94*w0^2*w5^2
      +18*w0*w4*w5^2+4*w3*w4*w5^2-14*w4^2*w5^2+34*w0*w5^3+2*w3*w5^3+4*w4*w5^3-2*w5^4+2*w0^2*w3-44*w0^2*w4+28*w0*w3*w4+
      10*w0*w4^2-128*w0^2*w5+41*w0*w3*w5-126*w0*w4*w5+28*w3*w4*w5+10*w4^2*w5-128*w0*w5^2+2*w3*w5^2-44*w4*w5^2+30*w0^2+
      13*w0*w3+68*w0*w4-32*w3*w4-29*w4^2+97*w0*w5+13*w3*w5+68*w4*w5+30*w5^2-28*w0-17*w3-28*w4-28*w5)
F = I_*;
end

restart
load "knot6_3-corrected.m2"
needsPackage "NumericalAlgebraicGeometry"
numericalIrreducibleDecomposition
numericalIrreducibleDecomposition (I,Software=>BERTINI)
-- numericalIrreducibleDecomposition (I,Software=>PHCPACK) -- can't do overdetermined 

-- NAG4M2: irreducible of degree 18 (not robust)
-- Bertini: ok, takes more than 1 hour

-- this is the 1-dim component of 
I = ideal (z0 + w0 - 1, z1 + w1 - 1, z2 + w2 - 1, z3 + w3 - 1, z4 + w4 - 1, z5 + w5 - 1,
    z0*z1*z2*z3*z5 - 1, 
    -z1^2*z3^2*w0*w5 + w1*w3, 
    -z0*z5*w1*w2*w4 + w0*w5, 
    -z0*z5*w2*w3*w4 + w0*w5, 
    w2^2*w4^2 - z2*z4*w0*w5)
mingens last decompose I
