-- this is the 1-dim component of the variety for the knot 6_3 with an error (via Stavros Garoufalidis)
CC[z0, z1, z2, z3, z4, z5, w0, w1, w2, w3, w4, w5];
I = ideal (w2-w4,w1-w3,z5+w5-1,z4+w4-1,z3+w3-1,z2+w4-1,z1+w3-1,z0+w0-1,2*w4^3-w0*w3*w5+2*w0*w4*w5+w0*w3+w4^2-3*w0*w5+w3
      *w5-w3,2*w3*w4^2+w0*w3*w5-4*w0*w4*w5+w0*w3+4*w0*w4-5*w4^2-3*w0*w5+w3*w5+4*w4*w5-2*w0-w3-6*w4-2*w5+4,2*w0*w3^2-3*w0
      *w3*w5+2*w3^2*w5-2*w0*w4*w5-w0*w3-2*w3^2+2*w0*w4-3*w4^2-w0*w5-w3*w5+2*w4*w5+w3-4*w4,3*w0*w3*w5^2-2*w3^2*w5^2+2*w0*
      w4*w5^2-5*w0*w3*w5+2*w3^2*w5-4*w0*w4*w5+3*w4^2*w5+w0*w5^2+w3*w5^2-2*w4*w5^2+2*w0*w3+2*w0*w4-2*w4^2+2*w0*w5+w3*w5+6
      *w4*w5-2*w0-2*w3-2*w4-2*w5+4,2*w0*w4^2*w5-2*w0*w4^2-w0*w3*w5-2*w4^2*w5-w0*w3+3*w4^2+5*w0*w5-w3*w5+w3,9*w0^2*w3*w5+
      14*w0^2*w4*w5-32*w0^2*w5^2+6*w3^2*w5^2+8*w0*w4*w5^2-9*w0^2*w3-14*w0^2*w4+14*w0*w3*w4+w0*w4^2+27*w0^2*w5-17*w0*w3*
      w5-6*w3^2*w5-64*w0*w4*w5+14*w3*w4*w5-8*w4^2*w5+24*w0*w5^2-12*w3*w5^2-8*w4*w5^2+6*w0^2+7*w0*w3+30*w0*w4-14*w3*w4+10
      *w4^2-10*w0*w5+10*w3*w5+18*w4*w5+6*w5^2-12*w0+2*w3-16*w4-12*w5);
F = I_*;
end

load "knot6_3-erroneous.m2"
needsPackage "NumericalAlgebraicGeometry"
numericalIrreducibleDecomposition I
numericalIrreducibleDecomposition (I,Software=>BERTINI)
-- NAG4M2: irreducible of degree 20
-- Bertini: finds 19 points, "can't classify"

-- this is the 1-dim component of 
I = ideal ( z0 + w0 - 1, z1 + w1 - 1, z2 + w2 - 1, z3 + w3 - 1, z4 + w4 - 1, z5 + w5 - 1,
    z0*z1*z2*z3*z5 + 1, 
    z1^2*z3^2*w0*w5 + w1*w3, 
    z0*z5*w1*w2*w4 + w0*w5, 
    z0*z5*w2*w3*w4 + w0*w5, 
    w2^2*w4^2 + z2*z4*w0*w5 
    ) 
decompose I