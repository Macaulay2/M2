-- taken from:
-- Bernd Sturmfels, FOUR COUNTEREXAMPLES IN COMBINATORIAL ALGEBRAIC GEOMETRY.

-- 2
S = QQ[x,y,z]
Ideals = { ideal(x, y, z^8), ideal(y*z, x, y^2, z^7),
     ideal(y*z^2, x, y^2, z^6), 
     ideal(y*z^3, x, y^2, z^5),
     ideal(y*z^2, y^2*z, x, y^3, z^5), 
     ideal(y^2*z, y*z^3, x, y^3, z^4),
     ideal(x*y,x*z,y*z^2,x^2,y^2,z^5), 
     ideal(x*y,x*z,y*z^3,x^2,y^2,z^4),
     J = ideal(x*y,x*z^2,y*z^2,x^2,y^2,z^4), 
     ideal(x*y,y*z,x*z,x^2,y^2,z^6),
     ideal(x*y,x*z,y*z^2,y^2*z,x^2,y^3,z^4),
     I8 = ideal(x^2,x*y,x*z^2,y*z^2,y^2*z,z^3,y^3) 
     }

degtan = I -> (
     R := ring I;
     degree Hom(I, R^1/I))

hilbtan = I -> (
     Tangentspace = Hom(module I, S^1/I);
     << {numgens I, degree I, degtan I, I} << endl; 
     )

assert( degree J == 8 )
assert( degtan J == 36 )
assert( degree I8 == 8 )
assert( degtan I8 == 32 )

K = ideal (x^3, x^2*y, x*y*z, x^2*z, x*z^2, x*y^3, y^3*z, y^2*z^2, y*z^3, z^4, y^5)
assert( degree K == 16 )
assert( degtan K == 88 )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test 4b.out"
-- End:
