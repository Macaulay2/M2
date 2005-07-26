R = QQ[x,y,z,MonomialOrder=>Lex]
gb ideal {x^5+y^4+z^3-1, x^3+y^2+z^2-1}


S = QQ[a..d]
I = ideal(a^2-1, a^3+2)
T = S/I
assert( T == 0 )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test gb3.out"
-- End:
