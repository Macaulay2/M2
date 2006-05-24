R = QQ[x,y,z,MonomialOrder=>Lex]
gb ideal {x^5+y^4+z^3-1, x^3+y^2+z^2-1}


S = QQ[a..d]
I = ideal(a^2-1, a^3+2)
T = S/I
assert( T == 0 )

-- May 23, 2006:
R = ZZ[s,t,x,y,z, MonomialOrder=>{2,3}];
I = ideal(x-s^3-s*t-1, y-t^3-3*t^2-t, z-s*t^3)
leadTerm gens gb I  --crashes on my macintel.

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test gb3.out"
-- End:
