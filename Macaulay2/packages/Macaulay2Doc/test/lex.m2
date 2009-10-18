-- From: Javier Fernandez <jfernand@math.umass.edu>
-- Subject: known problem?
-- To: Macaulay2@math.uiuc.edu
-- Date: Fri, 18 Feb 2000 11:26:46 -0500 (EST)

stderr << " -- TEST BYPASSED -- " << endl

exit 0

gbTrace = 3
R = QQ[u,v,x,y,z,MonomialOrder=>Lex]
li = {x - 3*u-3*u*v^2+u^3, y-3*v-3*u^2*v+v^3, z-3*u^2+3*v^2}
gblex = gb ideal li					    -- takes forever, in all versions

///
R = ZZ/32003[u,v,x,y,z,h,MonomialOrder=>Lex]
li = {x - 3*u-3*u*v^2+u^3, y-3*v-3*u^2*v+v^3, z-3*u^2+3*v^2}
I = homogenize(ideal li,h)
gb I;

R = ZZ/32003[u,v,x,y,z,MonomialOrder=>Lex]
li = {x - 3*u-3*u*v^2+u^3, y-3*v-3*u^2*v+v^3, z-3*u^2+3*v^2}
I = ideal li
time doGB(gens I,0,{Lex=>5,Position})

///
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test lex.out"
-- End:

