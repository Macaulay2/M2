-- notice 4 weights are given for 3 variables as part of this test
R = ZZ/101[t,x,z,MonomialOrder=>Weights=>4:-1,Global=>false]
--fixed!
--this bug was visible when building the debug version
--monomial overflow, Mike has to fix it
-- incorrect input to the monomial order routine trashes some memory
-- perhaps due to an array bound overflow
--when it finally gets to
--     void Monoid::mult(const_monomial m, const_monomial n, monomial result) const
--in monoid.ccp
-- we find trash in the first word of each monomial
--   (gdb) p *m@4			 
--   $14 = {0x53535353, 0x1, 0x1, 0x1}
--   (gdb) p *n@4			 
--   $15 = {0x53535353, 0x1, 0x0, 0x0}
--it also seems that the characteristic 101 has become stored in _P1 as 100
gb ideal "tz+z3,t3x+z5+xzt"

S = ZZ/101[x,y,z, MonomialOrder => Eliminate 2, MonomialSize => 16 ];
ourpoints = ideal(y^5-x^4, x*y^2-1, x^5-y^3, x^5+y^5+z^5-1)
gb ourpoints
assert( gens gb ourpoints === matrix {{z^65-13*z^60-23*z^55+30*z^50-21*z^45+5*z^40+6*z^35+44*z^30-z^25+22*z^20-46*z^15-39*z^10-16*z^5-48, y-4*z^60-20*z^55+36*z^50+36*z^45+43*z^40+40*z^35-12*z^30+40*z^25+37*z^20+7*z^15+42*z^10-47*z^5+14, x-17*z^60+11*z^55+2*z^45+z^40+13*z^35+38*z^30+41*z^25-45*z^20+z^15-z^10-22*z^5-32}} )


R = QQ[x,MonomialOrder => RevLex, Global => false]
x^7   -- used to overflow...
assert( degree oo == {7} )

R = QQ[x,MonomialSize => 8]
assert try ( x^127 * x ; false ) else true
assert try ( x^15^15 ; false ) else true
x^127

R = QQ[x,MonomialSize => 16]
assert try ( x^(2^15) ; false ) else true
x^(2^15-1)

R = QQ[x,Weights=>{2^16}]
assert( x^(2^13) > x^40 )
assert( x^(2^14) > x^40 )
assert( x^(2^15-1) > x^40 )
assert try ( x^(2^15) ; false ) else true
assert try ( x^(2^16) ; false ) else true
assert try ( x^(2^17) ; false ) else true

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test overflow.out"
-- End:
