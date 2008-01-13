R = ZZ/101[t,x,z,MonomialOrder=>Weights=>4:-1,Global=>false]-- notice the error
--status: monomial overflow, Dan will debug
gb ideal "tz+z3,t3x+z5+xzt"

S = ZZ/101[x,y,z, MonomialOrder => Eliminate 2, MonomialSize => 16 ];
ourpoints = ideal(y^5-x^4, x*y^2-1, x^5-y^3, x^5+y^5+z^5-1)
gb ourpoints
assert( gens gb ourpoints === matrix {{z^65-13*z^60-23*z^55+30*z^50-21*z^45+5*z^40+6*z^35+44*z^30-z^25+22*z^20-46*z^15-39*z^10-16*z^5-48, y-4*z^60-20*z^55+36*z^50+36*z^45+43*z^40+40*z^35-12*z^30+40*z^25+37*z^20+7*z^15+42*z^10-47*z^5+14, x-17*z^60+11*z^55+2*z^45+z^40+13*z^35+38*z^30+41*z^25-45*z^20+z^15-z^10-22*z^5-32}} )


R = QQ[x,MonomialOrder => RevLex, Global => false]
x^7							    -- monomial overflow
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
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test overflow.out"
-- End:
