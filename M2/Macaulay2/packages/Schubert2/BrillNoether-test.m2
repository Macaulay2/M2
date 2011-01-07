load "./BrillNoether.m2"

E = BrillNoetherBundle(5,1,4)
Wrd = variety E
euler Wrd
assert(euler Wrd === -20)			     -- this curve has genus 11


CxP = CxPic(3,5)
assert( ch CxP.PoincareBundle == 1 + (gamma + 5*eta) - theta*eta )
pi2 = CxP.projections#1
E = pi2_* CxP.PoincareBundle
ch E
use ring oo
assert( ch E == 3 - theta )
