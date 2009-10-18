test = (C,V) -> (
     assert( not isHomogeneous C or isHomogeneous V );
     assert( not isHomogeneous C or isHomogeneous C.minimalPresentationMap );
     assert( not isHomogeneous C or isHomogeneous C.minimalPresentationMapInv );
     assert( target C.minimalPresentationMap === V );
     assert( source C.minimalPresentationMap === C );
     assert( target C.minimalPresentationMapInv === C );
     assert( source C.minimalPresentationMapInv === V );
     assert( C.minimalPresentationMap * C.minimalPresentationMapInv == 1 );
     assert( C.minimalPresentationMapInv * C.minimalPresentationMap == 1 );
     assert isWellDefined C.minimalPresentationMapInv;
     assert isWellDefined C.minimalPresentationMap;
     assert( kernel C.minimalPresentationMap == 0 );
     assert( kernel C.minimalPresentationMapInv == 0 );
     )

C=ZZ/101[x,y,z,Degrees => {2,3,1}]/ideal(x-x^2-y,z+x*y)
V= time minPres(C)
test(C,V)
assert( gens V == {x} )
assert( degrees V == {{2}} )

C=ZZ/101[x,y,z,Degrees => {{1,2},{1,3},{1,1}}]/ideal(x-x^2-y,z+x*y)
V = time minPres(C)
test(C,V)
assert( numgens C == 3 )
assert( numgens V == 1 )
assert( numgens ideal C == 2 )
assert( numgens ideal V == 0 )

assert( gens V == {x} )
assert( degrees V == {{1,2}} )

C=ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2)
V= time minPres(C)
assert( gens V == {x,u,w} )
use ring ideal V
assert(ideal V == ideal(u^2-w^2))

w = symbol w
y = symbol y
S = ZZ/101[w_16, w_11, w_12, w_13, w_14, w_15, w_8, w_9, w_10, y_1, y_2, y_3, y_4, y_5, y_6, y_7, y_8,
     Degrees => {{1}, {1}, {2}, {2}, {2}, {2}, {2}, {3}, {3}, {1}, {1}, {1}, {1}, {1}, {1}, {1}, {1}}, MonomialOrder => ProductOrder {1, 5, 3, 8}, MonomialSize => 16]

J=ideal(y_2*y_6-y_3*y_7,w_11*y_6-w_8,w_11*y_1-y_3*y_7,w_11^2-w_15,w_16*y_6-y_1*y_5,w_16*y_3-w_13,w_16*y_2-w_15,w_16*y_1-w_8,w_16*w_11-y_2*y_5,w_16^2-w_11*y_5,y_1*y_4*y_5-y_2*y_3*y_8,w_11*y_3*y_8-y_4*y_5*y_6,w_11*y_2*y_8-y_4*y_5*y_7,w_11*y_4*y_7-w_9,w_14*y_6-y_1*y_2*y_8,w_14*y_5-w_15*y_8,w_14*y_3-w_8*y_4,w_14*y_2-w_9,w_14*y_1-y_4*y_6*y_7,w_12*y_7-y_1*y_2*y_8,w_12*y_6-y_1*y_3*y_8,w_12*y_5-w_13*y_8,w_12*y_3-w_10,w_12*y_2-w_8*y_4,w_12*y_1-y_4*y_6^2,w_11*w_14-y_2^2*y_8,w_11*w_12-y_2*y_3*y_8,w_16*y_4*y_7-y_2^2*y_8,w_16*w_14-y_4*y_5*y_7,w_16*w_12-y_4*y_5*y_6,w_14^2-y_2*y_4*y_7*y_8,w_12*w_14-y_3*y_4*y_7*y_8,w_12^2-y_3*y_4*y_6*y_8)

J1 = minPres(J)
use ring J1
assert( J1 == ideal(y_2*y_6-y_3*y_7,y_1*y_5-w_16*y_6,w_11*y_1-y_3*y_7,w_16*y_1-w_11*y_6,w_11^2-w_16*y_2,w_16*w_11-y_2*y_5,w_16^2-w_11*y_5,y_4*y_5*y_7-w_11*y_2*y_8,w_16*y_4*y_7-y_2^2*y_8,w_12*y_7-y_1*y_2*y_8,y_4*y_5*y_6-w_11*y_3*y_8,w_14*y_6-y_1*y_2*y_8,w_12*y_6-y_1*y_3*y_8,w_16*y_4*y_6-y_2*y_3*y_8,w_14*y_5-w_16*y_2*y_8,w_12*y_5-w_16*y_3*y_8,w_14*y_3-w_11*y_4*y_6,w_14*y_2-w_11*y_4*y_7,w_12*y_2-w_11*y_4*y_6,w_14*y_1-y_4*y_6*y_7,w_12*y_1-y_4*y_6^2,w_11*w_14-y_2^2*y_8,w_16*w_14-w_11*y_2*y_8,w_11*w_12-y_2*y_3*y_8,w_16*w_12-w_11*y_3*y_8,w_14^2-y_2*y_4*y_7*y_8,w_12*w_14-y_3*y_4*y_7*y_8,w_12^2-y_3*y_4*y_6*y_8) )

C = S/J
V = time minPres C
assert( target C.minimalPresentationMap === V )
assert( source C.minimalPresentationMap === C )
assert( target C.minimalPresentationMapInv === C )
assert( source C.minimalPresentationMapInv === V )
assert( C.minimalPresentationMap * C.minimalPresentationMapInv == 1 )
assert( C.minimalPresentationMapInv * C.minimalPresentationMap == 1 )
assert( numgens C == 17 )
assert( numgens V <= 12 )
assert( numgens J == 33 )
assert( numgens ideal V <= 28 )

C=ZZ/101[x,y,z,u,w]/ideal(x-x^2-y,z+x*y,w^2-u^2)
V= time minimalPresentation(C,Exclude=>{1})
describe V
use V
assert(gens V === {x,y,u,w})

V= time minimalPresentation(ideal C,Exclude=>{1})
use ring V
assert(gens ring V === {x,y,u,w})

A = QQ[t]
assert try V = minimalPresentation(ideal C,Exclude=>{a}) else true
assert try V = minimalPresentation(ideal C,Exclude=>{t}) else true

