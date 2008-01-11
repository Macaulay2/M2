-- rings considered here: ZZ, QQ, RR_nn, CC_nn

assert( toRR 1 === 1. )
assert( toRR (1/10) === .1 )

assert( 1. === 1. )
assert( 1. =!= .1 )
assert( hash 1p111 =!= hash 1p110 )
assert( 1p111 =!= 1p110 )
assert( toCC 1p111 =!= toCC 1p110 )
assert( toCC 1p111 =!= toCC 1p110 )
assert( hash (1p111*ii) =!= hash (1p110*ii) )
assert( hash (1p111*ii) === hash (1p111*ii) )
assert( realPart toCC 1. === 1. )
assert( imaginaryPart toCC 1. === 0. )

assert try (lift(1.3,ZZ); false) else true
assert not liftable(1.3,ZZ)
assert liftable(1.,ZZ)
assert (lift(1.,ZZ) === 1)

ring 4.
RR _ (precision 4.)
assert(oo===ooo)

ring (4.*ii)
CC _ (precision 4.)
assert(oo===ooo)

d = defaultPrecision
defaultPrecision = 100
assert(precision 1. == 100)
defaultPrecision = d

ring (4.*ii)
CC _ (precision 4.)
assert(oo===ooo)

M = matrix{{1.3,2.4567}}
assert (ring M === ring 1.3)

assert( 1_(RR_44) == 1 )
assert( precision 1_(RR_44) == 44 )
assert( 1_RR == 1 )
assert( precision 1_RR == defaultPrecision )

assert( 1_(CC_44) == 1 )
assert( precision 1_(CC_44) == 44 )
assert( 1_CC == 1 )
assert( precision 1_CC == defaultPrecision )

assert( ii_(CC_44) == ii )
assert( precision ii_(CC_44) == 44 )
assert( ii_CC == ii )
assert( precision ii_CC == defaultPrecision )

assert( promote(1.,RR_222) === 1p222 )

assert( promote(1,RR) === 1. )
assert( promote(1,RR_222) === 1p222 )
assert( promote(1/1,RR) === 1. )
assert( promote(1/1,RR_222) === 1p222 )

assert( promote(1,CC) == 1 )
assert( promote(1,CC) == 1. )
assert( promote(1,CC) === 1.+0*ii )
assert( promote(1,CC) === toCC 1 )
assert( promote(1,CC_222) == 1p222+0*ii )
assert( promote(1,CC_222) =!= 1p222+0*ii )
assert( promote(1,CC_222) === toCC 1p222 )
assert( promote(1/1,CC) == 1 )
assert( promote(1/1,CC) == 1. )
assert( promote(1/1,CC) === 1.+0*ii )
assert( promote(1/1,CC_222) === toCC 1p222 )
assert( promote(1.,CC) == 1 )
assert( promote(1.,CC) == 1. )
assert( promote(1.,CC) === 1.+0*ii )
assert( promote(1.,CC_222) === toCC 1p222 )
assert( promote(1.,CC_222) === toCC 1p222 )
assert( not liftable(+ii,RR) )
assert( liftable(0*ii,RR) )
assert( lift(3.5,QQ) === 7/2 )
assert( lift(3.5+0*ii,QQ) === 7/2 )
assert( lift(3331333p66/3333133,QQ) === 3331333/3333133 )
assert( numeric 1 === 1. )
assert( numeric {1} === {1.} )
assert( numeric ii === 0.+ii )
assert( numeric_44 1 === 1p44 )
assert( numeric_44 {1} === {1p44} )
assert( numeric_44 ii === 0p44+ii )
assert( 1_RR === 1. )
assert( 1_CC === ii/ii )
assert( (1/1)_RR === 1. )
assert( (1/1)_CC === ii/ii )
assert( 1._RR === 1. )
assert( 1._CC === ii/ii )
assert( (toCC 1.)_CC === ii/ii )
assert( ring 1. == default RR )
assert( ring(1+ii) == default CC )
assert( ring 1p44 == RR_44 )
assert( ring(1p44+ii) == CC_44 )
assert( round 3.4 === 3 )
assert( round 3.5 === 4 )
assert( round 4.5 === 4 )
assert( round_1 .34 === .3 )
assert( round_1 .35 === .4 )
assert( round_1 .45 === .4 )
setRandomSeed 4
assert( apply(20,i -> random 100) === {47, 38, 51, 74, 28, 50, 44, 25, 72, 16, 41, 61, 76, 89, 28, 27, 77, 34, 26, 57} ) -- version 1.1
setRandomSeed 4
assert( random RR === .75495928896616160p53e-2 )
setRandomSeed 4
assert( random RR_100 === .70502447625750651317270153955621p100e-1 )
setRandomSeed 4
assert( random CC === .75495928896616160p53e-2+.70502447625750642p53e-1*ii )
setRandomSeed 4
assert( random CC_100 === .70502447625750651317270153955621p100e-1+.58738933054097710728398429265722p100e0*ii)

RR[x]
f = (1+x)^5
CC[x]

assert( toString RR_3 == "RR_3" )
assert( toString CC_3 == "CC_3" )
assert( toString RR == "RR" )
assert( toString CC == "CC" )

assert( precision matrix {{1.}} === precision 1. )

assert( promote(matrix {{1.}}, CC_33) === matrix {{toCC 1p33}})
assert( promote(matrix {{1.}}, CC_33) === matrix {{toCC 1p33}})
assert( promote(matrix {{1.}}, RR_33) === matrix {{1p33}})
assert( promote(matrix {{1.}}, RR) === matrix {{1.}})
assert( promote(matrix {{1}}, CC_33) === matrix {{toCC 1p33}})
assert( promote(matrix {{1}}, CC) === matrix {{toCC 1.}})
assert( promote(matrix {{1}}, RR_33) === matrix {{1p33}})
assert( promote(matrix {{1}}, RR) === matrix {{1.}})
assert( promote(matrix {{1/1}}, CC_33) === matrix {{toCC 1p33}})
assert( promote(matrix {{1/1}}, CC) === matrix {{toCC 1.}})
assert( promote(matrix {{1/1}}, RR_33) === matrix {{1p33}})
assert( promote(matrix {{1/1}}, RR) === matrix {{1.}})
assert( promote(matrix {{toCC 1.}}, CC) === matrix {{toCC 1.}})
assert( promote(matrix {{toCC 1}}, CC_33) === matrix {{toCC 1p33}})

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2/test numbers.out"
-- End:
