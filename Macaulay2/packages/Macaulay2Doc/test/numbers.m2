-- rings considered here: ZZ, QQ, RR_nn, CC_nn

assert( toRR 1 === 1. )
assert( toRR (1/10) === .1 )

assert ( (1p111 + 2^-110) =!= 1p111 )
assert ( (1p111 + 2^-111) === 1p111 )
assert ( (1p111 + 2^-110) != 1 )
assert ( (1p111 + 2^-111) == 1 )

isint = x -> floor x == x
assert isint .13454234234523487687p100e20
assert isint 4.13454234234523487687p100e20
assert isint 34.13454234234523487687p100e20
assert isint 234.13454234234523487687p100e20
assert isint 2341.3454234234523487687p100e19
assert isint 23413.454234234523487687p100e18
assert isint 234134.54234234523487687p100e17
assert isint 2341345.4234234523487687p100e16
assert isint 23413454.234234523487687p100e15
assert isint 234134542.34234523487687p100e14
assert isint 2341345423.4234523487687p100e13

assert isint .13454234234523487687e20
assert isint 4.13454234234523487687e20
assert isint 34.13454234234523487687e20
assert isint 234.13454234234523487687e20
assert isint 2341.3454234234523487687e19
assert isint 23413.454234234523487687e18
assert isint 234134.54234234523487687e17
assert isint 2341345.4234234523487687e16
assert isint 23413454.234234523487687e15
assert isint 234134542.34234523487687e14
assert isint 2341345423.4234523487687e13

ch = x -> value toExternalString x === x
assert ch .5873893305409771
assert ch 587.3893305409771
assert ch 587389.3305409771
assert ch 587389330.5409771
assert ch 58738933054.09771
assert ch .58738933054097710728398429265722p100e0
assert ch .73249827349273948274398273498273p100e0
assert ch( .32847293749287439287439237498274p100e0+ii*.73249827349273948274398273498273p100e0)
assert ch( .27349827349287439827439827349827p100e0+ii*.92837938475938759387539847539847p100e0)

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

assert( 1.*ii < 2.*ii )
assert( (1.*ii ? 2.*ii) == symbol < )
assert( 3+1.*ii < 3+2.*ii )
assert( (3+1.*ii ? 3+2.*ii) == symbol < )
assert( 3+1.*ii < 4+2.*ii )
assert( (3+1.*ii ? 4+2.*ii) == symbol < )

assert( 1.*ii > 0.*ii )
assert( (1.*ii ? 0.*ii) == symbol > )
assert( 3+1.*ii > 3+0.*ii )
assert( (3+1.*ii ? 3+0.*ii) == symbol > )
assert( 5+1.*ii > 4+0.*ii )
assert( (5+1.*ii ? 4+0.*ii) == symbol > )

assert(3 < 3+ii)
assert(3 > 3-ii)
assert(3. < 3+ii)
assert(3. > 3-ii)
assert(3/1 < 3+ii)
assert(3/1 > 3-ii)
x = .234p100
y = 1.234p100
z = x + y * ii
assert( (abs z) === .12559904458235341210425337079217p100e1 )
assert( (abs(-3.2)) === 3.2 )
assert( (abs(3.2)) === 3.2 )

x = symbol x
R = RR[x]
assert( value toString(x+1/3) =!= x+1/3 )
assert( value toExternalString(x+1/3) === x+1/3 )

assert( toRR infinity === - log 0 )
assert( toRR (-infinity) === log 0 )

assert( isReal 1 )
assert( isReal 1. )
assert( isReal(1. + 0 * ii ) )
assert( not isReal (1.- ii) )
assert( isReal (1/1) )

assert( instance(log(3.), RR) )
assert( instance(log(3/1), RR) )
assert( instance(log(3), RR) )
assert( instance(log(-3.), CC) )
assert( instance(log(-3/1), CC) )
assert( instance(log(-3), CC) )

assert( instance(log(3.,2), RR) )
assert( instance(log(3/1,2), RR) )
assert( instance(log(3,2), RR) )
assert( instance(log(-3.,2), CC) )
assert( instance(log(-3/1,2), CC) )
assert( instance(log(-3,2), CC) )

assert( instance(log(3.,-2), CC) )
assert( instance(log(3/1,-2), CC) )
assert( instance(log(3,-2), CC) )
assert( instance(log(-3.,-2), CC) )
assert( instance(log(-3/1,-2), CC) )
assert( instance(log(-3,-2), CC) )

assert( instance(log(3.,2.), RR) )
assert( instance(log(3/1,2.), RR) )
assert( instance(log(3,2.), RR) )
assert( instance(log(-3.,2.), CC) )
assert( instance(log(-3/1,2.), CC) )
assert( instance(log(-3,2.), CC) )

assert( instance(log(3.,-2.), CC) )
assert( instance(log(3/1,-2.), CC) )
assert( instance(log(3,-2.), CC) )
assert( instance(log(-3.,-2.), CC) )
assert( instance(log(-3/1,-2.), CC) )
assert( instance(log(-3,-2.), CC) )

assert( instance(log(3.,2/1), RR) )
assert( instance(log(3/1,2/1), RR) )
assert( instance(log(3,2/1), RR) )
assert( instance(log(-3.,2/1), CC) )
assert( instance(log(-3/1,2/1), CC) )
assert( instance(log(-3,2/1), CC) )

assert( instance(log(3.,-2/1), CC) )
assert( instance(log(3/1,-2/1), CC) )
assert( instance(log(3,-2/1), CC) )
assert( instance(log(-3.,-2/1), CC) )
assert( instance(log(-3/1,-2/1), CC) )
assert( instance(log(-3,-2/1), CC) )

scan(((3.), (3/1), (3), (-3.), (-3/1), (-3)), x -> assert( abs(exp(log(x)) - x) < 1e-10) )
scan((
(3.,2), (3/1,2), (3,2), (-3.,2),
(-3/1,2), (-3,2), (3.,-2), (3/1,-2), (3,-2), (-3.,-2), (-3/1,-2), (-3,-2),
(3.,2.), (3/1,2.), (3,2.), (-3.,2.), (-3/1,2.), (-3,2.), (3.,-2.), (3/1,-2.),
(3,-2.), (-3.,-2.), (-3/1,-2.), (-3,-2.), (3.,2/1), (3/1,2/1), (3,2/1),
(-3.,2/1), (-3/1,2/1), (-3,2/1), (3.,-2/1), (3/1,-2/1), (3,-2/1), (-3.,-2/1),
(-3/1,-2/1), (-3,-2/1)
), (b,x) -> assert(abs(log(b,x)-log x/log b)<1e-10))

assert( format_10 .00044448888 === ".00044448888" )
assert( format_9 .00044448888 === ".00044448888" )
assert( format_8 .00044448888 === ".00044448888" )
assert( format_6 .00044448888 === ".000444489" )
assert( format_5 .00044448888 === ".00044449" )
assert( format_4 .00044448888 === ".0004445" )
assert( format_3 .00044448888 === ".000444" )

assert( format_(10,11) .00044448888 === ".00044448888" )
assert( format_(10,10) .00044448888 === ".0004444889" )
assert( format_(10,9) .00044448888 === ".000444489" )
assert( format_(10,8) .00044448888 === ".00044449" )
assert( format_(10,7) .00044448888 === ".0004445" )
assert( format_(10,6) .00044448888 === ".000444" )
assert( format_(10,5) .00044448888 === ".00044" )
assert( format_(10,4) .00044448888 === ".0004" )
assert( format_(10,3) .00044448888 === "0" )
assert( format_(10,4) .00088888888 === ".0009" )
assert( format_(10,4) .00098765432 === ".001" )

assert( format_(10,) 44.448888 === "44.448888" )
assert( format_(10,-1) 44.448888 === "44.448888" )
assert( format_(10,7) 44.448888 === "44.448888" )
assert( format_(10,6) 44.448888 === "44.448888" )
assert( format_(10,5) 44.448888 === "44.44889" )
assert( format_(10,4) 44.448888 === "44.4489" )
assert( format_(10,3) 44.448888 === "44.449" )
assert( format_(10,2) 44.448888 === "44.45" )
assert( format_(10,1) 44.448888 === "44.4" )
assert( format_(10,0) 44.448888 === "44" )

assert( format_(10,) 4.4448888e-50 === "4.4448888e-50" )
assert( format_(10,-1) 4.4448888e-50 === "4.4448888e-50" )
assert( format_(10,58) 4.4448888e-50 === "4.4448888e-50" )
assert( format_(10,57) 4.4448888e-50 === "4.4448888e-50" )
assert( format_(10,56) 4.4448888e-50 === "4.444889e-50" )
assert( format_(10,55) 4.4448888e-50 === "4.44489e-50" )
assert( format_(10,54) 4.4448888e-50 === "4.4449e-50" )
assert( format_(10,53) 4.4448888e-50 === "4.445e-50" )
assert( format_(10,52) 4.4448888e-50 === "4.44e-50" )
assert( format_(10,51) 4.4448888e-50 === "4.4e-50" )
assert( format_(10,50) 4.4448888e-50 === "4e-50" )
assert( format_(10,49) 4.4448888e-50 === "0" )

assert( format_(10,) 7.7778888e-50 === "7.7778888e-50" )
assert( format_(10,-1) 7.7778888e-50 === "7.7778888e-50" )
assert( format_(10,58) 7.7778888e-50 === "7.7778888e-50" )
assert( format_(10,57) 7.7778888e-50 === "7.7778888e-50" )
assert( format_(10,56) 7.7778888e-50 === "7.777889e-50" )
assert( format_(10,55) 7.7778888e-50 === "7.77789e-50" )
assert( format_(10,54) 7.7778888e-50 === "7.7779e-50" )
assert( format_(10,53) 7.7778888e-50 === "7.778e-50" )
assert( format_(10,52) 7.7778888e-50 === "7.78e-50" )
assert( format_(10,51) 7.7778888e-50 === "7.8e-50" )
assert( format_(10,50) 7.7778888e-50 === "8e-50" )
assert( format_(10,49) 7.7778888e-50 === "1e-49" )
assert( format_(10,48) 7.7778888e-50 === "0" )
assert( format_(10,47) 7.7778888e-50 === "0" )


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test numbers.out"
-- End:
