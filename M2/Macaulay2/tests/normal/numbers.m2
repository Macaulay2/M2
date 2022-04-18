-- rings considered here: ZZ, QQ, RR_nn, CC_nn

assert( -0. =!= 0. )
assert( -0. == 0. )

assert( toRR 1 === 1. )
assert( toRR (1/10) === .1 )
assert( 0. + (1/10) === .1 )
assert( 1. * (1/10) === .1 )

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
assert ch 0.
assert ch (-0.)
assert ch toCC(53,-0.,0.)
assert ch toCC(53,0.,0.)
assert ch toCC(53,-0.,-0.)
assert ch toCC(53,0.,-0.)
assert ch .5873893305409771
assert ch 587.3893305409771
assert ch 587389.3305409771
assert ch 587389330.5409771
assert ch 58738933054.09771
assert ch .58738933054097710728398429265722p100e0
assert ch .73249827349273948274398273498273p100e0
assert ch( .32847293749287439287439237498274p100e0+ii*.73249827349273948274398273498273p100e0)
assert ch( .27349827349287439827439827349827p100e0+ii*.92837938475938759387539847539847p100e0)
assert ch( .32847293749287439287439237498274p100e0-ii*.73249827349273948274398273498273p100e0)
assert ch( .27349827349287439827439827349827p100e0-ii*.92837938475938759387539847539847p100e0)
assert ch( -.32847293749287439287439237498274p100e0+ii*.73249827349273948274398273498273p100e0)
assert ch( -.27349827349287439827439827349827p100e0+ii*.92837938475938759387539847539847p100e0)
assert ch( -.32847293749287439287439237498274p100e0-ii*.73249827349273948274398273498273p100e0)
assert ch( -.27349827349287439827439827349827p100e0-ii*.92837938475938759387539847539847p100e0)
assert ch( .32847293749287439287439237498274p100e0+ii*0)
assert ch( .27349827349287439827439827349827p100e0+ii*0)
assert ch( -.32847293749287439287439237498274p100e0+ii*0)
assert ch( -.27349827349287439827439827349827p100e0+ii*0)
assert ch( +ii*.73249827349273948274398273498273p100e0)
assert ch( +ii*.92837938475938759387539847539847p100e0)
assert ch( -ii*.73249827349273948274398273498273p100e0)
assert ch( -ii*.92837938475938759387539847539847p100e0)

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
assert( ring 1. === default RR )
assert( ring(1+ii) === default CC )
assert( ring 1p44 === RR_44 )
assert( ring(1p44+ii) === CC_44 )
assert( round 3.4 === 3 )
assert( round 3.5 === 4 )
assert( round 4.5 === 4 )
assert( round_1 .34 === .3 )
assert( round_1 .35 === .4 )
assert( round_1 .45 === .4 )
setRandomSeed 4
assert( apply(20,i -> random 100) === {47, 38, 51, 74, 28, 50, 44, 25, 72, 16, 41, 61, 76, 89, 28, 27, 77, 34, 26, 57} ) -- version 1.1

-*
    The MPFR library changed the way it generates random real numbers in version 3.1, see
    their file doc/mpfr.texi .

    generateAssertions ///
    setRandomSeed 4; random RR
    setRandomSeed 4; random RR_100
    setRandomSeed 4; random CC
    setRandomSeed 4; random CC_100
    ///
    *-
   assert( (setRandomSeed 4; random RR) === .46156623802715113p53 );
   assert( (setRandomSeed 4; random RR_100) === .67753449342864841445246801151208p100 );
   assert( (setRandomSeed 4; random CC) === toCC(.46156623802715113p53,.3890127375373339p53) );
   assert( (setRandomSeed 4; random CC_100) === toCC(.67753449342864841445246801151208p100,.79330191647933724509628456990079p100) );

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

assert( abs(0.) === 0. )
assert( abs(0.) =!= -0. )
assert( abs(-0.) === 0. )
assert( abs(-0.) =!= -0. )

x = symbol x
R = RR[x]
assert( value toString(x+1/3) =!= x+1/3 )
assert( value toExternalString(x+1/3) === x+1/3 )

assert( numeric infinity === - log 0 )
assert( numeric (-infinity) === log 0 )

assert( isReal 1 )
assert( isReal 1. )
assert( isReal(1. + 0 * ii ) )
assert( not isReal (1.- ii) )
assert( isReal (1/1) )
assert( isReal pi )
assert( not isReal ii )
assert( not isReal infinity )

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
assert( format_(10,0) .00044448888 === "0" )

assert( format_(10,4) .00088888888 === ".0009" )
assert( format_(10,0) .00088888888 === "0" )

assert( format_(10,4) .00098765432 === ".001" )
assert( format_(10,0) .00098765432 === "0" )

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

assert ( 1 == # set (toString \ { toCC(53,-0.,0.), toCC(53,0.,0.), toCC(53,-0.,-0.), toCC(53,0.,-0.) } ) )
assert ( 4 == # set (toExternalString \ { toCC(53,-0.,0.), toCC(53,0.,0.), toCC(53,-0.,-0.), toCC(53,0.,-0.) } ) )


assert( toRR(100,-0.) === -0.p100 )
assert( toRR(100,0.) === 0.p100 )

distinct = x -> scan(#x,i->scan(#x,j-> assert( (x#i === x#j) === (i===j) )))
distinct { toCC(-0.,0.), toCC(0.,0.), toCC(-0.,-0.), toCC(0.,-0.) }
distinct { toCC(53,-0.,0.), toCC(53,0.,0.), toCC(53,-0.,-0.), toCC(53,0.,-0.) }
distinct (hash \ { toCC(53,-0.,0.), toCC(53,0.,0.), toCC(53,-0.,-0.), toCC(53,0.,-0.) })

assert ( 4 == # set { toCC(-0.,0.), toCC(0.,0.), toCC(-0.,-0.), toCC(0.,-0.) } )
assert ( 4 == # set { toCC(53,-0.,0.), toCC(53,0.,0.), toCC(53,-0.,-0.), toCC(53,0.,-0.) } )
assert ( 4 == # set (hash \ { toCC(53,-0.,0.), toCC(53,0.,0.), toCC(53,-0.,-0.), toCC(53,0.,-0.) } ) )

toExternalString (0 *(1-ii))
toExternalString (0.*(1-ii))
assert( 0*(1-ii) === 0.*(1-ii) )

epsilon = 10 * 2.^-defaultPrecision
small = z -> abs z < epsilon
see = z -> (<< "see: " << z << endl; z)
scan( {exp,log,sin,cos,sinh,cosh,tanh,coth,asin,acos,asinh,acosh,atan,acot,cot,tan,csc,sec}, 
     f -> (
	  << f << endl;
	  assert small see ( f(.21 + 0.e-12*ii) - f .21);
	  assert small see ( f(2.1 + 0.e-12*ii) - f 2.1);
	  )
     )

scan({.2, 1.2, .2 + .3*ii,  1.2 - .13*ii}, z -> (
     	  assert small(z - acosh cosh z);
	  assert small(z - acos cos z);
	  assert small(z - asinh sinh z);
	  assert small(z - asin sin z);
	  assert small(z - atan tan z);
	  assert small(z - acot cot z);
	  assert small(z - log exp z);
	  ))

assert( class asin .3 === RR )
assert( class asin 1.3 === CC )
assert( class acos .3 === RR )
assert( class acos 1.3 === CC )

assert( class acot .3 === RR )
assert( class acot 1.3 === RR )
assert( class acot (-.3) === RR )
assert( class acot (-1.3) === RR )

assert( class asinh .3 === RR )
assert( class asinh 1.3 === RR )
assert( class acosh .3 === CC )
assert( class acosh 1.3 === RR )

assert( class ( (2) ^ (3) ) === ZZ )
assert( class ( (-2) ^ (3) ) === ZZ )
assert( class ( (2) ^ (-3) ) === QQ )
assert( class ( (-2) ^ (-3) ) === QQ )

assert( class ( (2) ^ (2/3) ) === RR )
assert( class ( (-2) ^ (2/3) ) === RR )
assert( class ( (2) ^ (-2/3) ) === RR )
assert( class ( (-2) ^ (-2/3) ) === RR )

assert( class ( (2) ^ (3/2) ) === RR )
assert( class ( (-2) ^ (3/2) ) === CC )
assert( class ( (2) ^ (-3/2) ) === RR )
assert( class ( (-2) ^ (-3/2) ) === CC )

assert( class ( (2) ^ (3.) ) === RR )
assert( class ( (-2) ^ (3.) ) === CC )
assert( class ( (2) ^ (-3.) ) === RR )
assert( class ( (-2) ^ (-3.) ) === CC )

assert( class ( (2) ^ (3.+ii) ) === CC )
assert( class ( (-2) ^ (3.+ii) ) === CC )

assert( class ( (2/3) ^ (3) ) === QQ )
assert( class ( (-2/3) ^ (3) ) === QQ )
assert( class ( (2/3) ^ (-3) ) === QQ )
assert( class ( (-2/3) ^ (-3) ) === QQ )

assert( class ( (2/3) ^ (2/3) ) === RR )
assert( class ( (-2/3) ^ (2/3) ) === RR )
assert( class ( (2/3) ^ (-2/3) ) === RR )
assert( class ( (-2/3) ^ (-2/3) ) === RR )

assert( class ( (2/3) ^ (3/2) ) === RR )
assert( class ( (-2/3) ^ (3/2) ) === CC )
assert( class ( (2/3) ^ (-3/2) ) === RR )
assert( class ( (-2/3) ^ (-3/2) ) === CC )

assert( class ( (2/3) ^ (3.) ) === RR )
assert( class ( (-2/3) ^ (3.) ) === CC )
assert( class ( (2/3) ^ (-3.) ) === RR )
assert( class ( (-2/3) ^ (-3.) ) === CC )

assert( class ( (2/3) ^ (3.+ii) ) === CC )
assert( class ( (-2/3) ^ (3.+ii) ) === CC )

assert( class ( (2.) ^ (3) ) === RR )
assert( class ( (-2.) ^ (3) ) === RR )
assert( class ( (2.) ^ (-3) ) === RR )
assert( class ( (-2.) ^ (-3) ) === RR )

assert( class ( (2.) ^ (2/3) ) === RR )
assert( class ( (-2.) ^ (2/3) ) === RR )
assert( class ( (2.) ^ (-2/3) ) === RR )
assert( class ( (-2.) ^ (-2/3) ) === RR )

assert( class ( (2.) ^ (3/2) ) === RR )
assert( class ( (-2.) ^ (3/2) ) === CC )
assert( class ( (2.) ^ (-3/2) ) === RR )
assert( class ( (-2.) ^ (-3/2) ) === CC )

assert( class ( (2.) ^ (3.) ) === RR )
assert( class ( (-2.) ^ (3.) ) === CC )
assert( class ( (2.) ^ (-3.) ) === RR )
assert( class ( (-2.) ^ (-3.) ) === CC )

assert( class ( (2.) ^ (3.+ii) ) === CC )
assert( class ( (-2.) ^ (3.+ii) ) === CC )

assert( class ( (2.+ii) ^ (3.+ii) ) === CC )

assert( precision ( (2) ^ (3.p123) ) === 123 )
assert( precision ( (-2) ^ (3.p123) ) === 123 )
assert( precision ( (2) ^ (-3.p123) ) === 123 )
assert( precision ( (-2) ^ (-3.p123) ) === 123 )
assert( precision ( (2) ^ (3.p123+ii) ) === 123 )
assert( precision ( (-2) ^ (3.p123+ii) ) === 123 )
assert( precision ( (2/3) ^ (3.p123) ) === 123 )
assert( precision ( (-2/3) ^ (3.p123) ) === 123 )
assert( precision ( (2/3) ^ (-3.p123) ) === 123 )
assert( precision ( (-2/3) ^ (-3.p123) ) === 123 )
assert( precision ( (2/3) ^ (3.p123+ii) ) === 123 )
assert( precision ( (-2/3) ^ (3.p123+ii) ) === 123 )
assert( precision ( (2.p123) ^ (3) ) === 123 )
assert( precision ( (-2.p123) ^ (3) ) === 123 )
assert( precision ( (2.p123) ^ (-3) ) === 123 )
assert( precision ( (-2.p123) ^ (-3) ) === 123 )
assert( precision ( (2.p123) ^ (2/3) ) === 123 )
assert( precision ( (-2.p123) ^ (2/3) ) === 123 )
assert( precision ( (2.p123) ^ (-2/3) ) === 123 )
assert( precision ( (-2.p123) ^ (-2/3) ) === 123 )
assert( precision ( (2.p123) ^ (3/2) ) === 123 )
assert( precision ( (-2.p123) ^ (3/2) ) === 123 )
assert( precision ( (2.p123) ^ (-3/2) ) === 123 )
assert( precision ( (-2.p123) ^ (-3/2) ) === 123 )

assert isFinite ((2) ^ (2/3))
assert isFinite ((-2) ^ (2/3))
assert isFinite ((2) ^ (-2/3))
assert isFinite ((-2) ^ (-2/3))
assert isFinite ((2) ^ (3/2))
assert isFinite ((-2) ^ (3/2))
assert isFinite ((2) ^ (-3/2))
assert isFinite ((-2) ^ (-3/2))
assert isFinite ((2) ^ (3.))
assert isFinite ((-2) ^ (3.))
assert isFinite ((2) ^ (-3.))
assert isFinite ((-2) ^ (-3.))
assert isFinite ((2) ^ (3.+ii))
assert isFinite ((-2) ^ (3.+ii))
assert isFinite ((2/3) ^ (2/3))
assert isFinite ((-2/3) ^ (2/3))
assert isFinite ((2/3) ^ (-2/3))
assert isFinite ((-2/3) ^ (-2/3))
assert isFinite ((2/3) ^ (3/2))
assert isFinite ((-2/3) ^ (3/2))
assert isFinite ((2/3) ^ (-3/2))
assert isFinite ((-2/3) ^ (-3/2))
assert isFinite ((2/3) ^ (3.))
assert isFinite ((-2/3) ^ (3.))
assert isFinite ((2/3) ^ (-3.))
assert isFinite ((-2/3) ^ (-3.))
assert isFinite ((2/3) ^ (3.+ii))
assert isFinite ((-2/3) ^ (3.+ii))
assert isFinite ((2.) ^ (3))
assert isFinite ((-2.) ^ (3))
assert isFinite ((2.) ^ (-3))
assert isFinite ((-2.) ^ (-3))
assert isFinite ((2.) ^ (2/3))
assert isFinite ((-2.) ^ (2/3))
assert isFinite ((2.) ^ (-2/3))
assert isFinite ((-2.) ^ (-2/3))
assert isFinite ((2.) ^ (3/2))
assert isFinite ((-2.) ^ (3/2))
assert isFinite ((2.) ^ (-3/2))
assert isFinite ((-2.) ^ (-3/2))
assert isFinite ((2.) ^ (3.))
assert isFinite ((-2.) ^ (3.))
assert isFinite ((2.) ^ (-3.))
assert isFinite ((-2.) ^ (-3.))
assert isFinite ((2.) ^ (3.+ii))
assert isFinite ((-2.) ^ (3.+ii))
assert isFinite ((2.+ii) ^ (3.+ii))

assert isFinite (3)
assert not isInfinite (3)
assert isANumber (3)

assert isFinite (3/2)
assert not isInfinite (3/2)
assert isANumber (3/2)

assert isFinite (3.)
assert not isInfinite (3.)
assert isANumber (3.)

assert not isFinite (1/0.)
assert isInfinite (1/0.)
assert isANumber (1/0.)

assert not isFinite (1/0.-1/0.)
assert not isInfinite (1/0.-1/0.)
assert not isANumber (1/0.-1/0.)

assert not isFinite (1/0.+1/0.)
assert isInfinite (1/0.+1/0.)
assert isANumber (1/0.+1/0.)

assert not isFinite ((1/0.)*(1/0.))
assert isInfinite ((1/0.)*(1/0.))
assert isANumber ((1/0.)*(1/0.))

assert not isFinite ((1/0.)/(1/0.))
assert not isInfinite ((1/0.)/(1/0.))
assert not isANumber ((1/0.)/(1/0.))

assert isFinite toCC(3.,1.)
assert not isInfinite toCC(3.,1.)
assert isANumber toCC(3.,1.)

assert not isFinite toCC(1/0.,1.)
assert isInfinite toCC(1/0.,1.)
assert isANumber toCC(1/0.,1.)

assert not isFinite toCC(1/0.-1/0.,1.)
assert not isInfinite toCC(1/0.-1/0.,1.)
assert not isANumber toCC(1/0.-1/0.,1.)

assert isFinite toCC(1.,3.)
assert not isInfinite toCC(1.,3.)
assert isANumber toCC(1.,3.)

assert not isFinite toCC(1.,1/0.)
assert isInfinite toCC(1.,1/0.)
assert isANumber toCC(1.,1/0.)

assert not isFinite toCC(1.,1/0.-1/0.)
assert not isInfinite toCC(1.,1/0.-1/0.)
assert not isANumber toCC(1.,1/0.-1/0.)

assert not isFinite toCC(3.,2/0.)
assert isInfinite toCC(3.,2/0.)
assert isANumber toCC(3.,2/0.)

assert not isFinite toCC(1/0.,2/0.)
assert isInfinite toCC(1/0.,2/0.)
assert isANumber toCC(1/0.,2/0.)

assert not isFinite toCC(1/0.-1/0.,2/0.)
assert not isInfinite toCC(1/0.-1/0.,2/0.)
assert not isANumber toCC(1/0.-1/0.,2/0.)

assert not isFinite toCC(2/0.,3.)
assert isInfinite toCC(2/0.,3.)
assert isANumber toCC(2/0.,3.)

assert not isFinite toCC(2/0.,1/0.)
assert isInfinite toCC(2/0.,1/0.)
assert isANumber toCC(2/0.,1/0.)

assert not isFinite toCC(2/0.,1/0.-1/0.)
assert not isInfinite toCC(2/0.,1/0.-1/0.)
assert not isANumber toCC(2/0.,1/0.-1/0.)

assert not isFinite toCC(3.,2/0.-2/0.)
assert not isInfinite toCC(3.,2/0.-2/0.)
assert not isANumber toCC(3.,2/0.-2/0.)

assert not isFinite toCC(1/0.,2/0.-2/0.)
assert not isInfinite toCC(1/0.,2/0.-2/0.)
assert not isANumber toCC(1/0.,2/0.-2/0.)

assert not isFinite toCC(1/0.-1/0.,2/0.-2/0.)
assert not isInfinite toCC(1/0.-1/0.,2/0.-2/0.)
assert not isANumber toCC(1/0.-1/0.,2/0.-2/0.)

assert not isFinite toCC(2/0.-2/0.,3.)
assert not isInfinite toCC(2/0.-2/0.,3.)
assert not isANumber toCC(2/0.-2/0.,3.)

assert not isFinite toCC(2/0.-2/0.,1/0.)
assert not isInfinite toCC(2/0.-2/0.,1/0.)
assert not isANumber toCC(2/0.-2/0.,1/0.)

assert not isFinite toCC(2/0.-2/0.,1/0.-1/0.)
assert not isInfinite toCC(2/0.-2/0.,1/0.-1/0.)
assert not isANumber toCC(2/0.-2/0.,1/0.-1/0.)

inf = toCC(1/0.,1/0.)
assert ( isInfinite (inf*inf) )

z = 0.+0.*ii
assert ( isInfinite realPart (1/z) )
assert ( isInfinite imaginaryPart (1/z) )
assert ( isInfinite realPart (z^-1) )
assert ( isInfinite imaginaryPart (z^-1) )
assert ( isInfinite realPart (z^-2) )
assert ( isInfinite imaginaryPart (z^-2) )
assert ( isInfinite realPart (z^-3) )
assert ( isInfinite imaginaryPart (z^-3) )

assert ( zero (z^1) )
assert ( zero (z^2) )
assert ( zero (z^3) )

assert ( zero (z^-1^-1) )
assert ( zero (z^-2^-2) )
assert ( zero (z^-3^-3) )

assert ( zero ((1/z)^-1) )
assert ( zero ((1/z)^-2) )
assert ( zero ((1/z)^-3) )

assert ( zero (1/(1/z)) )
assert ( zero (1./(1./z)) )
assert ( zero (1/(1./z)) )
assert ( zero (1./(1/z)) )

assert ( zero ((1.+ii)/(1/z)) )
assert ( zero (1/((1.+ii)/z)) )
assert ( zero ((1.+ii)/((1.+ii)/z)) )

-- Tests of arithmetic operations with infinite numbers

assert ( infinity + infinity == infinity )
assert ( infinity - infinity === indeterminate )
assert ( infinity - -infinity == infinity )
assert ( infinity * infinity == infinity )
assert ( infinity * - infinity == - infinity )
assert ( infinity / infinity === indeterminate )

nan = 1./0. - 1./0.
assert ( infinity + 3 == infinity )
assert ( infinity + 3/2 == infinity )
assert ( infinity + 3. == infinity )
assert ( infinity + pi == infinity )
assert ( infinity + (5 + ii) == promote(1/0.,CC) )
assert ( infinity + 1./0. == 1/0. )
assert ( infinity + nan === indeterminate )
assert ( - infinity + 1./0. === indeterminate )

assert ( 3 - infinity == - infinity )
assert ( 2/3 - infinity == - infinity )
assert ( 2. - infinity == - infinity )
assert ( EulerConstant - infinity == - infinity )

assert ( 3 * infinity == infinity )
assert ( 0 * infinity === indeterminate )
assert ( 2/3 * - infinity == - infinity )
assert ( -3. * - infinity == 1/0. )
assert ( infinity * (1./0.) == 1/0. )
assert ( infinity * (-1./0.) == - 1/0. )
assert ( not isANumber(infinity * nan) )

assert ( 3 // infinity == 0 )
assert ( 3. / infinity == 0 )
assert ( (5 + ii) // infinity == 0 )
assert ( (1./0.) // infinity === indeterminate )
assert ( nan // infinity === indeterminate )

assert ( infinity // 3 == infinity )
assert ( infinity // - 3 == - infinity )
assert ( infinity // (1/2) == infinity )
assert ( - infinity // -3. == infinity )
assert ( infinity // 0 === indeterminate )

assert ( infinity ^ 1 == infinity )
assert ( infinity ^ 2 == infinity )
assert ( infinity ^ 3 == infinity )
assert ( infinity ^ (-1) == 0 )
assert ( (-infinity) ^ 1 == - infinity )
assert ( (-infinity) ^ 2 == infinity )
assert ( (-infinity) ^ 3 == - infinity )
assert ( (-infinity) ^ (-1) == 0 )
assert ( infinity ^ 0 === indeterminate )
assert ( infinity ^ (1/2) == infinity )
assert ( (-infinity) ^ (1/2) === indeterminate )
assert ( (-infinity) ^ (2/3) == infinity )
assert ( (-infinity) ^ (5/3) == - infinity )
assert ( infinity ^ 0.2 == infinity )
assert ( (-infinity) ^ 0.2 === indeterminate )
assert ( infinity ^ (-0.2) == 0 )
assert ( infinity ^ (1./0.) == infinity )
assert ( infinity ^ (-1./0.) == 0 )
assert ( infinity ^ nan === indeterminate )
assert ( (-infinity) ^ infinity === indeterminate )
assert ( (-infinity) ^ (-infinity) == 0 )
assert ( (-infinity) ^ (1./0.) === indeterminate )
assert ( (-infinity) ^ (-1./0.) === indeterminate )
assert ( (-infinity) ^ nan === indeterminate )

assert ( 1 ^ infinity == 1 )
assert ( 1 ^ - infinity == 1 )
assert ( 0 ^ infinity == 0 )
assert ( 0 ^ - infinity === indeterminate )
assert ( (-1) ^ infinity === indeterminate )
assert ( (-1) ^ - infinity === indeterminate )
assert ( 3 ^ infinity == infinity )
assert ( (-3) ^ infinity === indeterminate )
assert ( 3 ^ - infinity == 0 )
assert ( (-3) ^ - infinity === indeterminate )
assert ( (1/5) ^ infinity == 0 )
assert ( 0.2 ^ infinity == 0 )
assert ( (-1/5) ^ infinity === indeterminate )
assert ( (-0.2) ^ infinity === indeterminate )
assert ( (1/5) ^ - infinity == infinity )
assert ( 0.2 ^ - infinity == infinity )
assert ( (-1/5) ^ - infinity === indeterminate )
assert ( (-0.2) ^ - infinity === indeterminate )
assert ( (7/6) ^ infinity == infinity )
assert ( 1.3 ^ infinity == infinity )
assert ( (-7/6) ^ infinity === indeterminate )
assert ( (-1.3) ^ infinity === indeterminate )
assert ( (7/6) ^ - infinity == 0 )
assert ( 1.3 ^ - infinity == 0 )
assert ( (-7/6) ^ - infinity === indeterminate )
assert ( (-1.3) ^ - infinity === indeterminate )
assert ( (1/1) ^ infinity == 1 )
assert ( 1. ^ infinity == 1. )
assert ( nan ^ infinity === indeterminate )

assert ( size2 4. == 3 )
assert ( size2 3.999999 == 2 )
assert ( size2 (-4.) == 3 )
assert ( size2 (-3.999999) == 2 )
assert ( size2 0. < -2000 )
assert ( size2 (1/0.) > 2000 )
assert ( size2 (1/0.-1/0.) > 2000 )

assert ( isInfinite ( ii / 0. ) )
assert ( isInfinite ( ii * (1/0.) ) )
assert ( isInfinite ( ii * (1/(0.+0.*ii)) ) )
assert ( not isInfinite pi )
assert ( isInfinite infinity )

assert ( atan (1/0.) === pi/2 )
assert ( atan (1/(0.*ii)) == pi/2 )
assert ( instance(atan (1/(0.*ii)) , CC) )

assert small(1 - inverseErf erf 1)
assert small(1/2 - erf inverseErf(1/2))

assert small(Gamma(5, 0) - Gamma 5)
assert small(Gamma(1, 5) - exp(-5))
assert small(Gamma(1/2, 5) - sqrt pi * erfc sqrt 5)

assert small(Gamma(1/2, 5) / Gamma(1/2) - regularizedGamma(1/2, 5))
assert small(5 - inverseRegularizedGamma(1/2, regularizedGamma(1/2, 5)))
assert small(1/5 - regularizedGamma(1/2, inverseRegularizedGamma(1/2, 1/5)))

assert small(regularizedBeta(1/3, 4, 1) - 1/81)
assert small(regularizedBeta(1/3, 4, 5) -
    (regularizedBeta(1/3, 3, 5) - (1/3)^3 * (2/3)^5 / (3 * Beta(3, 5))))
assert small(1/3 - inverseRegularizedBeta(regularizedBeta(1/3, 4, 5), 4, 5))
assert small(1/3 - regularizedBeta(inverseRegularizedBeta(1/3, 4, 5), 4, 5))

-- I don't know whether we want to fix this:
-- assert ( 1/11 == toRR(1/11) )
-- assert ( 1/11 == 1p200/11 )

R = CC[x]
assert( size (2.1 + x) == 2 )
assert( size (2.1 - x) == 2 )
assert( size (2.1 * x) == 1 )
assert( size (pi + x) == 2 )
assert( size (pi * x) == 1 )
assert( size (ii + x) == 2 )
assert( size (ii - x) == 2 )
assert( size (ii * x) == 1 )
assert( size (1 + x) == 2 )
assert( size (1 - x) == 2 )
assert( size (1 * x) == 1 )

printingPrecision = 2
printingAccuracy = -1
assert( toString( 1.3333 ) == "1.3" )
assert( toString( -1.3333 ) == "-1.3" )
assert( toString( .13333 ) == ".13" )
assert( toString( -.13333 ) == "-.13" )
assert( toString( 0. ) == "0" )
assert( toString( -0. ) == "-0" )
assert( toString( 1/0. ) == "infinity" )
assert( toString( 1/-0. ) == "-infinity" )
assert( toString( 1/0.-1/0. ) == "NotANumber" )
assert( toString toCC( 1/-0., 0. ) == "infinity" )
assert( toString toCC( 1/0., 0. ) == "infinity" )
assert( toString toCC( 0., 1/-0. ) == "infinity" )
assert( toString toCC( 0., 1/0. ) == "infinity" )
assert( toString toCC( 0., 1/0.-1/0. ) == "NotANumber" )
assert( toString toCC( 4.,0. ) == "4" )
assert( toString toCC( 0.,4. ) == "4*ii" )
assert( net toCC( 0.,4. ) == "4*ii" )
assert( toString toCC( 0.,-4. ) == "-4*ii" )
assert( toString toCC( 4.,-0. ) == "4" )
assert( toString toCC(-0.,-0.) == "0" )
assert( toString toCC(-0.,0.) == "0" )
assert( toString toCC(0.,0.) == "0" )
assert( toString toCC(0.,-0.) == "0" )
assert( toString toCC(-0.,-2.) == "-2*ii" )
assert( toString toCC(-0.,2.) == "2*ii" )
assert( toString toCC(0.,2.) == "2*ii" )
assert( toString toCC(0.,-2.) == "-2*ii" )
assert( toString toCC(-2.,-0.) == "-2" )
assert( toString toCC(-2.,0.) == "-2" )
assert( toString toCC(2.,0.) == "2" )
assert( toString toCC(2.,-0.) == "2" )
assert( toString toCC(-5.,-2.) == "-5-2*ii" )
assert( toString toCC(-5.,2.) == "-5+2*ii" )
assert( toString toCC(5.,2.) == "5+2*ii" )
assert( toString toCC(5.,-2.) == "5-2*ii" )
assert( toString toCC(-5.,-1.) == "-5-ii" )
assert( toString toCC(-5.,1.) == "-5+ii" )
assert( toString toCC(5.,1.) == "5+ii" )
assert( toString toCC(5.,-1.) == "5-ii" )
assert( toString toCC( 8.88888, 0. ) == "8.9" )
assert( toString toCC( 8.88888, 0.001 ) == "8.9" )
assert( toString toCC( 8.88888, 0.01 ) == "8.9" )
assert( toString toCC( 8.88888, 0.1 ) == "8.9+.1*ii" )
assert( toString toCC( 8.88888, -0.1 ) == "8.9-.1*ii" )
assert( toString toCC( 0., 8.88888) == "8.9*ii" )
assert( toString toCC( 0.001, 8.88888 ) == "8.9*ii" )
assert( toString toCC( 0.01, 8.88888 ) == "8.9*ii" )
assert( toString toCC( 0.1, 8.88888 ) == ".1+8.9*ii" )
assert( toString toCC( -0.1, 8.88888 ) == "-.1+8.9*ii" )

-- Tests of comparisons among real numbers, complex numbers, NaNs, and infinite numbers

assert( 1/0. > 0. )
assert( (1/0. ? 0.) == symbol > )
assert( (ii/0. ? 0.) == symbol incomparable )
assert( (1/0. ? (1/0. - 1/0.)) == symbol incomparable )
assert( (1. ? (1/0. - 1/0.)) == symbol incomparable )
assert( ((1/0. - 1/0.) ? (1/0. - 1/0.)) == symbol incomparable )
assert( (1/0. ? 1.) == symbol > )
assert( (1/0. ? 2/0.) == symbol == )
assert( (1/0. + 1*ii ? 1.) == symbol incomparable )
assert( (ii/0. ? ii/0. - ii/0.) == symbol incomparable )
assert( (infinity ? 1/0. - 1/0.) == symbol incomparable )
assert( (infinity ? ii/0. - ii/0.) == symbol incomparable )
assert( ii/0. == 1 + ii/0. )
assert( ii/0. == 1/0. + ii*1 )
assert( ii/0. == 1/0. + ii/1. )
assert( ii/0. == -ii/0. )
assert( ii/0. == 1 - ii/0. )
assert( ii/0. - ii/0. != 1 + ii/0. )
assert( not isANumber ((1/0.-1/0.) + 1*ii) )
assert( not isANumber (1 + (ii/0.-ii/0.) ) )
assert( not isANumber ((1 + ii/0.) - ii/0. ) )


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test numbers.out"
-- End:

