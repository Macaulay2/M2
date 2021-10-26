-- Copyright 1996 by Michael E. Stillman, David Eisenbud
-- based on David Eisenbud's
-- subring and push_forward1 scripts for Macaulay

-- Tests of the subring and pushForward code

-- pushForward1 has been removed, but we can simulate it:

pushforward1 = method(Options => options pushForward)
pushforward1(RingMap, Module) := opts -> (f,M) -> (
     S := source f;
     g := gens M;
     N := source g;
     assert( isFreeModule N );
     N':= S^(- degrees N);
     gens kernel map(M,N',f,id_N))

test1 = () -> (
    -- One of the simplest: the rational quartic curve
    R = ZZ/101[s,t];
    S = ZZ/101[a..d];
    f = map(R,S,matrix{{s^4,s^3*t,s*t^3,t^4}});
    time J0 = generators kernel f;
    time J2 = pushforward1(f,R^1);
    assert(image J0 == image J2);
    )

test1a = () -> (
    R = ZZ/101[s,t];
    S = ZZ/101[a..d];
    f = map(R,S,matrix{{s^4,s^3*t,s*t^3,t^4}});
    J0 = pushforward1(f,R^1,DegreeLimit=>5);
    )

test2 = () -> (
    -- image of an elliptic curve on the Veronese
    -- tests the quotient ring case
    R = ZZ/101[symbol x, symbol y, symbol z]/(y^2*z - x*(x+z)*(x-z));
    S = ZZ/101[symbol a..symbol f];
    F = map(R,S,symmetricPower(2,vars R));
    time J0 = generators kernel F;
    time J2 = pushforward1(F,R^1);
    assert(image J0 == image J2);
    )

test3 = () -> (
    -- The middle monomial ribbon of genus 5
    R = ZZ/101[symbol s, symbol t, symbol y]/(y^2);
    S = ZZ/101[vars(0..4)];
    f = map(R,S,matrix{{t^4, t^3*s, t^2*s^2, t*s^3-t^3*y, s^4-2*s*t^2*y}});
    time J0 = generators kernel f;
    time J2 = pushforward1(f,R^1);
    assert(image J0 == image J2);
    )

test4 = () -> (
    -- a simple example where one of the image values is 0
    R = ZZ/101[symbol a, symbol b];
    f = map(R,R,map(R^1, R^{-1,-1}, {{a,0}}));
    time J0 = generators kernel f;
    time J2 = pushforward1(f,R^1);
    assert(image J0 == image J2);
    -- assert(image J1 == image J2);
    )

test5 = () -> (
    -- a simple example where the entries are not linear independent
    R = ZZ/101[symbol a, symbol b, symbol c];
    f = map(R,R,matrix{{a,b,a-b}});
    time J0 = generators kernel f;
    time J2 = pushforward1(f,R^1);
    assert(image J0 == image J2);
    )
    
test6 = () -> (
    -- a simple example which caused a bug in Macaulay classic
    R = ZZ/101[symbol a]/(a^4);
    S = ZZ/101[symbol a];
    use R;
    f = map(R,S,matrix{{a}});
    time J0 = generators kernel f;
    time J2 = pushforward1(f,R^1);
    assert(image J0 == image J2);
    )

test7 = () -> (
    -- sheaves on P1 x P1
    x = symbol x;
    y = symbol y;
    z = symbol z;
    R = ZZ/32003[x_0, x_1, y_0, y_1];
    S = ZZ/32003[z_0 .. z_3];
    f = map(R,S,matrix{{x_0,x_1}} ** matrix{{y_0,y_1}});
    I = matrix{{x_0,y_0}};   -- a point
    J = matrix{{x_0,x_1}};   -- the empty set
    K = matrix{{x_0*y_0^2 + x_1*y_1^2}};  -- a twisted cubic (i.e. of type (1,2))
    time J0 = pushforward1(f,cokernel I);
    time J1 = pushforward1(f,cokernel J);
    time J2 = pushforward1(f,cokernel K);
    assert(image J0 == image matrix{{z_0,z_1,z_2}});
    assert(image J1 == image matrix{{z_0,z_1,z_2,z_3}});
    assert(image J2 == image matrix{{z_1*z_2-z_0*z_3, z_0*z_2+z_3^2, z_0^2+z_1*z_3}});
    )

test8 = () -> (
    -- A hard example
    S = ZZ/32003[vars(0..3)];
    R = ZZ/32003[vars(0..4)];
    I = matrix{{a^5 + b^5 + c^5 + d^5 + e^5 - 5*a*b*c*d*e,
               e*a^3*b + a*b^3*c + b*c^3*d + c*d^3*e + d*e^3*a,
               e^2*a*b^2 + a^2*b*c^2 + b^2*c*d^2 + c^2*d*e^2 + d^2*e*a^2}};
    f = map(R,S,matrix{{a+b,2*a+c,3*a+d,4*a+e}});
    -- gbTrace = 3;
    time J2 = pushforward1(f, cokernel I,MonomialOrder=>ProductOrder);
    )

testx = () -> (
    -- test the newCoordinateSystem routine, and give an example as to how it gets used
    R = ZZ/101[vars(0..7)];
    y = matrix{{a+b+c}};
    J = matrix{{a-c+d, b-e}};
    fg1 = newCoordinateSystem(R, y);
    f1 = fg1#0; 
    g1 = fg1#1;
    J1 = f1 J;
    J2 = g1 J1;
    assert(J == J2);

    fg2 = newCoordinateSystem(R, J);
    f2 = fg2#0;
    g2 = fg2#1;
    J3 = f2 y;
    J4 = g2 J3;
    assert(J4 == y);
    )

testx1 = () -> (
    -- test the newCoordinateSystem routine, and give an example as to how it gets used
    -- Start with a non-saturated ideal, in a ring using an order other than
    -- reverse lex.
    -- see also 'sat1'.
    S = ZZ/101[vars(0..3)];
    R = ZZ/101[vars(0..3),MonomialOrder=>Eliminate 2];
    I = intersect(
	 image matrix{{a+b, c^3, a*b^2}}, 
	 image matrix{{b^2 - c^2, c^3 - d^3}});
    fg = newCoordinateSystem(S, matrix{{a+b}});
    Fto = fg#0;
    Fback = fg#1;
    J1 = mingens image Fback (divideByVariable(generators gb Fto generators I,S_3))#0;
    assert(image J1 == image matrix{{b^2 - c^2, c^3 - d^3}});
    )

test1()
test2()
test3()
--test4()
test5()
test6()
test7()
test8()
testx()
testx1()

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test testsubring.out"
-- End:
