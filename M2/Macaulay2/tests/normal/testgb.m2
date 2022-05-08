-- -*- coding: utf-8 -*-

-- Copyright 1995  Michael E. Stillman
--
-- Test some of the Gröbner basis/syzygy code 
--
-- 

strat = LongPolynomial;


-- Easy first case

    R = ZZ/101[a..d];
    m = matrix{{a*b-c^2, c*b}};
    assert(generators gb(m, Strategy=>strat) == matrix {{b*c, a*b-c^2, c^3}} )

    R = ZZ/101[a,b,c,d];
    m = matrix{{a*b - c^2, a*c - d^2, b*c - c*d}};
    assert(generators gb(m, Strategy=>strat) == matrix {{b*c-c*d, a*c-d^2, a*b-c^2, b*d^2-d^3, c^3-d^3, c^2*d^2-a*d^3, a^2*d^3-c*d^4}})
    assert(image m == image matrix ( {{b*c-c*d, a*c-d^2, a*b-c^2, b*d^2-d^3, c^3-d^3, c^2*d^2-a*d^3, a^2*d^3-c*d^4}}))

    R = ZZ/101[a,b,c,d, Degrees => {1,2,3,4}, MonomialOrder=>GRevLex=>{1,2,3,4}];
    m = matrix{{a^4*b - c^2, a*c - b^2, c^3 - a*d^2}};
    assert ( generators gb(m, Strategy => strat) == sort matrix {{b^2-a*c, a^4*b-c^2, a^5*c-b*c^2, c^3-a*d^2, a^6*d^2-a*b*c*d^2}} )

    R = ZZ/101[symbol r,symbol s,symbol t,symbol a..symbol f,
              Degrees=>{1,1,1,3,3,3,3,3,3},
	      MonomialOrder => Eliminate 3];
    m = matrix {{r^3, r^2*s, r^2*t, r*t^2, r*s*t, t^3}} - matrix {{a,b,c,d,e,f}};
    n = selectInSubring(1,generators gb(m, Strategy=>strat));
    assert (n == matrix "de-bf,d2-cf,cd-af,bd-ce,c2-ad,bc-ae,ce2-b2f,ae3-b3f" )

    -- Easy first case computing minimal generators
    R = ZZ/101[a..d];
    m = matrix{{a*b-c^2, b*c, c^3}};
    assert(mingens image m == matrix{{b*c, a*b-c^2}})

    -- 3 by 3 commuting matrices
    R = ZZ/101[vars(0..17)];
    m1 = genericMatrix(R,R_0,3,3);
    m2 = genericMatrix(R,R_9,3,3);
    I = flatten(m1*m2-m2*m1);
    ans1 = matrix {{g*l, d*l, g*k, d*k, g*j, d*j, c*j, b*j, h*l*n, h*l*m, 
		h*k*m, f*k*m, c*k*m, c*h*m, b*h*m, c*g*m, b*g*m, a*g*m, 
		c*e*m, a*c*k, b*g^2*o, b*g^2*n, b*f*g*n, c*d*g*n, b*c*g*n, c^2*d*h*n}};
    assert(image leadTerm generators gb(I, Strategy=>strat) == image ans1);
    sz = syz I;
    assert(degrees target sz == {{2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}});
    assert(degrees source sz == 
		{{2}, {3}, {3}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, 
		{4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}, 
		{4}, {4}, {4}, {4}, {4}, {4}, {4}, {4}});

    -- test Hilbert-driven Gröbner basis
    R = ZZ/101[vars(0..17)];
    m1 = genericMatrix(R,R_0,3,3);
    m2 = genericMatrix(R,R_9,3,3);
    I = flatten(m1*m2-m2*m1);
    J = flatten(m1*m2-m2*m1);
    h1 = poincare cokernel I;
    -- cache poincare
    poincare cokernel J = h1;
    g = generators gb(J, Strategy=>strat);
    assert(image g == image generators gb I)

    -- another Hilbert-driven test
    R = ZZ/101[a..f];
    h = poincare cokernel matrix{{a^2, b^2, c^2, d^2, e^2}};
    I = random(R^1, R^{-2,-2,-2,-2,-2});
    g = generators gb I;
    J = matrix entries I;
    -- cache poincare
    poincare cokernel J = h;
    assert(image g == image generators gb(J,Strategy=>strat))

    -- an elimination, first we compute using rlex, obtaining Hilbert function
    -- then we do the same computation with lex order.
    R = ZZ/101[r,s,t,a.. f,Degrees=>{1,1,1,2,2,2,2,2,2}];
    I = matrix{{r^2, r*s, r*t, s^2, s*t, t^2}} - genericMatrix(R,a,1,6);
    h1 = poincare cokernel I;
    R1 = ZZ/101[r,s,t,a..f,
               Degrees=>{1,1,1,2,2,2,2,2,2},
               MonomialOrder => Lex];
    J = substitute(I, R1);
    -- cache poincare
    poincare cokernel J = h1;
    g = gb(J, Strategy=>strat);
    assert(image generators gb I == image substitute(generators g, R))

    -- finding minimal generators
    R = ZZ/5[vars(0..4)];
    f = a*b^2 + c^3;
    g = (a+b+c)^3;
    m = matrix{{a*f + b*g, f, g}};
    assert( mingens image m == matrix{{f,g+2*f}})

    -- working over quotient rings
    R = ZZ/101[symbol a..symbol d]/(a^2+b^2+c^2+d^2);
    m1 = vars R;
    m2 = syz(m1,Strategy=>strat);
    m3 = syz(m2,Strategy=>strat);
    m4 = syz(m3,Strategy=>strat);
    assert(numgens source m2 == 7);
    assert(numgens source m3 == 8);
    assert(numgens source m4 == 8);
    assert(m1 * m2 == 0);
    assert(m2 * m3 == 0);
    assert(m3 * m4 == 0)

    -- second test over quotient ring
    A = ZZ/32003[symbol x, symbol y, symbol z, symbol t, Degrees => {2,1,1,1}];
    B = A/(x*y+z^3+z*t^2);
    I = symmetricPower(3,matrix{{y,z}});
    -- now compute E = Ext^1(I,B) 'by hand'
    m1 = syz(I,Strategy=>strat);
    m2 = syz(m1,Strategy=>strat);
    mm1 = transpose m1;
    mm2 = syz(transpose m2, Strategy=>strat);
    E = prune cokernel modulo(mm2, mm1);
    assert((E == cokernel map(B^{3,3}, , 
                   {{z, 0, y, 0, x, t^2}, {0, y, -z, t^2, 0, x}}))
           or
           (E == cokernel map(B^{3,3}, , 
                   {{0, y, -z, t^2, 0, x}, {z, 0, y, 0, x, t^2}})))


end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test testgb.out"
-- End:
