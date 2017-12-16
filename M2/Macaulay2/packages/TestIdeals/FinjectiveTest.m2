TEST /// --Fedder's original F-injective not F-pure, deformation examples
    S = ZZ/2[x,y,z,u,v];
    I =  minors(2, matrix {{x^2, z, v}, {u, z, y^2}});
    J = I + ideal(x,y);
    assert(isFinjective(S/I));
    assert(isFinjective(S/J));
    assert(isFpure(S/J));
    assert(not isFpure(S/I));
///

TEST /// --cone over P1 times supersingular elliptic curve (non CM)
    S = ZZ/3[xs, ys, zs, xt, yt, zt];
    EP1 = ZZ/3[x,y,z,s,t]/ideal(x^3+y^2*z-x*z^2); --supersingular elliptic curve
    f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
    R = S/(ker f);
    assert(not isFinjective(R));
///

TEST /// --cone over P1 times ordinary elliptic curve (non CM)
    S = ZZ/3[xs, ys, zs, xt, yt, zt];
    EP1 = ZZ/3[x,y,z,s,t]/ideal(y^2*z-x^3+x*y*z); --ordinary elliptic curve
    f = map(EP1, S, {x*s, y*s, z*s, x*t, y*t, z*t});
    R = S/(ker f);
    assert( isFinjective(R) );
///

TEST /// --HSLGModule cone over ordinary elliptic curve
    R = ZZ/7[x,y,z]/ideal(x^3+y^3+z^3);
    HSLmod = HSLGModule(R);
    assert(HSLmod#0 == HSLmod#1);
///

TEST /// --the isLocal option
    R = ZZ/5[x,y,z]/ideal((x-2)^3 + y^3 + z^3); --supersingular
    assert( isFinjective(R, IsLocal=>true) );
    assert( not isFinjective(R) );
///

TEST /// --HSLGModule cone over supersingular elliptic curve
    R = ZZ/5[x,y,z]/ideal(x^3+y^3+z^3);
    HSLmod = HSLGModule(R);
    assert(not (HSLmod#0 == HSLmod#1));
///

TEST /// --HSLGModule cone over supersingular elliptic curve
    R = ZZ/7[x,y]
    HSLmod = HSLGModule(5/6, y^2-x^3);
    assert((HSLmod#0 == HSLmod#1));
///

TEST /// --isFinjective of a ring with no variables
    R = ZZ/17[];
    assert(isFinjective(R));
///

TEST /// --checking brute force F-injective vs canonicalStrategy
    R = ZZ/3[x,y,z]/ideal(y^2*z-x^3+x*y*z); --ordinary
    assert(isFinjective(R));
    assert(isFinjective(R, CanonicalStrategy=>null));
    S = ZZ/3[x,y,z]/ideal(x^3+y^2*z-x*z^2); --supersingular
    assert(not isFinjective(S));
    assert(not isFinjective(S, CanonicalStrategy=>null));
///
