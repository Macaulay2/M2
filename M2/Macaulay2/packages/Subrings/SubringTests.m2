TEST ///    
    R := QQ[x,y];
    S := subring {x^2, x*y, y^2};
    assert(isSubringElement(x^2, S));
    assert(not isSubringElement(x, S));
///

TEST ///
    R := QQ[x,y];
    S1 := subring {x^2, y^2};
    S2 := subring {x^2, x^4, y^2};
    assert(S1 == S2);
///

TEST ///
    R1 := QQ[x,y];
    S1 := subring {x^2, x*y, y^2};
    R2 := QQ[x,y,z];
    S2 := subring {x^2, x*y, y^2};
    assert(not S1 == S2);
///

TEST ///
    R := QQ[x,y];
    S := subring {x^2, x*y, y^2};
    assert(class(presentationRing S) === PolynomialRing);
    assert(numgens(presentationRing S) == 3);
///

TEST ///
    R := QQ[x,y];
    S := subring {x^2, x*y, y^2};
    f := presentationMap S;
    assert(class(f) === RingMap);
    assert(target f === R);
    assert(numgens source f == 3);
    assert(ambient S === R);
    assert(numgens presentationIdeal S == 1);
    assert(ring subringGenerators S === R);
    assert(instance(toQuotientRing S, QuotientRing));
///


TEST /// -- check towers of polynomial rings
    R := QQ[x][y];
    S := subring {x+y, x*y};
    assert(isSubringElement(x^3+y^3, S));
    assert(not isSubringElement(x^3+y^3+x, S));
    assert(numgens flattenedRing S == 2);
    assert(ring subringGenerators S === flattenedRing S);
    assert(zero presentationIdeal S);
    assert(instance(toQuotientRing S, PolynomialRing));
    assert(ring gens S === R);
///

TEST /// -- assignment of GeneratorSymbol in subring
    R := QQ[x,y];
    S := subring({x^2, x*y, y^2}, GeneratorSymbol => getSymbol "z");
    u := (presentationRing S)_0;
    assert((baseName u)#0 === getSymbol "z");
///
