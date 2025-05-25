TEST ///    
    R := QQ[x,y];
    S := subring {x^2, x*y, y^2};
    assert(isSubringElement(x^2, S))
    assert(not isSubringElement(x, S))
///

TEST ///
    R := QQ[x,y];
    S1 := subring {x^2, y^2};
    S2 := subring {x^2, x^4, y^2};
    assert(S1 == S2)
///

TEST ///
    R1 := QQ[x,y];
    S1 := subring {x^2, x*y, y^2};
    R2 := QQ[x,y,z];
    S2 := subring {x^2, x*y, y^2};
    assert(not S1 == S2)
///

TEST ///
    R := QQ[x,y];
    S := subring {x^2, x*y, y^2};
    assert(class(presentationRing S) === PolynomialRing)
    assert(numgens(presentationRing S) == 3)
///

TEST ///
    R := QQ[x,y];
    S := subring {x^2, x*y, y^2};
    f := presentationMap S;
    assert(class(f) === RingMap)
    assert(target f === R)
    assert(numgens source f == 3)
    assert(ambient S === R)
///
